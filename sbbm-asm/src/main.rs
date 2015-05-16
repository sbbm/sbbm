extern crate docopt;
extern crate rustc_serialize;
extern crate sbbm_asm;

use sbbm_asm::assembler::{Assembler, AssembledItem};
use sbbm_asm::commands::{
    Command, Selector, SelectorName, SelectorTeam, Target,
    objectives, players, teams};
use sbbm_asm::hw::{Computer, MemoryRegion};
use docopt::Docopt;
use sbbm_asm::layout::{Layout, LayoutMotion, LinearMotion, PackedMotion};
use sbbm_asm::lexer::Lexer;
use sbbm_asm::nbt::{Nbt, NbtCompound};
use sbbm_asm::parser::Parser;
use sbbm_asm::types::{Extent, Pos3, Vec3};

use std::i32;
use std::ops::DerefMut;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

static USAGE : &'static str = "
usage: sbbm-asm [-l LAYOUT] [-k INIT] [-b BOOT] [-d DESTROY] [-o OUTPUT] <x> <y> <z> <source>

Options:
    -o, --output OUTPUT    Output file.
    -l, --layout LAYOUT    Layout kind (packed or linear).
    -k, --init INIT        A filename that will be used to write out the
                           commands needed to initialize the circuit.  (Creates
                           the necessary entities and objectives, and performs
                           other one-time work.)
    -b, --boot BOOT        A filename that will be used to write out the
                           commands needed to boot up the assembled circuit.
    -d, --destroy DESTROY  A filename that will be used to write out a list of
                           commands that destroy the blocks and entities created
                           during assembly and initialization.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_x: i32,
    arg_y: i32,
    arg_z: i32,
    arg_source: String,
    flag_output: Option<String>,
    flag_layout: Option<LayoutKind>,
    flag_init: Option<String>,
    flag_boot: Option<String>,
    flag_destroy: Option<String>,
}

#[derive(RustcDecodable, Debug)]
enum LayoutKind {
    Linear,
    Packed,
}

#[cfg(not(test))]
fn main() {
    let args : Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let mut output : Box<Write> = if let Some(outfile) = args.flag_output {
        Box::new(File::create(Path::new(&outfile[..])).unwrap())
    } else {
        Box::new(std::io::stdout())
    };

    let mut file = File::open(Path::new(&args.arg_source[..])).unwrap();
    let mut input = String::new();
    if let Ok(_) = file.read_to_string(&mut input) {
        let origin = Vec3::new(args.arg_x, args.arg_y, args.arg_z);
        let computer = Computer {
            memory: vec!(
                MemoryRegion {
                    start: 0x10,
                    size: 0x8000,
                    origin: Vec3::new(origin.x - 1, origin.y, origin.z),
                    growth: Vec3::new(-1, 1, 1),
                })
        };

        let mut parser = Parser::new(Lexer::new(&input[..], &args.arg_source[..]));
        let stmts = parser.parse_program();

        // FIXME: Check for warnings/errors before starting to place blocks.
        let assembler = Assembler::new(&computer, stmts.into_iter());
        let motion : Box<LayoutMotion> = match args.flag_layout {
            Some(LayoutKind::Linear) => Box::new(LinearMotion::new(origin)),
            Some(LayoutKind::Packed) | None => Box::new(PackedMotion::new(origin)),
        };
        let mut layout = Layout::new(motion, assembler);

        let mut extent = Extent::Empty;
        for (pos, block) in &mut layout {
            extent.add(pos);
            output.write_cmd(&Command::SetBlock(
                pos.as_abs(), block.id, None, None,
                Some(Nbt::Compound(block.nbt)))).unwrap();
        }

        if args.flag_init.is_some() || args.flag_destroy.is_some() {
            let mut init = optional_out(args.flag_init);
            let mut destroy = optional_out(args.flag_destroy);
            write_init_destroy(
                init.deref_mut(), destroy.deref_mut(), origin, extent).unwrap();
        }

        if let Some(boot) = args.flag_boot {
            let mut f = File::create(Path::new(&boot[..])).unwrap();
            boot_computer(&mut f, &layout);
        }
    }
}

fn optional_out(path: Option<String>) -> Box<CommandWrite> {
    if let Some(path) = path {
        Box::new(File::create(Path::new(&path[..])).unwrap())
    } else {
        Box::new(io::sink())
    }
}

trait CommandWrite {
    fn write_cmd(&mut self, cmd: &Command) -> io::Result<()>;
}

impl<W : Write> CommandWrite for W {
    fn write_cmd(&mut self, cmd: &Command) -> io::Result<()> {
        write!(self, "{}\n", cmd)
    }
}

fn write_init_destroy(
    init: &mut CommandWrite, destroy: &mut CommandWrite,
    origin: Vec3, extent: Extent) -> io::Result<()>
{
    let comp_tgt = Target::Sel(Selector {
        name: Some(SelectorName::Is("computer".to_string())),
        ..Selector::entity()
    });

    let pos = origin.as_abs();
    let mut data_tag = NbtCompound::new();
    data_tag.insert("CustomName".to_string(), Nbt::String("computer".to_string()));
    data_tag.insert("NoGravity".to_string(), Nbt::Byte(1));
    try!(init.write_cmd(&Command::Summon(
        "ArmorStand".to_string(), Some(pos),
        Some(Nbt::Compound(data_tag)))));
    try!(destroy.write_cmd(&Command::Kill(comp_tgt.clone())));

    try!(init_destroy_regs(init, destroy, comp_tgt.clone()));
    try!(init_destroy_bitwise(init, destroy, origin));

    if let Extent::MinMax(min, max) = extent {
        let cmd = Command::Fill(
            min.as_abs(), max.as_abs(),
            "minecraft:air".to_string(), None, None, None);
        destroy.write_cmd(&cmd).unwrap();
    }

    Ok(())
}

fn init_destroy_regs(
    init: &mut CommandWrite, destroy: &mut CommandWrite, comp_tgt: Target)
    -> io::Result<()>
{
    // General registers
    for i in (0..32) {
        let obj = format!("r{}", i);

        try!(init.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(init.write_cmd(&players::set(comp_tgt.clone(), obj.clone(), 0, None)));

        try!(destroy.write_cmd(&objectives::remove(obj)));
    }

    // Predicate registers
    for i in (0..8) {
        let obj = format!("p{}", i);

        try!(init.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(init.write_cmd(&players::set(comp_tgt.clone(), obj.clone(), 0, None)));

        try!(destroy.write_cmd(&objectives::remove(obj)));
    }

    // Temp registers (implementation details)
    for i in (0..4) {
        let obj = format!("t{}", i);

        try!(init.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(init.write_cmd(&players::set(comp_tgt.clone(), obj.clone(), 0, None)));

        try!(destroy.write_cmd(&objectives::remove(obj)));
    }

    let special_regs = [
        ("ZERO", 0),
        ("TWO", 2),
        ("MIN", i32::MIN)];

    // Special registers (implementation details)
    for &(name, value) in special_regs.iter() {
        let obj = name.to_string();
        try!(init.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(init.write_cmd(&players::set(comp_tgt.clone(), obj.clone(), value, None)));

        try!(destroy.write_cmd(&objectives::remove(obj)));
    }

    Ok(())
}

fn init_destroy_bitwise(
    init: &mut CommandWrite, destroy: &mut CommandWrite, origin: Vec3)
    -> io::Result<()>
{
    for obj in ["BitComponent", "BitNumber"].iter() {
        try!(init.write_cmd(&objectives::add(obj.to_string(), "dummy".to_string(), None)));

        try!(destroy.write_cmd(&objectives::remove(obj.to_string())));
    }

    let bit_team = "Shifters".to_string();

    // Bitwise entities
    let mut shifters = vec!();
    for i in (0..32) {
        let name = format!("bit_{}", i);
        let target = Target::Sel(Selector {
            name: Some(SelectorName::Is(name.clone())),
            ..Selector::entity()
        });
        shifters.push(target.clone());

        let pos = Pos3::abs(origin.x, origin.y, origin.z + i);
        let mut data_tag = NbtCompound::new();
        data_tag.insert("CustomName".to_string(), Nbt::String(name));
        data_tag.insert("NoGravity".to_string(), Nbt::Byte(1));
        try!(init.write_cmd(&Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag)))));

        try!(init.write_cmd(&players::set(target.clone(), "BitNumber".to_string(), i, None)));
        try!(init.write_cmd(&players::set(target, "BitComponent".to_string(), 1 << i, None)));
    }

    try!(init.write_cmd(&teams::add(bit_team.clone(), None)));
    try!(init.write_cmd(&teams::join(bit_team.clone(), shifters)));

    let bit_team_target = Target::Sel(Selector {
        team: Some(SelectorTeam::On(bit_team.clone())),
        ..Selector::entity()
    });
    try!(destroy.write_cmd(&Command::Kill(bit_team_target)));
    try!(destroy.write_cmd(&teams::remove(bit_team)));

    Ok(())
}

fn boot_computer<Source>(w: &mut CommandWrite, layout: &Layout<Source>)
    where Source : Iterator<Item=AssembledItem>
{
    if let Some(Extent::MinMax(min, max)) = layout.get_power_extent("main") {
        let cmd = Command::Fill(
            min.as_abs(), max.as_abs(),
            "minecraft:redstone_block".to_string(), None, None, None);
        w.write_cmd(&cmd).unwrap();
    }
}
