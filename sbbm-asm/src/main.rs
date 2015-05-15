extern crate docopt;
extern crate rustc_serialize;
extern crate sbbm_asm;

use sbbm_asm::assembler::{Assembler, AssembledItem};
use sbbm_asm::commands::{
    Command, Selector, SelectorName, SelectorTeam, Target,
    objectives, players, teams};
use docopt::Docopt;
use sbbm_asm::layout::{Layout, LayoutMotion, LinearMotion, PackedMotion};
use sbbm_asm::lexer::Lexer;
use sbbm_asm::nbt::{Nbt, NbtCompound};
use sbbm_asm::parser::Parser;
use sbbm_asm::types::{Extent, Pos3, Vec3};

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
        let mut parser = Parser::new(Lexer::new(&input[..], &args.arg_source[..]));
        let stmts = parser.parse_program();

        // FIXME: Check for warnings/errors before starting to place blocks.
        let assembler = Assembler::new(stmts.into_iter());
        let origin = Vec3::new(args.arg_x, args.arg_y, args.arg_z);
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

        if let Some(init) = args.flag_init {
            let mut f = File::create(Path::new(&init[..])).unwrap();
            init_computer(&mut f, origin).unwrap();
        }

        if let Some(destroy) = args.flag_destroy {
            let mut f = File::create(Path::new(&destroy[..])).unwrap();
            destroy_computer(&mut f, extent);
        }

        if let Some(boot) = args.flag_boot {
            let mut f = File::create(Path::new(&boot[..])).unwrap();
            boot_computer(&mut f, &layout);
        }
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

fn init_computer(conn: &mut CommandWrite, origin: Vec3) -> io::Result<()> {
    let comp_tgt = Target::Sel(Selector {
        name: Some(SelectorName::Is("computer".to_string())),
        ..Selector::entity()
    });

    try!(conn.write_cmd(&Command::Kill(comp_tgt.clone())));
    let pos = origin.as_abs();
    let mut data_tag = NbtCompound::new();
    data_tag.insert("CustomName".to_string(), Nbt::String("computer".to_string()));
    try!(conn.write_cmd(&Command::Summon(
        "ArmorStand".to_string(), Some(pos),
        Some(Nbt::Compound(data_tag)))));

    try!(init_registers(conn, comp_tgt.clone()));
    try!(init_bitwise(conn, origin));

    Ok(())
}

fn init_registers(conn: &mut CommandWrite, comp_tgt: Target) -> io::Result<()> {
    // General registers
    for i in (0..32) {
        let obj = format!("r{}", i);

        try!(conn.write_cmd(&objectives::remove(obj.clone())));
        try!(conn.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(conn.write_cmd(&players::set(comp_tgt.clone(), obj, 0, None)));
    }

    // Predicate registers
    for i in (0..8) {
        let obj = format!("p{}", i);

        try!(conn.write_cmd(&objectives::remove(obj.clone())));
        try!(conn.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(conn.write_cmd(&players::set(comp_tgt.clone(), obj, 0, None)));
    }

    // Temp registers (implementation details)
    for i in (0..4) {
        let obj = format!("t{}", i);

        try!(conn.write_cmd(&objectives::remove(obj.clone())));
        try!(conn.write_cmd(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(conn.write_cmd(&players::set(comp_tgt.clone(), obj, 0, None)));
    }

    // Special registers (implementation details)
    for name in ["ZERO", "TWO", "MIN", "TEST"].iter() {
        let obj = name.to_string();
        try!(conn.write_cmd(&objectives::remove(obj.clone())));
        try!(conn.write_cmd(&objectives::add(obj, "dummy".to_string(), None)));
    }

    try!(conn.write_cmd(&players::set(comp_tgt.clone(), "ZERO".to_string(), 0, None)));
    try!(conn.write_cmd(&players::set(comp_tgt.clone(), "TWO".to_string(), 2, None)));
    try!(conn.write_cmd(&players::set(comp_tgt.clone(), "MIN".to_string(), std::i32::MIN, None)));
    Ok(())
}

fn init_bitwise(conn: &mut CommandWrite, origin: Vec3) -> io::Result<()> {
    for obj in ["BitComponent", "BitNumber", "t0", "t1", "t2"].iter() {
        try!(conn.write_cmd(&objectives::remove(obj.to_string())));
        try!(conn.write_cmd(&objectives::add(obj.to_string(), "dummy".to_string(), None)));
    }

    let bit_team = "Shifters".to_string();

    // Bitwise entities
    let bit_team_target = Target::Sel(Selector {
        team: Some(SelectorTeam::On(bit_team.clone())),
        ..Selector::entity()
    });
    try!(conn.write_cmd(&Command::Kill(bit_team_target)));
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
        try!(conn.write_cmd(&Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag)))));

        try!(conn.write_cmd(&players::set(target.clone(), "BitNumber".to_string(), i, None)));
        try!(conn.write_cmd(&players::set(target, "BitComponent".to_string(), 1 << i, None)));
    }

    try!(conn.write_cmd(&teams::remove(bit_team.clone())));
    try!(conn.write_cmd(&teams::add(bit_team.clone(), None)));
    try!(conn.write_cmd(&teams::join(bit_team, shifters)));

    Ok(())
}

fn destroy_computer(w: &mut CommandWrite, extent: Extent) {
    // FIXME: Really reverse all the activity in init_computer.

    if let Extent::MinMax(min, max) = extent {
        let cmd = Command::Fill(
            min.as_abs(), max.as_abs(),
            "minecraft:air".to_string(), None, None, None);
        w.write_cmd(&cmd).unwrap();
    }
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
