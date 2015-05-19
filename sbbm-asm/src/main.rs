extern crate docopt;
extern crate rustc_serialize;
extern crate sbbm_asm;

use docopt::Docopt;
use sbbm_asm::assembler::{Assembler, AssembledItem};
use sbbm_asm::commands::{self, Command};
use sbbm_asm::fab;
use sbbm_asm::hw::{Computer, MemoryRegion, MemoryStride};
use sbbm_asm::layout::{Layout, LayoutMotion, LinearMotion, PackedMotion};
use sbbm_asm::lexer::Lexer;
use sbbm_asm::nbt::Nbt;
use sbbm_asm::parser::Parser;
use sbbm_asm::types::{Extent, Vec3};

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
    -t, --track-output     When present, command blocks are generated with
                           TrackOutput enabled, so that the result of previous
                           executions can be viewed in-game.  (Mainly useful for
                           debugging.)
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
    flag_track_output: bool,
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
            name: "computer".to_string(),
            origin: origin,
            memory: vec![
                MemoryRegion {
                    start: 0x10,
                    size: 0x8000,
                    origin: Vec3::new(origin.x - 1, 0, origin.z),
                    growth: Vec3::new(-1, 1, 1),
                    stride: MemoryStride::XY(32, 256)
                }]
        };

        let mut parser = Parser::new(Lexer::new(&input[..], &args.arg_source[..]));
        let stmts = parser.parse_program();

        // FIXME: Check for warnings/errors before starting to place blocks.
        let mut assembler = Assembler::new(&computer, stmts.into_iter());
        assembler.set_track_output(args.flag_track_output);
        let motion : Box<LayoutMotion> = match args.flag_layout {
            Some(LayoutKind::Linear) => Box::new(LinearMotion::new(origin)),
            Some(LayoutKind::Packed) | None => Box::new(PackedMotion::new(origin)),
        };
        let mem_controllers = {
            let mut c = vec!();
            for region in computer.memory.iter() {
                c.extend(fab::make_mem_ctrl(region));
            }
            c };
        let mut layout = Layout::new(motion, assembler.chain(mem_controllers));

        let mut extent = Extent::Empty;
        for (pos, block) in &mut layout {
            extent.add(pos);
            write!(output, "{}\n", Command::SetBlock(
                pos.as_abs(), block.id, None, None,
                Some(Nbt::Compound(block.nbt)))).unwrap();
        }

        if let Some(init) = args.flag_init {
            let mut f = File::create(Path::new(&init[..])).unwrap();
            computer.write_init_script(&mut f).unwrap();
        }

        if let Some(destroy) = args.flag_destroy {
            let mut f = File::create(Path::new(&destroy[..])).unwrap();
            computer.write_destroy_script(&mut f).unwrap();
            for cmd in commands::safe_fill(
                extent, "minecraft:air".to_string(), None, None, None)
            {
                write!(f, "{}\n", cmd).unwrap();
            }
        }

        if let Some(boot) = args.flag_boot {
            let mut f = File::create(Path::new(&boot[..])).unwrap();
            boot_computer(&mut f, &layout).unwrap()
        }
    }

}

fn boot_computer<Source>(w: &mut Write, layout: &Layout<Source>) -> io::Result<()>
    where Source : Iterator<Item=AssembledItem>
{
    if let Some(Extent::MinMax(min, max)) = layout.get_power_extent("main") {
        let cmd = Command::Fill(
            min.as_abs(), max.as_abs(),
            "minecraft:redstone_block".to_string(), None, None, None);
        try!(write!(w, "{}\n", cmd));
    }
    Ok(())
}
