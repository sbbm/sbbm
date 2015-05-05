// WHEN(rust-1.0): Remove std_misc (it is here for StaticMutex and MUTEX_INIT)
#![feature(plugin, std_misc)]
#![plugin(regex_macros)]

extern crate sbbm_asm;
extern crate regex;

use regex::Regex;
use sbbm_asm::assembler::Assembler;
use sbbm_asm::commands::{
    Command, Selector, SelectorName, Target, players};
use sbbm_asm::layout::{Layout, LinearMotion};
use sbbm_asm::lexer::Lexer;
use sbbm_asm::nbt::Nbt;
use sbbm_asm::parser::Parser;
use sbbm_asm::types::{Extent, Vec3};

use std::env;
use std::i32;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::fs::{self, File, OpenOptions};
use std::path::PathBuf;
use std::sync::{StaticMutex, MUTEX_INIT};

static SERVER_MUTEX: StaticMutex = MUTEX_INIT;
static CIRCUIT_MUTEX: StaticMutex = MUTEX_INIT;

// NOTE: This requires /gamerule logAdminCommands false
// Probably some other things are also required.  I need a server init script.
fn exec(cmd: &Command) -> io::Result<String> {
    // FIXME: Eliminate all unwrap to prevent poisoning the mutex.
    let _g = SERVER_MUTEX.lock().unwrap();

    // TODO: verify that server/input exists
    let cargo_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let server_path = {
        let mut p = cargo_path.clone();
        p.push("..");
        p.push("server");
        p
    };
    let capture_path = {
        let mut p = server_path.clone();
        p.push("capture");
        p
    };

    File::create(capture_path.clone()).unwrap();

    let input_path = {
        let mut p = server_path.clone();
        p.push("input");
        p
    };

    let output_path = {
        let mut p = server_path.clone();
        p.push("output");
        p
    };

    let mut f = OpenOptions::new().write(true).append(true).open(input_path).unwrap();
    write!(f, "{}\n", cmd).unwrap();
    //f.sync_all().unwrap();

    // FIXME: handle errors, good god man.
    let out = File::open(output_path).unwrap();
    let mut out = BufReader::new(out);
    // FIXME: eeeerrroors
    let mut result = String::new();
    out.read_line(&mut result).unwrap();

    fs::remove_file(capture_path).unwrap();
    Ok(result)
}

static SET_REGEX : Regex = regex!(
    r"Set score of (\w+) for player .+ to (-?\d+)");

fn get(target: &Target, obj: &str) -> io::Result<i32> {
    let resp = try!(exec(&players::add(target.clone(), obj.to_string(), 0, None)));

    if let Some(cap) = SET_REGEX.captures(&resp[..]) {
        // TODO: Verify that the objectives match.
        // FIXME: Real error handling.
        Ok(cap.at(2).unwrap().parse().unwrap())
    } else {
        panic!("ugh error handling is hard");
    }
}

fn run_asm(input: &str) {
    let _g = CIRCUIT_MUTEX.lock().unwrap();

    // FIXME: Eliminate all unwrap to prevent poisoning the mutex.

    let mut parser = Parser::new(Lexer::new(input));
    let assembler = Assembler::new(parser.parse_program().into_iter());
    let motion = LinearMotion::new(Vec3::new(0, 57, 0));
    let mut layout = Layout::new(motion, assembler);

    let mut dirty_extent = Extent::Empty;
    for (pos, block) in &mut layout {
        dirty_extent.add(pos);
        exec(&Command::SetBlock(
            pos.as_abs(), block.id, None, None,
            Some(Nbt::Compound(block.nbt)))).unwrap();
    }

    if let Some(Extent::MinMax(min, max)) = layout.get_power_extent("main") {
        dirty_extent.add(min);
        dirty_extent.add(max);
        exec(&Command::Fill(
            min.as_abs(), max.as_abs(), "minecraft:redstone_block".to_string(),
            None, None, None)).unwrap();
    }

    if let Extent::MinMax(min, max) = dirty_extent {
        exec(&Command::Fill(
            min.as_abs(), max.as_abs(), "minecraft:air".to_string(),
            None, None, None)).unwrap();
    }
}

fn computer_target() -> Target {
    Target::Sel(Selector {
        name: Some(SelectorName::Is("computer".to_string())),
        ..Selector::entity()
    })
}

#[test]
fn test_constant_regs() {
    let target = computer_target();
    assert_eq!(i32::MIN, get(&target, "MIN").unwrap());
    assert_eq!(2, get(&target, "TWO").unwrap());
    assert_eq!(0, get(&target, "ZERO").unwrap());
}

#[test]
fn test_add() {
    run_asm("
main:
mov r0, #100
mov r1, #37
add r0, r1");

    let target = computer_target();
    assert_eq!(100 + 37, get(&target, "r0").unwrap());
}

#[test]
fn test_sub() {
    run_asm("
main:
mov r0, #100
mov r1, #37
sub r0, r1");

    let target = computer_target();
    assert_eq!(100 - 37, get(&target, "r0").unwrap());
}

#[test]
fn test_mul() {
    run_asm("
main:
mov r0, #100
mov r1, #37
mul r0, r1");

    let target = computer_target();
    assert_eq!(100 * 37, get(&target, "r0").unwrap());
}


#[test]
fn test_sdiv() {
    run_asm("
main:
mov r0, #100
mov r1, #37
sdiv r0, r1");

    let target = computer_target();
    assert_eq!(100 / 37, get(&target, "r0").unwrap());
}

#[test]
fn test_srem() {
    run_asm("
main:
mov r0, #100
mov r1, #37
srem r0, r1");

    let target = computer_target();
    assert_eq!(100 % 37, get(&target, "r0").unwrap());
}
