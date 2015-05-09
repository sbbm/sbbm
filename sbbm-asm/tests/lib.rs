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
static SET_REGEX: Regex = regex!(
    r"Set score of (\w+) for player .+ to (-?\d+)");

macro_rules! lock_server {
    () => { let _g = SERVER_MUTEX.lock().unwrap(); }
}

fn server_path() -> PathBuf {
    let mut p = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    p.push("..");
    p.push("server");
    p
}

fn write(cmd: &Command) -> io::Result<()> {
    let input_path = {
        let mut p = server_path();
        p.push("input");
        p
    };

    let mut f = OpenOptions::new()
        .write(true)
        .append(true)
        .open(input_path)
        .unwrap();
    write!(f, "{}\n", cmd)
}

fn exec(cmd: &Command) -> io::Result<String> {
    // FIXME: Eliminate all unwrap to prevent poisoning the mutex.
    let (_, output) = capture_until(|_| true, || {
        write(cmd).unwrap();
    });
    Ok(output)
}

fn get(target: &Target, obj: &str) -> io::Result<i32> {
    let resp = try!(exec(
        &players::add(target.clone(), obj.to_string(), 0, None)));

    if let Some(cap) = SET_REGEX.captures(&resp[..]) {
        // TODO: Verify that the objectives match.
        // FIXME: Real error handling.
        Ok(cap.at(2).unwrap().parse().unwrap())
    } else {
        panic!("ugh error handling is hard");
    }
}

fn capture_until<F, P, T>(p: P, f: F) -> (T, String)
    where F : Fn() -> T,
          P : Fn(&str) -> bool
{
    // FIXME: Eliminate all panics, return some kind of Result<>

    // Begin capturing
    let capture_path = {
        let mut p = server_path();
        p.push("capture");
        p
    };

    File::create(capture_path.clone()).unwrap();

    let res = f();

    // Capture output and wait for p to return true.
    let output_path = {
        let mut p = server_path();
        p.push("output");
        p
    };

    // FIXME: handle errors, good god man.
    let out = File::open(output_path).unwrap();
    let mut out = BufReader::new(out);
    // FIXME: eeeerrroors
    let mut captured = String::new();
    loop {
        let start = captured.len();
        out.read_line(&mut captured).unwrap();
        if p(&captured[start..]) { break; }
    }

    fs::remove_file(capture_path).unwrap();
    (res, captured)
}

fn capture<F, T>(f: F) -> (T, String) where F : Fn() -> T {
    let marker = "54799be5-7239-4e00-bd9f-095ae6ed58a3";
    let (result, mut output) = capture_until(|s| s.contains(marker), || {
        let result = f();
        write(&Command::Say(marker.to_string())).unwrap();
        result
    });

    // Remove the marker line from output.
    let mut count = 0;
    match output.rfind(|c| if c == '\n' { count += 1; count > 1 } else { false }) {
        Some(index) => output.truncate(index + 1),
        None => output.truncate(0),
    }

    (result, output)
}

fn run_asm(input: &str) {
    // FIXME: Eliminate all unwrap to prevent poisoning the mutex.

    let marker = "6ee5dd4a-ea5c-476d-bcab-4c2a912ce2ed";
    let (dirty_extent, _) = capture_until(|s| s.contains(marker), || {
        let mut marked = input.to_string();
        marked.push_str("\n\traw say ");
        marked.push_str(marker);
        let mut parser = Parser::new(Lexer::new(&marked[..]));
        let assembler = Assembler::new(parser.parse_program().into_iter());
        let motion = LinearMotion::new(Vec3::new(0, 57, 0));
        let mut layout = Layout::new(motion, assembler);

        let mut dirty_extent = Extent::Empty;
        for (pos, block) in &mut layout {
            dirty_extent.add(pos);
            write(&Command::SetBlock(
                pos.as_abs(), block.id, None, None,
                Some(Nbt::Compound(block.nbt)))).unwrap();
        }

        if let Some(Extent::MinMax(min, max)) = layout.get_power_extent("main") {
            dirty_extent.add(min);
            dirty_extent.add(max);
            write(&Command::Fill(
                min.as_abs(), max.as_abs(), "minecraft:redstone_block".to_string(),
                None, None, None)).unwrap();
        }

        dirty_extent
    });

    capture(|| {
        if let Extent::MinMax(min, max) = dirty_extent {
            write(&Command::Fill(
                min.as_abs(), max.as_abs(), "minecraft:air".to_string(),
                None, None, None)).unwrap();
        }
    });
}

fn computer_target() -> Target {
    Target::Sel(Selector {
        name: Some(SelectorName::Is("computer".to_string())),
        ..Selector::entity()
    })
}

#[test]
fn test_constant_regs() {
    lock_server!();

    let target = computer_target();
    assert_eq!(i32::MIN, get(&target, "MIN").unwrap());
    assert_eq!(2, get(&target, "TWO").unwrap());
    assert_eq!(0, get(&target, "ZERO").unwrap());
}

#[test]
fn test_add() {
    lock_server!();

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
    lock_server!();

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
    lock_server!();

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
    lock_server!();

    run_asm("
main:
mov r0, #100
mov r1, #37
sdiv r0, r1");

    let target = computer_target();
    assert_eq!(100 / 37, get(&target, "r0").unwrap());
}

#[test]
fn test_udiv() {
    lock_server!();

    let target = computer_target();
    let values = [
        i32::MIN, -1234568, -1234567, -33, -32, -3, -2, -1,
        1, 2, 3, 32, 33, 1234567, 1234568, i32::MAX];

    for left in values.iter() {
        for right in values.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
udiv r0, r1", left, right)[..]);

            assert_eq!(
                ((*left as u32) / (*right as u32)) as i32,
                get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_srem() {
    lock_server!();

    run_asm("
main:
mov r0, #100
mov r1, #37
srem r0, r1");

    let target = computer_target();
    assert_eq!(100 % 37, get(&target, "r0").unwrap());
}

#[test]
fn test_and() {
    lock_server!();

    let target = computer_target();
    let values = [
        i32::MIN, -1234568, -1234567, -1,
        0, 1, 1234567, 1234568, i32::MAX];

    for left in values.iter() {
        for right in values.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
and r0, r1", left, right)[..]);

            assert_eq!(left & right, get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_orr() {
    lock_server!();

    let target = computer_target();
    let values = [
        i32::MIN, -1234568, -1234567, -1,
        0, 1, 1234567, 1234568, i32::MAX];

    for left in values.iter() {
        for right in values.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
orr r0, r1", left, right)[..]);

            assert_eq!(left | right, get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_eor() {
    lock_server!();

    let target = computer_target();
    let values = [
        i32::MIN, -1234568, -1234567, -1,
        0, 1, 1234567, 1234568, i32::MAX];

    for left in values.iter() {
        for right in values.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
eor r0, r1", left, right)[..]);

            assert_eq!(left ^ right, get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_asr() {
    lock_server!();

    let values = [
        i32::MIN, -1234568, -1234567, -3, -2, -1,
        0, 1, 2, 3, 1234567, 1234568, i32::MAX];
    let amounts = [0, 1, 2, 30, 31];

    let target = computer_target();
    for value in values.iter() {
        for amount in amounts.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
asr r0, r1", value, amount)[..]);

            assert_eq!(value >> amount, get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_lsr() {
    lock_server!();

    let values = [
        i32::MIN, -1234568, -1234567, -3, -2, -1,
        0, 1, 2, 3, 1234567, 1234568, i32::MAX];
    let amounts = [0, 1, 2, 30, 31];

    let target = computer_target();
    for value in values.iter() {
        for amount in amounts.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
lsr r0, r1", value, amount)[..]);

            assert_eq!(
                ((*value as u32) >> amount) as i32,
                get(&target, "r0").unwrap());
        }
    }
}

#[test]
fn test_lsl() {
    lock_server!();

    let values = [
        i32::MIN, -1234568, -1234567, -3, -2, -1,
        0, 1, 2, 3, 1234567, 1234568, i32::MAX];
    let amounts = [0, 1, 2, 30, 31];

    let target = computer_target();
    for value in values.iter() {
        for amount in amounts.iter() {
            run_asm(&format!("
main:
mov r0, #{}
mov r1, #{}
lsl r0, r1", value, amount)[..]);

            assert_eq!(value << amount, get(&target, "r0").unwrap());
        }
    }
}
