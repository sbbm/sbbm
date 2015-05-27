// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

use regex::Regex;
use sbbm_asm::assembler::Assembler;
use sbbm_asm::commands::{Command, Target, IntoTarget, players};
use sbbm_asm::fab;
use sbbm_asm::hw::{Computer, MemoryRegion, MemoryStride};
use sbbm_asm::layout::{Layout, LinearMotion};
use sbbm_asm::lexer::Lexer;

use sbbm_asm::nbt::Nbt;
use sbbm_asm::parser::Parser;
use sbbm_asm::types::{Extent, Vec3};

use std::env;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::mem;
use std::path::PathBuf;
use std::rt::at_exit;
use std::sync::{MutexGuard, Once, StaticMutex, MUTEX_INIT, ONCE_INIT};

const ORIGIN: Vec3 = Vec3 { x: 0, y: 56, z: 0 };
static mut COMPUTER: *const Computer = 0 as *const Computer;
static COMPUTER_INIT: Once = ONCE_INIT;
static SERVER_MUTEX: StaticMutex = MUTEX_INIT;
static SET_REGEX: Regex = regex!(
    r"Set score of (\w+) for player .+ to (-?\d+)");

fn server_path() -> PathBuf {
    let mut path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("..");
    path.push("server");
    path
}

fn input_path() -> PathBuf {
    let mut p = server_path();
    p.push("input");
    p
}

fn output_path() -> PathBuf {
    let mut p = server_path();
    p.push("output");
    p
}

fn capture_path() -> PathBuf {
    let mut p = server_path();
    p.push("capture");
    p
}

pub struct Server {
    _guard: MutexGuard<'static, ()>,
}

impl Server {
    pub fn new() -> Server {
        let guard = SERVER_MUTEX.lock().unwrap();

        COMPUTER_INIT.call_once(|| {
            init_computer();
            at_exit(destroy_computer).unwrap();
        });

        let server = Server {
            _guard: guard,
        };

        // Wait for any noise to die down.
        server.capture(|| {});
        server
    }

    pub fn write(&self, cmd: &Command) -> io::Result<()> {
        let mut f = OpenOptions::new()
            .write(true)
            .append(true)
            .open(input_path())
            .unwrap();
        write!(f, "{}\n", cmd)
    }

    pub fn exec(&self, cmd: &Command) -> io::Result<String> {
        // FIXME: Eliminate all unwrap to prevent poisoning the mutex.
        let (_, output) = self.capture_until(|_| true, || {
            self.write(cmd).unwrap();
        });
        Ok(output)
    }

    pub fn get(&self, target: &Target, obj: &str) -> io::Result<i32> {
        let resp = try!(self.exec(
            &players::add(target.clone(), obj.to_string(), 0, None)));

        if let Some(cap) = SET_REGEX.captures(&resp[..]) {
            // TODO: Verify that the objectives match.
            // FIXME: Real error handling.
            Ok(cap.at(2).unwrap().parse().unwrap())
        } else {
            panic!("ugh error handling is hard");
        }
    }

    pub fn get_computer(&self, obj: &str) -> io::Result<i32> {
        self.get(&computer().selector().into_target(), obj)
    }

    pub fn capture_until<F, P, T>(&self, p: P, f: F) -> (T, String)
        where F : Fn() -> T,
            P : Fn(&str) -> bool
    {
        // FIXME: Eliminate all panics, return some kind of Result<>

        File::create(capture_path()).unwrap();

        let res = f();

        // FIXME: handle errors, good god man.
        let out = File::open(output_path()).unwrap();
        let mut out = BufReader::new(out);
        // FIXME: eeeerrroors
        let mut captured = String::new();
        loop {
            let start = captured.len();
            out.read_line(&mut captured).unwrap();
            if p(&captured[start..]) { break; }
        }

        fs::remove_file(capture_path()).unwrap();
        (res, captured)
    }

    pub fn capture<F, T>(&self, f: F) -> (T, String) where F : Fn() -> T {
        let marker = "54799be5-7239-4e00-bd9f-095ae6ed58a3";
        let (result, mut output) = self.capture_until(|s| s.contains(marker), || {
            let result = f();
            self.write(&Command::Say(marker.to_string())).unwrap();
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

    pub fn run_asm(&self, input: &str) {
        // FIXME: Eliminate all unwrap to prevent poisoning the mutex.

        let marker = "6ee5dd4a-ea5c-476d-bcab-4c2a912ce2ed";
        let (dirty_extent, _) = self.capture_until(|s| s.contains(marker), || {
            let mut marked = input.to_string();
            marked.push_str("\n\traw say ");
            marked.push_str(marker);

            let mut parser = Parser::new(Lexer::mem(&marked[..]));
            let mut assembler = Assembler::new(
                computer(), parser.parse_program().into_iter());
            assembler.set_track_output(true);

            let mem_controllers = {
                let mut c = vec!();
                for region in computer().memory.iter() {
                    c.extend(fab::make_mem_ctrl(region));
                }
                c };

            let motion = Box::new(LinearMotion::new(ORIGIN));
            let mut layout = Layout::new(motion, assembler.chain(mem_controllers));

            let mut dirty_extent = Extent::Empty;
            for (pos, block) in &mut layout {
                dirty_extent.add(pos);
                self.write(&Command::SetBlock(
                    pos.as_abs(), block.id, None, None,
                    Some(Nbt::Compound(block.nbt)))).unwrap();
            }

            if let Some(Extent::MinMax(min, max)) = layout.get_power_extent("main") {
                dirty_extent.add(min);
                dirty_extent.add(max);
                self.write(&Command::Fill(
                    min.as_abs(), max.as_abs(), "minecraft:redstone_block".to_string(),
                    None, None, None)).unwrap();
            }

            dirty_extent
        });

        self.capture(|| {
            if let Extent::MinMax(min, max) = dirty_extent {
                self.write(&Command::Fill(
                    min.as_abs(), max.as_abs(), "minecraft:air".to_string(),
                    None, None, None)).unwrap();
            }
        });
    }
}

fn computer() -> &'static Computer {
    unsafe { mem::transmute(COMPUTER) }
}

fn init_computer() {
    unsafe {
        COMPUTER = mem::transmute(Box::new(Computer {
            name: "computer".to_string(),
            origin: ORIGIN,
            memory: vec![
                MemoryRegion {
                    start: 0x10,
                    size: 0x100,
                    origin: Vec3::new(ORIGIN.x - 1, ORIGIN.y, ORIGIN.z),
                    growth: Vec3::new(-1, 1, 1),
                    stride: MemoryStride::XY(8, 8),
                }]
        }))
    }

    let mut f = OpenOptions::new()
        .write(true)
        .append(true)
        .open(input_path())
        .unwrap();
    computer().write_init_script(&mut f).unwrap();
}

fn destroy_computer() {
    let mut f = OpenOptions::new()
        .write(true)
        .append(true)
        .create(true)
        .open(input_path())
        .unwrap();
    computer().write_destroy_script(&mut f).unwrap();
}
