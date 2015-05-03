#![feature(core, plugin)]
#![plugin(regex_macros)]

extern crate docopt;
extern crate regex;
extern crate rcon;
extern crate rustc_serialize;

mod assembler;
mod ast;
mod commands;
mod core;
mod layout;
mod lexer;
mod nbt;
mod parser;

use assembler::Assembler;
use commands::Command;
use core::{Pos3, Vec3};
use docopt::Docopt;
use layout::{Layout, LinearMotion};
use lexer::Lexer;
use nbt::{Nbt, NbtCompound};
use parser::Parser;
use rcon::Connection;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;

static USAGE : &'static str = "
usage: sbbm-asm <source> <server> <password>
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_source: String,
    arg_server: String,
    arg_password: String,
}

#[cfg(not(test))]
fn main() {
    let args : Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let mut file = File::open(Path::new(&args.arg_source[..])).unwrap();
    let mut input = String::new();
    if let Ok(_) = file.read_to_string(&mut input) {
        let mut parser = Parser::new(Lexer::new(&input[..]));
        let stmts = parser.parse_program();

        // FIXME: Check for warnings/errors before starting to place blocks.
        let assembler = Assembler::new(stmts.into_iter());
        let motion = LinearMotion::new(Vec3::new(27, 57, 0));
        //let motion = PackedMotion::new(Vec3::new(27, 57, 0));
        let layout = Layout::new(motion, assembler);

        let mut conn = Connection::connect(
            &args.arg_server[..], &args.arg_password[..]).unwrap();

        conn.cmd("fill 26 56 0 60 67 32 minecraft:air").unwrap();
        init_computer(&mut conn);
        for (pos, block) in layout {
            let cmd = format!(
                "setblock {} {} {} {} 0 replace {}",
                pos.x, pos.y, pos.z, block.id, Nbt::Compound(block.nbt));
            //println!("cmd: {}", cmd);
            conn.cmd(&cmd[..]).unwrap();
        }
    }
}

trait ConnEx {
    fn exec(&mut self, cmd: &Command) -> io::Result<String>;
}

impl ConnEx for Connection {
    fn exec(&mut self, cmd: &Command) -> io::Result<String> {
        // FIXME: Minecraft is having trouble keeping up.
        std::thread::sleep_ms(10);
        self.cmd(&format!("{}", cmd)[..])
    }
}

fn init_computer(conn: &mut Connection) {
    use commands::ScoreboardCmd::*;
    use commands::TeamCmd::*;
    use commands::PlayerCmd::Set;

    for i in (0..32) {
        let name = format!("bit_{}", i);
        let sel = format!("@e[name={}]", name);
        conn.exec(&Command::Kill(sel.clone())).unwrap();

        let pos = Pos3::abs(26, 56, i);
        let mut data_tag = NbtCompound::new();
        data_tag.insert("CustomName".to_string(), Nbt::String(name.clone()));
        conn.exec(&Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag)))).unwrap();

        conn.exec(&Command::Scoreboard(Teams(Join(
            "Shifters".to_string(), vec!(sel.clone()))))).unwrap();

        conn.exec(&Command::Scoreboard(Players(Set(
            sel, "BitComponent".to_string(), 1 << i, None))));
    }
}
