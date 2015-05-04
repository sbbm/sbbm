#![feature(core, plugin)]
#![plugin(regex_macros)]

extern crate docopt;
extern crate regex;
extern crate rustc_serialize;

mod assembler;
mod ast;
mod commands;
mod types;
mod layout;
mod lexer;
mod nbt;
mod parser;

use assembler::Assembler;
use commands::{Command, Selector};
use docopt::Docopt;
use layout::{Layout, LinearMotion};
use lexer::Lexer;
use nbt::{Nbt, NbtCompound};
use parser::Parser;
use types::{Pos3, Vec3};

use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

static USAGE : &'static str = "
usage: sbbm-asm [--output OUTPUT] <source>

Options:
    -o, --output OUTPUT    Output file.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_source: String,
    flag_output: Option<String>,
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
        let mut parser = Parser::new(Lexer::new(&input[..]));
        let stmts = parser.parse_program();

        // FIXME: Check for warnings/errors before starting to place blocks.
        let assembler = Assembler::new(stmts.into_iter());
        let motion = LinearMotion::new(Vec3::new(27, 57, 0));
        //let motion = PackedMotion::new(Vec3::new(27, 57, 0));
        let layout = Layout::new(motion, assembler);

        output.exec(&Command::Fill(
            Pos3::abs(26, 56, 0), Pos3::abs(60, 67, 32),
            "minecraft:air".to_string(), None, None, None)).unwrap();
        init_computer(&mut output);
        for (pos, block) in layout {
            output.exec(&Command::SetBlock(
                pos.as_abs(), block.id, None, None,
                Some(Nbt::Compound(block.nbt)))).unwrap();
        }
    }
}

trait MinecraftConn {
    fn exec(&mut self, cmd: &Command) -> io::Result<()>;
}

impl<W : Write> MinecraftConn for W {
    fn exec(&mut self, cmd: &Command) -> io::Result<()> {
        write!(self, "{}\n", cmd)
    }
}

fn init_computer(conn: &mut MinecraftConn) {
    let comp_sel = "@e[name=computer]".to_string();

    conn.exec(&Command::Kill(comp_sel.clone())).unwrap();
    let pos = Pos3::abs(25, 56, 0);
    let mut data_tag = NbtCompound::new();
    data_tag.insert("CustomName".to_string(), Nbt::String("computer".to_string()));
    conn.exec(&Command::Summon(
        "ArmorStand".to_string(), Some(pos),
        Some(Nbt::Compound(data_tag)))).unwrap();

    init_registers(conn, comp_sel.clone());
    init_bitwise(conn);
}

fn init_registers(conn: &mut MinecraftConn, comp_sel: Selector) {
    use commands::ScoreboardCmd::*;
    use commands::PlayerCmd::Set;
    use commands::ObjCmd;

    // General registers
    for i in (0..32) {
        let obj = format!("r{}", i);

        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Remove(
            obj.clone())))).unwrap();
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Add(
            obj.clone(), "dummy".to_string(), None)))).unwrap();
        conn.exec(&Command::Scoreboard(Players(Set(
            comp_sel.clone(), obj, 0, None)))).unwrap();
    }

    // Predicate registers
    for i in (0..8) {
        let obj = format!("p{}", i);

        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Remove(
            obj.clone())))).unwrap();
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Add(
            obj.clone(), "dummy".to_string(), None)))).unwrap();
        conn.exec(&Command::Scoreboard(Players(Set(
            comp_sel.clone(), obj, 0, None)))).unwrap();
    }

    // Special registers (implementation details)
    for name in ["ZERO", "TWO", "MIN", "TEST"].iter() {
        let obj = name.to_string();
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Remove(
            obj.clone())))).unwrap();
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Add(
            obj, "dummy".to_string(), None)))).unwrap();
    }

    conn.exec(&Command::Scoreboard(Players(Set(
        comp_sel.clone(), "ZERO".to_string(), 0, None)))).unwrap();
    conn.exec(&Command::Scoreboard(Players(Set(
        comp_sel.clone(), "TWO".to_string(), 2, None)))).unwrap();
    conn.exec(&Command::Scoreboard(Players(Set(
        comp_sel.clone(), "MIN".to_string(), std::i32::MIN, None)))).unwrap();

}

fn init_bitwise(conn: &mut MinecraftConn) {
    use commands::ScoreboardCmd::*;
    use commands::ObjCmd;
    use commands::PlayerCmd::Set;
    use commands::TeamCmd::*;

    for obj in ["BitComponent", "BitNumber"].iter() {
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Remove(
            obj.to_string())))).unwrap();
        conn.exec(&Command::Scoreboard(Objectives(ObjCmd::Add(
            obj.to_string(), "dummy".to_string(), None)))).unwrap();
    }

    let bit_team = "Shifters".to_string();

    // Bitwise entities
    conn.exec(&Command::Kill(format!("@e[team={}]", bit_team))).unwrap();
    let mut shifters = vec!();
    for i in (0..32) {
        let name = format!("bit_{}", i);
        let sel = format!("@e[name={}]", name);
        shifters.push(sel.clone());

        let pos = Pos3::abs(26, 56, i);
        let mut data_tag = NbtCompound::new();
        data_tag.insert("CustomName".to_string(), Nbt::String(name.clone()));
        conn.exec(&Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag)))).unwrap();

        conn.exec(&Command::Scoreboard(Players(Set(
            sel.clone(), "BitNumber".to_string(), i, None)))).unwrap();

        conn.exec(&Command::Scoreboard(Players(Set(
            sel, "BitComponent".to_string(), 1 << i, None)))).unwrap();
    }
    conn.exec(&Command::Scoreboard(Teams(Remove(
        bit_team.clone())))).unwrap();
    conn.exec(&Command::Scoreboard(Teams(Add(
        bit_team.clone(), None)))).unwrap();
    conn.exec(&Command::Scoreboard(Teams(Join(
        bit_team, shifters)))).unwrap();
}
