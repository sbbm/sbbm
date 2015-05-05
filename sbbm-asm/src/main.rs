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
use commands::{
    Command, Selector, SelectorName, SelectorTeam, Target,
    objectives, players, teams};
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
        init_computer(&mut output).unwrap();
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

fn init_computer(conn: &mut MinecraftConn) -> io::Result<()> {
    let comp_tgt = Target::Sel(Selector {
        name: Some(SelectorName::Is("computer".to_string())),
        ..Selector::entity()
    });

    try!(conn.exec(&Command::Kill(comp_tgt.clone())));
    let pos = Pos3::abs(25, 56, 0);
    let mut data_tag = NbtCompound::new();
    data_tag.insert("CustomName".to_string(), Nbt::String("computer".to_string()));
    try!(conn.exec(&Command::Summon(
        "ArmorStand".to_string(), Some(pos),
        Some(Nbt::Compound(data_tag)))));

    try!(init_registers(conn, comp_tgt.clone()));
    try!(init_bitwise(conn));

    Ok(())
}

fn init_registers(conn: &mut MinecraftConn, comp_tgt: Target) -> io::Result<()> {
    // General registers
    for i in (0..32) {
        let obj = format!("r{}", i);

        try!(conn.exec(&objectives::remove(obj.clone())));
        try!(conn.exec(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(conn.exec(&players::set(comp_tgt.clone(), obj, 0, None)));
    }

    // Predicate registers
    for i in (0..8) {
        let obj = format!("p{}", i);

        try!(conn.exec(&objectives::remove(obj.clone())));
        try!(conn.exec(&objectives::add(obj.clone(), "dummy".to_string(), None)));
        try!(conn.exec(&players::set(comp_tgt.clone(), obj, 0, None)));
    }

    // Special registers (implementation details)
    for name in ["ZERO", "TWO", "MIN", "TEST"].iter() {
        let obj = name.to_string();
        try!(conn.exec(&objectives::remove(obj.clone())));
        try!(conn.exec(&objectives::add(obj, "dummy".to_string(), None)));
    }

    try!(conn.exec(&players::set(comp_tgt.clone(), "ZERO".to_string(), 0, None)));
    try!(conn.exec(&players::set(comp_tgt.clone(), "TWO".to_string(), 2, None)));
    try!(conn.exec(&players::set(comp_tgt.clone(), "MIN".to_string(), std::i32::MIN, None)));
    Ok(())
}

fn init_bitwise(conn: &mut MinecraftConn) -> io::Result<()> {
    for obj in ["BitComponent", "BitNumber", "BitTmp1", "BitTmp2"].iter() {
        try!(conn.exec(&objectives::remove(obj.to_string())));
        try!(conn.exec(&objectives::add(obj.to_string(), "dummy".to_string(), None)));
    }

    let bit_team = "Shifters".to_string();

    // Bitwise entities
    let bit_team_target = Target::Sel(Selector {
        team: Some(SelectorTeam::On(bit_team.clone())),
        ..Selector::entity()
    });
    try!(conn.exec(&Command::Kill(bit_team_target)));
    let mut shifters = vec!();
    for i in (0..32) {
        let name = format!("bit_{}", i);
        let target = Target::Sel(Selector {
            name: Some(SelectorName::Is(name.clone())),
            ..Selector::entity()
        });
        shifters.push(target.clone());

        let pos = Pos3::abs(26, 56, i);
        let mut data_tag = NbtCompound::new();
        data_tag.insert("CustomName".to_string(), Nbt::String(name));
        try!(conn.exec(&Command::Summon(
            "ArmorStand".to_string(), Some(pos),
            Some(Nbt::Compound(data_tag)))));

        try!(conn.exec(&players::set(target.clone(), "BitNumber".to_string(), i, None)));
        try!(conn.exec(&players::set(target, "BitComponent".to_string(), 1 << i, None)));
    }

    try!(conn.exec(&teams::remove(bit_team.clone())));
    try!(conn.exec(&teams::add(bit_team.clone(), None)));
    try!(conn.exec(&teams::join(bit_team, shifters)));

    Ok(())
}
