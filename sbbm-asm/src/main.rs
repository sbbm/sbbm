#![feature(core, plugin)]
#![plugin(regex_macros)]

extern crate docopt;
extern crate regex;
extern crate rcon;
extern crate rustc_serialize;

mod assembler;
mod ast;
mod core;
mod layout;
mod lexer;
mod nbt;
mod parser;

use assembler::Assembler;
use core::Vec3;
use docopt::Docopt;
use layout::LinearLayout;
use lexer::Lexer;
use nbt::Nbt;
use parser::Parser;
use rcon::Connection;
use std::fs::File;
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
        let layout = LinearLayout::new(Vec3::new(27, 57, 0), assembler);

        let mut conn = Connection::connect(
            &args.arg_server[..], &args.arg_password[..]).unwrap();

        conn.cmd("fill 27 56 0 80 60 64 minecraft:air").unwrap();
        for (pos, block) in layout {
            let cmd = format!(
                "setblock {} {} {} {} 0 replace {}",
                pos.x, pos.y, pos.z, block.id, Nbt::Compound(block.nbt));
            //println!("cmd: {}", cmd);
            conn.cmd(&cmd[..]).unwrap();
        }
    }
}
