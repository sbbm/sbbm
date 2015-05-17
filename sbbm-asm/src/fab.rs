use assembler::AssembledItem;
use assembler::AssembledItem::*;
use commands::{self, Command};
use hw::MemoryRegion;
use nbt::{Nbt, NbtCompound};
use types::Block;

use std::vec::IntoIter;

pub fn cmd_block(cmd: Command, track_output: bool) -> Block {
    let mut nbt = NbtCompound::new();
    let cmd_str = commands::escape(&cmd.to_string()[..]);
    nbt.insert("Command".to_string(), Nbt::String(cmd_str));
    let nbt_track_output = Nbt::Byte(if track_output { 1 } else { 0 });
    nbt.insert("TrackOutput".to_string(), nbt_track_output);
    Block {
        id: "minecraft:command_block".to_string(),
        data: 0,
        nbt: nbt,
    }
}

pub fn mem_label(region: &MemoryRegion) -> String {
    format!("mem_{}_{}", region.start, region.size)
}

pub fn make_mem_ctrl(region: &MemoryRegion) -> IntoIter<AssembledItem> {
    let mut items = vec!();

    items.push(Label(mem_label(region)));
    let cmd = cmd_block(Command::Say("mmmmemory!".to_string()), false);
    items.push(Complete(cmd));
    items.push(Terminal);
    items.into_iter()
}
