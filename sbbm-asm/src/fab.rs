use assembler::AssembledItem;
use assembler::AssembledItem::*;
use commands::{
    self, players,
    Command, IntoTarget, Selector, SelectorName, SelectorTeam, ToTarget};
use hw::MemoryRegion;
use nbt::{Nbt, NbtCompound};
use types::{Block, Extent, Interval, Pos3};

use std::collections::HashMap;
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

pub fn power_off(label: String, track_output: bool) -> AssembledItem {
    Pending(label, Box::new(move |extent| {
        match extent {
            Extent::Empty => {
                panic!("oh no!");
            }
            Extent::MinMax(min, max) => {
                cmd_block(Command::Fill(
                    min.as_abs(), max.as_abs(), "minecraft:obsidian".to_string(),
                    None, None, None), track_output)
            }
        }
    }))
}

pub fn mem_label(region: &MemoryRegion) -> String {
    mem_name(region)
}

pub fn mem_name(region: &MemoryRegion) -> String {
    format!("mem_{:08x}", region.start)
}

pub fn mem_selector(region: &MemoryRegion) -> Selector {
    Selector {
        name: Some(SelectorName::Is(mem_name(region))),
        ..Selector::entity()
    }
}

fn mem_op_selector(region: &MemoryRegion, op: i32) -> Selector {
    Selector {
        scores: {
            let mut s = HashMap::new();
            s.insert("MemOp".to_string(), Interval::Bounded(op, op));
            s },
        ..mem_selector(region)
    }
}

pub struct MemComponent {
    pub name: String,
    pub sel: Selector,
    pub home: Pos3,
    pub mask: i32,
}

pub fn mem_components(region: &MemoryRegion) -> Vec<MemComponent> {
    let mut components = vec![];
    for comp in (0..8) {
        let name = format!("{}_{}", mem_name(region), comp);
        components.push(MemComponent {
            name: name.clone(),
            sel: Selector {
                name: Some(SelectorName::Is(name)),
                ..Selector::entity() },
            home: {
                let mut home = region.origin;
                home.y += comp;
                home.as_abs() },
            // FIXME: This is not really a mask.  It is the mask plus one.
            // Think of a name for that.
            mask: 1 << (comp * 4),
        })
    }
    components
}

pub fn mem_team(region: &MemoryRegion) -> String {
    format!("t_{}", mem_name(region))
}

pub fn mem_comps_selector(region: &MemoryRegion) -> Selector {
    Selector {
        team: Some(SelectorTeam::On(mem_team(region))),
        ..Selector::entity()
    }
}

pub fn make_mem_ctrl(region: &MemoryRegion) -> IntoIter<AssembledItem> {
    let track_output = true;

    let mut items = vec![];
    let label = mem_label(region);

    items.push(Label(label.clone()));
    items.push(power_off(label, track_output));

    mem_ctrl_home(region, track_output, &mut items);
    mem_ctrl_move(region, track_output, &mut items);
    mem_ctrl_read(region, track_output, &mut items);
    mem_ctrl_write(region, track_output, &mut items);
    items.push(Terminal);

    items.into_iter()
}

fn mem_ctrl_home(
    region: &MemoryRegion, track_output: bool, items: &mut Vec<AssembledItem>)
{
    use commands::Command::Teleport;

    for comp in mem_components(region) {
        let cmd = cmd_block(Teleport(
            Some(comp.sel.to_target()), comp.home), track_output);
        items.push(Complete(cmd));
    }
}

fn mem_ctrl_move(
    region: &MemoryRegion, track_output: bool, items: &mut Vec<AssembledItem>)
{
    use commands::Command::{Execute, Teleport};
    use types::REL_ZERO;

    let sel = mem_selector(region);

    // sub MemAddr, region.start
    let cmd = cmd_block(players::remove(
        sel.to_target(), "MemAddr".to_string(), region.start as i32, None), track_output);
    items.push(Complete(cmd));
    // mov t0, 4
    let cmd = cmd_block(players::set(
        sel.to_target(), "t0".to_string(), 4, None), track_output);
    items.push(Complete(cmd));
    // sdiv MemAddr, t0
    let cmd = cmd_block(players::div_op(
        sel.to_target(), "MemAddr".to_string(),
        sel.to_target(), "t0".to_string()), track_output);
    items.push(Complete(cmd));
    for (part, pos) in region.traversal() {
        let part = part as i32;

        let part_sel = Selector {
            scores: {
                let mut s = HashMap::new();
                s.insert("MemAddr".to_string(), Interval::Min(part));
                s },
            ..sel.clone()
        };

        let comp_sel = mem_comps_selector(region);

        let cmd = cmd_block(Execute(part_sel.to_target(), REL_ZERO,
            Box::new(Teleport(Some(comp_sel.to_target()), pos))), track_output);
        items.push(Complete(cmd));

        let cmd = cmd_block(players::remove(
            part_sel.into_target(), "MemAddr".to_string(), part, None), track_output);
        items.push(Complete(cmd));
    }
}

fn mem_read_cmd(region: &MemoryRegion, cmd: Command) -> Command {
    use types::REL_ZERO;
    let sel = mem_op_selector(region, 0);
    Command::Execute(sel.into_target(), REL_ZERO, Box::new(cmd))
}

fn mem_ctrl_read(
    region: &MemoryRegion, track_output: bool, items: &mut Vec<AssembledItem>)
{
    // TODO: mem_ctrl_read has the same problems as mem_ctrl_write.  Surely it
    // can share some code with Assembler::accum_bits.  And surely it can be
    // made more clear.

    use commands::Command::{Execute, ExecuteDetect};
    use types::REL_ZERO;

    let main_sel = mem_selector(region);
    let comps_sel = mem_comps_selector(region);
    let nearest_sel = Selector {
        count: Some(1),
        ..comps_sel.clone()
    };

    // Zero MemData
    let cmd = mem_read_cmd(
        region, players::set(
            main_sel.to_target(), "MemData".to_string(), 0, None));
    items.push(Complete(cmd_block(cmd, track_output)));

    let t0 = "t0".to_string();

    let clay = "minecraft:stained_hardened_clay".to_string();
    for data in (0..16) {
        let cmd = players::set(nearest_sel.to_target(), t0.clone(), data, None);
        let cmd = ExecuteDetect(
            comps_sel.to_target(), REL_ZERO,
            REL_ZERO, clay.clone(), data, Box::new(cmd));
        let block = cmd_block(mem_read_cmd(region, cmd), track_output);
        items.push(Complete(block));
    }

    // each-in-comps_sel: mul nearest_sel, t0, nearest_sel, MemMask
    let cmd = players::mul_op(
        nearest_sel.to_target(), t0.clone(),
        nearest_sel.to_target(), "MemMask".to_string());
    let cmd = Execute(comps_sel.to_target(), REL_ZERO, Box::new(cmd));
    let cmd = mem_read_cmd(region, cmd);
    items.push(Complete(cmd_block(cmd, track_output)));

    // each-in-comps_sel: add main_sel, MemData, nearest_sel, t0
    let cmd = players::add_op(
        main_sel.to_target(), "MemData".to_string(),
        nearest_sel.to_target(), t0.clone());
    let cmd = Execute(comps_sel.to_target(), REL_ZERO, Box::new(cmd));
    let cmd = mem_read_cmd(region, cmd);
    items.push(Complete(cmd_block(cmd, track_output)));
}

fn mem_write_cmd(region: &MemoryRegion, cmd: Command) -> Command {
    use types::REL_ZERO;
    let sel = mem_op_selector(region, 1);
    Command::Execute(sel.into_target(), REL_ZERO, Box::new(cmd))
}

fn mem_ctrl_write(
    region: &MemoryRegion, track_output: bool, items: &mut Vec<AssembledItem>)
{
    // TODO: mem_ctrl_write got really messy before all was said and done.  It
    // also shares the spirit of its function with Assembler::expand_bits.  The
    // two should be unified, if possible, and made more readable.

    use commands::Command::{Execute, SetBlock};
    use types::REL_ZERO;
    use std::i32;

    let main_sel = mem_selector(region);
    let lt_zero_sel = {
        let mut sel = mem_op_selector(region, 1);
        sel.scores.insert("MemData".to_string(), Interval::Max(-1));
        sel
    };

    let comps_sel = mem_comps_selector(region);
    let nearest_sel = Selector {
        count: Some(1),
        ..comps_sel.clone()
    };
    let high_comp = mem_components(region).swap_remove(7);

    let t0 = "t0".to_string();

    // mov mem_comps, t0, mem_entity, MemData
    let cmd = mem_write_cmd(region, players::asn_op(
        comps_sel.to_target(), t0.clone(),
        main_sel.to_target(), "MemData".to_string()));
    items.push(Complete(cmd_block(cmd, track_output)));

    // If MemData is negative, flip the sign of all temp values. This causes the
    // high bit to always end up zero, so that is handled later.
    // TODO: Ensure that MIN is set on the main memory entity and use add_op,
    // instead of two adds.
    let cmd = Execute(
        lt_zero_sel.to_target(), REL_ZERO,
        Box::new(players::add(comps_sel.to_target(), t0.clone(), i32::MAX, None)));
    items.push(Complete(cmd_block(cmd, track_output)));
    let cmd = Execute(
        lt_zero_sel.to_target(), REL_ZERO,
        Box::new(players::add(comps_sel.to_target(), t0.clone(), 1, None)));
    items.push(Complete(cmd_block(cmd, track_output)));

    // sdiv mem_comps, t0, mem_comps, MemMask
    let cmd = mem_write_cmd(
        region, Execute(
            comps_sel.to_target(), REL_ZERO, Box::new(players::div_op(
                nearest_sel.to_target(), t0.clone(),
                nearest_sel.to_target(), "MemMask".to_string()))));
    items.push(Complete(cmd_block(cmd, track_output)));

    // mov mem_entity, t0, 16
    let cmd = mem_write_cmd(
        region, players::set(main_sel.to_target(), t0.clone(), 16, None));
    items.push(Complete(cmd_block(cmd, track_output)));

    // srem mem_comps, t0, mem_entity, t0
    let cmd = mem_write_cmd(
        region, players::rem_op(
            comps_sel.to_target(), t0.clone(),
            main_sel.to_target(), t0.clone()));
    items.push(Complete(cmd_block(cmd, track_output)));

    // If MemData is negative, apply the high bit to the high component.
    let cmd = Execute(
        lt_zero_sel.to_target(), REL_ZERO,
        Box::new(players::add(high_comp.sel.to_target(), t0.clone(), 8, None)));
    items.push(Complete(cmd_block(cmd, track_output)));

    let clay = "minecraft:stained_hardened_clay".to_string();
    for data in (0..16) {
        let comps_data_sel = Selector {
            scores: {
                let mut s = HashMap::new();
                s.insert(t0.clone(), Interval::Bounded(data, data));
                s },
            ..comps_sel.clone()
        };

        let cmd = SetBlock(REL_ZERO, clay.clone(), Some(data), None, None);
        let cmd = Execute(comps_data_sel.into_target(), REL_ZERO, Box::new(cmd));
        let block = cmd_block(mem_write_cmd(region, cmd), track_output);
        items.push(Complete(block));
    }
}
