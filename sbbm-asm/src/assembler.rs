use ast::{Cond, Op, Register, Statement};
use ast::Op::*;
use ast::Statement::*;
use core::{Block, Extent};
use std::boxed::FnBox;
use nbt::*;
use std::collections::VecDeque;
use std::fmt;
use std::mem;
use self::AssembledItem::*;

pub type PendingFn = Box<FnBox(Extent) -> Block>;

pub enum AssembledItem {
    Label(String),
    Complete(Block),
    Pending(String, PendingFn),
    Terminal,
}

impl fmt::Debug for AssembledItem {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Label(ref label) => write!(formatter, "Label({:?})", label),
            Complete(ref block) => write!(formatter, "Complete({:?}", block),
            Pending(ref name, _) => write!(formatter, "Pending({:?}, ...)", name),
            Terminal => formatter.write_str("Terminal"),
        }
    }
}

pub struct Assembler<Source : Iterator<Item=Statement>> {
    input: Source,
    buffer: VecDeque<AssembledItem>,
    entity_name: String,
    selector: String,
    // TODO: Add these.
    //uses_memory: bool,
    //uses_bitwise: bool,
    done: bool,
    unique: u32,
    pending_labels: Vec<String>,
}

impl<S : Iterator<Item=Statement>> Assembler<S> {
    pub fn new(assembly: S) -> Assembler<S> {
        let entity_name = "computer".to_string();
        let selector = format!("@e[name={}]", entity_name);
        Assembler {
            input: assembly,
            buffer: VecDeque::new(),
            entity_name: entity_name,
            selector: selector,
            // TODO: Add these.
            //uses_memory: false,
            //uses_bitwise: false,
            done: false,
            unique: 0,
            pending_labels: vec!(),
        }
    }

    fn assemble(&mut self, stmt: Statement) {
        match stmt {
            LabelStmt(label) => { self.emit(Label(label)); }
            Instr(conds, op) => { self.assemble_instr(conds, op); }
        }
    }

    fn assemble_instr(&mut self, conds: Vec<Cond>, op: Op) {
        match op {
            AddRR(dst, src) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} += {} {}",
                        self.selector, reg_name(dst),
                        self.selector, reg_name(src)));
                self.emit(Complete(block));
            }
            AddXR(sel, obj, src, success) => {
                let mut block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} += {} {}",
                        sel, obj, self.selector, reg_name(src)));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            SubRR(dst, src) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} -= {} {}",
                        self.selector, reg_name(dst),
                        self.selector, reg_name(src)));
                self.emit(Complete(block));
            }
            SubXR(sel, obj, src, success) => {
                let mut block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} -= {} {}",
                        sel, obj, self.selector, reg_name(src)));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            MovRR(dst, src) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} = {} {}",
                        self.selector, reg_name(dst), self.selector, reg_name(src)));
                self.emit(Complete(block));
            }
            MovRI(dst, imm) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players set {} {} {}",
                        self.selector, reg_name(dst), imm));
                self.emit(Complete(block));
            }
            MovRX(dst, sel, obj) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} = {} {}",
                        self.selector, reg_name(dst), sel, obj));
                self.emit(Complete(block));
            }
            MovXR(sel, obj, src, success) => {
                let mut block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} = {} {}",
                        sel, obj, self.selector, reg_name(src)));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            MulRR(dst, src) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} *= {} {}",
                        self.selector, reg_name(dst),
                        self.selector, reg_name(src)));
                self.emit(Complete(block));
            }
            SdivRR(dst, src) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players operation {} {} /= {} {}",
                        self.selector, reg_name(dst),
                        self.selector, reg_name(src)));
                self.emit(Complete(block));
            }
            Srng(dst, test, min, max) => {
                let mut one_conds = conds.clone();
                one_conds.push(Cond { reg: test, min: min, max: max });

                let zero_block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players set {} {} 0",
                        self.selector, reg_name(dst.clone())));

                let one_block = make_cmd_block(
                    &self.entity_name[..], one_conds, format!(
                        "scoreboard players set {} {} 1",
                        self.selector, reg_name(dst)));

                self.emit(Complete(zero_block));
                self.emit(Complete(one_block));
            }
            BrL(label) => {
                // FIXME: formalize unique label generation.
                let cont_label = format!("{}_cont_{}", label, self.unique);
                self.unique += 1;

                let zero_block = make_cmd_block(
                    &self.entity_name[..], vec!(), format!(
                        "scoreboard players set {} {} 0",
                        self.selector, "Test"));

                let one_block = make_cmd_block(
                    &self.entity_name[..], conds, format!(
                        "scoreboard players set {} {} 1",
                        self.selector, "Test"));

                self.emit(Complete(zero_block));
                self.emit(Complete(one_block));

                let entity_name = self.entity_name.clone();
                self.emit(Pending(label, Box::new(move |extent| {
                    match extent {
                        Extent::Empty => {
                            panic!("oh no!");
                        }
                        Extent::MinMax(min, max) => {
                            make_cmd_block(
                                &entity_name[..], vec!(Cond { reg: Register::Spec("Test".to_string()), min: Some(1), max: Some(1) }),
                                format!("fill {} {} {} {} {} {} minecraft:redstone_block",
                                        min.x, min.y, min.z, max.x, max.y, max.z))
                        }
                    }
                })));

                let entity_name = self.entity_name.clone();
                self.emit(Pending(cont_label.clone(), Box::new(move |extent| {
                    match extent {
                        Extent::Empty => {
                            panic!("oh no!");
                        }
                        Extent::MinMax(min, max) => {
                            make_cmd_block(
                                &entity_name[..], vec!(Cond { reg: Register::Spec("Test".to_string()), min: Some(0), max: Some(0) }),
                                format!("fill {} {} {} {} {} {} minecraft:redstone_block",
                                        min.x, min.y, min.z, max.x, max.y, max.z))
                        }
                    }
                })));

                self.emit(Terminal);
                self.emit(Label(cont_label));
            }
            BrR(_) => {
                let block = make_cmd_block(
                    &self.entity_name[..], conds, "say FIXME: emit BrR".to_string());
                self.emit(Complete(block));
                self.emit(Terminal);
            }
            RawCmd(cmd) => {
                let block = make_cmd_block(&self.entity_name[..], conds, cmd);
                self.emit(Complete(block));
            }
            _ => panic!("not implemented: {:?}", op)
        }
    }

    fn emit(&mut self, item: AssembledItem) {
        match item {
            Label(label) => {
                // Queue labels so they can be processed all at once, so extra
                // power-off blocks are elided.
                self.pending_labels.push(label);
            }
            _ => {
                // Flush pending labels
                if !self.pending_labels.is_empty() {
                    let first_label = self.pending_labels[0].clone();

                    // FIXME: Use drain when it is no longer unstable.
                    let mut labels = vec!();
                    mem::swap(&mut labels, &mut self.pending_labels);
                    for label in labels.into_iter() {
                        self.buffer.push_back(Label(label));
                    }

                    let off_item = self.make_label_power_off_item(first_label);
                    self.buffer.push_back(off_item);
                }

                self.buffer.push_back(item);
            }
        };

    }

    fn add_success_count(&self, block: &mut Block, reg: Register) {
        let stats = make_command_stats(self.selector.clone(), reg_name(reg));
        block.nbt.insert("CommandStats".to_string(), stats);
    }

    fn make_label_power_off_item(&self, label: String) -> AssembledItem {
        let entity_name = self.entity_name.clone();
        Pending(label, Box::new(move |extent| {
            match extent {
                Extent::Empty => {
                    panic!("oh no!");
                }
                Extent::MinMax(min, max) => {
                    make_cmd_block(
                        &entity_name[..], vec!(),
                        format!("fill {} {} {} {} {} {} minecraft:obsidian",
                                min.x, min.y, min.z, max.x, max.y, max.z))
                }
            }
        }))
    }

}

fn make_cmd_block(entity_name: &str, conds: Vec<Cond>, cmd: String) -> Block {
    let mut final_cmd = cmd;
    for cond in conds.into_iter() {
        let sel = format!(
            "@e[name={},score_{}_min={},score_{}={}]",
            entity_name,
            reg_name(cond.reg.clone()), fmt_opt(cond.min),
            reg_name(cond.reg), fmt_opt(cond.max));
        final_cmd = format!("execute {} ~ ~ ~ {}", sel, final_cmd);
    }

    let mut nbt = NbtCompound::new();
    nbt.insert("Command".to_string(), Nbt::String(final_cmd));
    nbt.insert("TrackOutput".to_string(), Nbt::Byte(1));
    Block {
        id: "minecraft:command_block".to_string(),
        data: 0,
        nbt: nbt,
        // FIXME: Add NBT for the command and conditions.
    }
}

fn reg_name(reg: Register) -> String {
    match reg {
        Register::Gen(n) => format!("r{}", n),
        Register::Pred(n) => format!("p{}", n),
        Register::Spec(s) => s,
    }
}

fn fmt_opt<T : ToString>(value: Option<T>) -> String {
    if let Some(value) = value {
        value.to_string()
    } else {
        "*".to_string()
    }
}

fn make_command_stats(success_count_sel: String, success_count_obj: String) -> Nbt {
    let mut stats = NbtCompound::new();
    stats.insert("SuccessCountName".to_string(), Nbt::String(success_count_sel));
    stats.insert("SuccessCountObjective".to_string(), Nbt::String(success_count_obj));
    Nbt::Compound(stats)
}

impl<S : Iterator<Item=Statement>> Iterator for Assembler<S> {
    type Item = AssembledItem;

    fn next(&mut self) -> Option<AssembledItem> {
        while self.buffer.is_empty() {
            if let Some(stmt) = self.input.next() {
                self.assemble(stmt);
            } else {
                self.done = true;
                return None
            }
        }
        self.buffer.pop_front()
    }
}
