use ast::{Cond, Op, Register, Statement};
use ast::Op::*;
use ast::Statement::*;
use commands::{
    Command, Objective, PlayerOp, Selector, SelectorName, SelectorTeam, Target,
    Team, players};
use commands::Command::*;
use std::boxed::FnBox;
use nbt::*;
use types::{self, Block, Extent, Interval, REL_ZERO};

use std::collections::{HashMap, VecDeque};
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
    target: Target,
    selector: Selector,
    // TODO: Add uses_memory.
    //uses_memory: bool,
    uses_bitwise: bool,
    done: bool,
    unique: u32,
    pending_labels: Vec<String>,
    team_bit: Team,
    tgt_bit_all: Target,
    tgt_bit_one: Target,
    obj_bit_tmp1: Objective,
    obj_bit_tmp2: Objective,
    obj_bit_comp: Objective,
    obj_bit_num: Objective,
    obj_two: Objective,
    obj_min: Objective,
}

impl<S : Iterator<Item=Statement>> Assembler<S> {
    pub fn new(assembly: S) -> Assembler<S> {
        let entity_name = "computer".to_string();
        let selector = Selector {
            name: Some(SelectorName::Is(entity_name)),
            ..Selector::entity()
        };
        let target = Target::Sel(selector.clone());
        let team_bit = "Shifters";
        Assembler {
            input: assembly,
            buffer: VecDeque::new(),
            target: target,
            selector: selector,
            // TODO: Add uses_memory.
            //uses_memory: false,
            uses_bitwise: false,
            done: false,
            unique: 0,
            pending_labels: vec!(),
            team_bit: team_bit.to_string(),
            tgt_bit_all: Target::Sel(Selector {
                team: Some(SelectorTeam::On(team_bit.to_string())),
                ..Selector::entity() }),
            tgt_bit_one: Target::Sel(Selector {
                team: Some(SelectorTeam::On(team_bit.to_string())),
                count: Some(1),
                ..Selector::entity() }),
            obj_bit_tmp1: "BitTmp1".to_string(),
            obj_bit_tmp2: "BitTmp2".to_string(),
            obj_bit_comp: "BitComponent".to_string(),
            obj_bit_num: "BitNumber".to_string(),
            obj_two: "TWO".to_string(),
            obj_min: "MIN".to_string(),
        }
    }

    fn assemble(&mut self, stmt: Statement) {
        match stmt {
            LabelStmt(label) => { self.emit(Label(label)); }
            Instr(conds, op) => { self.assemble_instr(conds, op); }
        }
    }

    fn make_op_cmd_rr(
        &self, lhs: Register, op: PlayerOp, rhs: Register) -> Command
    {
        players::op(
            self.target.clone(), reg_name(lhs), op,
            self.target.clone(), reg_name(rhs))
    }

    fn make_op_cmd_rx(
        &self, lhs: Register, op: PlayerOp, rtgt: Target, robj: Objective)
        -> Command
    {
        players::op(self.target.clone(), reg_name(lhs), op, rtgt, robj)
    }

    fn make_op_cmd_xr(
        &self, ltgt: Target, lobj: Objective, op: PlayerOp, rhs: Register)
        -> Command
    {
        players::op(ltgt, lobj, op, self.target.clone(), reg_name(rhs))
    }

    fn expand_bits(&mut self, conds: Vec<Cond>, reg: Register, bit_obj: Objective) {
        let tgt_all = self.tgt_bit_all.clone();

        // Set all bit entities' bit_obj to the value to be expanded, reg.
        // Like this: [11, 11, 11, 11]
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            self.make_op_cmd_xr(
                tgt_all.clone(), bit_obj.clone(), PlayerOp::Asn, reg.clone()));
        self.emit(Complete(block));

        // Divide all bit entities' bit_obj by their bit component.
        // Like this: [11, 11, 11, 11] / [8, 4, 2, 1] = [1, 2, 5, 11]
        let bit_comp = self.obj_bit_comp.clone();
        self.bit_vec_op(conds.clone(), bit_obj.clone(), PlayerOp::Div, bit_comp);

        // Modulo all bit entities' bit_obj by two to produce a vector of 1s
        // and 0s representing the bits of reg.
        // Like this: [1, 2, 5, 11] %= 2 = [1, 0, 1, 1]
        let block = make_cmd_block(
            self.selector.clone(), conds, players::rem_op(
                tgt_all, bit_obj.clone(),
                self.target.clone(), self.obj_two.clone()));
        self.emit(Complete(block));
    }

    fn accum_bits(&mut self, conds: Vec<Cond>, dst: Register, bit_obj: String) {
        // Multiply all bit entities' bit_obj by their bit component.
        // Like this: [1, 0, 1, 1] * [8, 4, 2, 1] = [8, 0, 2, 1]
        let bit_comp = self.obj_bit_comp.clone();
        self.bit_vec_op(conds.clone(), bit_obj.clone(), PlayerOp::Mul, bit_comp);

        // Zero the dst register.
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::set(self.target.clone(), reg_name(dst.clone()), 0, None));
        self.emit(Complete(block));

        // Accumulate the bit entities' bit_obj into dst.
        // Like this: dst + [8, 0, 2, 1] = 11
        let block = make_cmd_block(
            self.selector.clone(), conds, Execute(
                self.tgt_bit_all.clone(), REL_ZERO,
                Box::new(self.make_op_cmd_rx(
                    dst, PlayerOp::Add, self.tgt_bit_one.clone(), bit_obj))));
        self.emit(Complete(block));
    }

    fn bit_vec_op(&mut self, conds: Vec<Cond>, lhs: Objective, op: PlayerOp, rhs: Objective) {
        //execute @e[team=Shifters] ~ ~ ~ scoreboard players operation @e[team=Shifters,c=1] BitTmp1 *= @e[team=Shifters,c=1] BitTmp2
        let block = make_cmd_block(
            self.selector.clone(), conds, Execute(
                self.tgt_bit_all.clone(), REL_ZERO,
                Box::new(players::op(
                    self.tgt_bit_one.clone(), lhs, op,
                    self.tgt_bit_one.clone(), rhs))));
        self.emit(Complete(block));
    }

    fn activate_bitwise_entities(&mut self, conds: Vec<Cond>, amount: Register) {
        let bit_num = self.obj_bit_num.clone();
        let tmp1 = self.obj_bit_tmp1.clone();

        // SIMD copy bitwise entities' BitNumber to tmp1
        self.bit_vec_op(
            conds.clone(), tmp1.clone(), PlayerOp::Asn, bit_num);

        // Vector-scalar remove 32 from bitwise entities' tmp1
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::remove(self.tgt_bit_all.clone(), tmp1.clone(), 32, None));
        self.emit(Complete(block));

        // Vector-scalar add shift amount to bitwise entities' tmp1.
        // This makes all active shifters greater than or equal to zero.
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), self.make_op_cmd_xr(
                self.tgt_bit_all.clone(), tmp1.clone(), PlayerOp::Add, amount));
        self.emit(Complete(block));
    }

    fn raw_shift_right(&mut self, conds: Vec<Cond>, dst: Register, src: Register) {
        let tmp1 = self.obj_bit_tmp1.clone();
        let mut lt_zero_conds = conds.clone();
        lt_zero_conds.push(Cond::lt(dst.clone(), 0));

        let tmp1_reg = Register::Spec(tmp1.clone());
        let two_reg = Register::Spec(self.obj_two.clone());
        let min_reg = Register::Spec(self.obj_min.clone());

        // Copy to tmp1
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), self.make_op_cmd_rr(
                tmp1_reg.clone(), PlayerOp::Asn, dst.clone()));
        self.emit(Complete(block));

        // if dst < 0, tmp1 -= i32::MIN
        let block = make_cmd_block(
            self.selector.clone(), lt_zero_conds.clone(),
            self.make_op_cmd_rr(tmp1_reg.clone(), PlayerOp::Sub, min_reg));
        self.emit(Complete(block));

        self.activate_bitwise_entities(conds.clone(), src);

        let active_bit_tgt = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            scores: {
                let mut s = HashMap::new();
                s.insert(tmp1, Interval::Min(0));
                s },
            ..Selector::entity()
        });
        // execute-in-bitwise-entities: divide computer tmp1 by TWO if entity tmp1 > 0
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), Execute(
                active_bit_tgt, REL_ZERO,
                Box::new(self.make_op_cmd_rr(
                    tmp1_reg.clone(), PlayerOp::Div, two_reg))));
        self.emit(Complete(block));
    }

    fn assemble_instr(&mut self, conds: Vec<Cond>, op: Op) {
        match op {
            AddRR(dst, src) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rr(dst, PlayerOp::Add, src));
                self.emit(Complete(block));
            }
            AddXR(tgt, obj, src, success) => {
                let mut block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_xr(tgt, obj, PlayerOp::Add, src));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            SubRR(dst, src) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rr(dst, PlayerOp::Sub, src));
                self.emit(Complete(block));
            }
            SubXR(sel, obj, src, success) => {
                let mut block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_xr(sel, obj, PlayerOp::Sub, src));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            And(dst, src) => {
                self.uses_bitwise = true;

                let tmp1 = self.obj_bit_tmp1.clone();
                let tmp2 = self.obj_bit_tmp2.clone();

                self.expand_bits(conds.clone(), dst.clone(), tmp1.clone());
                self.expand_bits(conds.clone(), src, tmp2.clone());
                // 'and' the bits together.
                self.bit_vec_op(conds.clone(), tmp1.clone(), PlayerOp::Mul, tmp2);
                self.accum_bits(conds.clone(), dst, tmp1);
            }
            Orr(dst, src) => {
                self.uses_bitwise = true;

                let tmp1 = self.obj_bit_tmp1.clone();
                let tmp2 = self.obj_bit_tmp2.clone();

                self.expand_bits(conds.clone(), dst.clone(), tmp1.clone());
                self.expand_bits(conds.clone(), src, tmp2.clone());
                // 'orr' the bits together.
                self.bit_vec_op(conds.clone(), tmp1.clone(), PlayerOp::Max, tmp2);
                self.accum_bits(conds.clone(), dst, tmp1);
            }
            Eor(dst, src) => {
                self.uses_bitwise = true;

                let tmp1 = self.obj_bit_tmp1.clone();
                let tmp2 = self.obj_bit_tmp2.clone();

                self.expand_bits(conds.clone(), dst.clone(), tmp1.clone());
                self.expand_bits(conds.clone(), src, tmp2.clone());
                // 'eor' the bits together.
                self.bit_vec_op(conds.clone(), tmp1.clone(), PlayerOp::Add, tmp2);
                let block = make_cmd_block(
                    self.selector.clone(), conds.clone(), players::rem_op(
                        self.tgt_bit_all.clone(), tmp1.clone(),
                        self.target.clone(), self.obj_two.clone()));
                self.emit(Complete(block));
                self.accum_bits(conds.clone(), dst, tmp1);
            }
            AsrRR(dst, src) => {
                self.uses_bitwise = true;

                self.raw_shift_right(conds.clone(), dst.clone(), src);

                let tmp1 = self.obj_bit_tmp1.clone();
                let mut lt_zero_conds = conds.clone();
                lt_zero_conds.push(Cond::lt(dst.clone(), 0));
                let tmp1_reg = Register::Spec(tmp1.clone());

                let sign_bits_tgt = Target::Sel(Selector {
                    team: Some(SelectorTeam::On(self.team_bit.clone())),
                    scores: {
                        let mut s = HashMap::new();
                        s.insert(tmp1, Interval::Min(-1));
                        s },
                    ..Selector::entity()
                });
                // if dst < 0 execute-in-bitwise-entities: computer tmp1 += entity BitComponent
                let block = make_cmd_block(
                    self.selector.clone(), lt_zero_conds, Execute(
                        sign_bits_tgt, REL_ZERO,
                        Box::new(self.make_op_cmd_rx(
                            tmp1_reg.clone(), PlayerOp::Add,
                            self.tgt_bit_one.clone(), self.obj_bit_comp.clone()))));
                self.emit(Complete(block));

                // copy computer tmp1 to dst
                let block = make_cmd_block(
                    self.selector.clone(), conds, self.make_op_cmd_rr(
                        dst, PlayerOp::Asn, tmp1_reg));
                self.emit(Complete(block));
            }
            LsrRR(dst, src) => {
                self.uses_bitwise = true;

                self.raw_shift_right(conds.clone(), dst.clone(), src);

                let tmp1 = self.obj_bit_tmp1.clone();
                let mut lt_zero_conds = conds.clone();
                lt_zero_conds.push(Cond::lt(dst.clone(), 0));
                let tmp1_reg = Register::Spec(tmp1.clone());

                let high_bit_tgt = Target::Sel(Selector {
                    team: Some(SelectorTeam::On(self.team_bit.clone())),
                    count: Some(1),
                    scores: {
                        let mut s = HashMap::new();
                        s.insert(tmp1, Interval::Bounded(-1, -1));
                        s },
                    ..Selector::entity()
                });
                // if dst < 0 computer tmp1 += entity[high-bit] BitComponent
                let block = make_cmd_block(
                    self.selector.clone(), lt_zero_conds, self.make_op_cmd_rx(
                        tmp1_reg.clone(), PlayerOp::Add,
                        high_bit_tgt, self.obj_bit_comp.clone()));
                self.emit(Complete(block));

                // copy computer tmp1 to dst
                let block = make_cmd_block(
                    self.selector.clone(), conds, self.make_op_cmd_rr(
                        dst, PlayerOp::Asn, tmp1_reg));
                self.emit(Complete(block));
            }
            LslRR(dst, src) => {
                self.uses_bitwise = true;

                self.activate_bitwise_entities(conds.clone(), src);

                let tmp1 = self.obj_bit_tmp1.clone();
                let two_reg = Register::Spec(self.obj_two.clone());

                let active_bit_tgt = Target::Sel(Selector {
                    team: Some(SelectorTeam::On(self.team_bit.clone())),
                    scores: {
                        let mut s = HashMap::new();
                        s.insert(tmp1, Interval::Min(-1));
                        s },
                    ..Selector::entity()
                });
                // execute-in-bitwise-entities: dst *= TWO
                let block = make_cmd_block(
                    self.selector.clone(), conds.clone(), Execute(
                        active_bit_tgt, REL_ZERO,
                        Box::new(self.make_op_cmd_rr(
                            dst, PlayerOp::Mul, two_reg))));
                self.emit(Complete(block));
            }
            MovRR(dst, src) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rr(dst, PlayerOp::Asn, src));
                self.emit(Complete(block));
            }
            MovRI(dst, imm) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    players::set(self.target.clone(), reg_name(dst), imm, None));
                self.emit(Complete(block));
            }
            MovRX(dst, sel, obj) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rx(dst, PlayerOp::Asn, sel, obj));
                self.emit(Complete(block));
            }
            MovXR(sel, obj, src, success) => {
                let mut block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_xr(sel, obj, PlayerOp::Asn, src));
                self.add_success_count(&mut block, success);
                self.emit(Complete(block));
            }
            MulRR(dst, src) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rr(dst, PlayerOp::Mul, src));
                self.emit(Complete(block));
            }
            SdivRR(dst, src) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    self.make_op_cmd_rr(dst, PlayerOp::Div, src));
                self.emit(Complete(block));
            }
            Srng(dst, test, min, max) => {
                let mut one_conds = conds.clone();
                if let Some(interval) = Interval::new(min, max) {
                    one_conds.push(Cond::new(test, interval));
                } else {
                    // TODO: Issue a warning.
                }

                let zero_block = make_cmd_block(
                    self.selector.clone(), conds,
                    players::set(
                        self.target.clone(), reg_name(dst.clone()), 0, None));

                let one_block = make_cmd_block(
                    self.selector.clone(), one_conds,
                    players::set(self.target.clone(), reg_name(dst), 1, None));

                self.emit(Complete(zero_block));
                self.emit(Complete(one_block));
            }
            BrL(label) => {
                // FIXME: formalize unique label generation.
                let cont_label = format!("{}_cont_{}", label, self.unique);
                self.unique += 1;

                let zero_block = make_cmd_block(
                    self.selector.clone(), vec!(),
                    players::set(
                        self.target.clone(), "TEST".to_string(), 0, None));

                let one_block = make_cmd_block(
                    self.selector.clone(), conds,
                    players::set(
                        self.target.clone(), "TEST".to_string(), 1, None));

                self.emit(Complete(zero_block));
                self.emit(Complete(one_block));

                let selector = self.selector.clone();
                self.emit(Pending(label, Box::new(move |extent| {
                    match extent {
                        Extent::Empty => {
                            panic!("oh no!");
                        }
                        Extent::MinMax(min, max) => {
                            let test_reg = Register::Spec("TEST".to_string());
                            make_cmd_block(
                                selector, vec!(Cond::eq(test_reg, 1)),
                                Fill(
                                    min.as_abs(), max.as_abs(),
                                    "minecraft:redstone_block".to_string(),
                                    None, None, None))
                        }
                    }
                })));

                let selector = self.selector.clone();
                self.emit(Pending(cont_label.clone(), Box::new(move |extent| {
                    match extent {
                        Extent::Empty => {
                            panic!("oh no!");
                        }
                        Extent::MinMax(min, max) => {
                            let test_reg = Register::Spec("TEST".to_string());
                            make_cmd_block(
                                selector, vec!(Cond::eq(test_reg, 0)),
                                Fill(
                                    min.as_abs(), max.as_abs(),
                                    "minecraft:redstone_block".to_string(),
                                    None, None, None))
                        }
                    }
                })));

                self.emit(Terminal);
                self.emit(Label(cont_label));
            }
            BrR(_) => {
                let block = make_cmd_block(
                    self.selector.clone(), conds,
                    Say("FIXME: emit BrR".to_string()));
                self.emit(Complete(block));
                self.emit(Terminal);
            }
            RawCmd(cmd) => {
                let block = make_cmd_block(self.selector.clone(), conds, Raw(cmd));
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
        let stats = make_command_stats(self.target.clone(), reg_name(reg));
        block.nbt.insert("CommandStats".to_string(), stats);
    }

    fn make_label_power_off_item(&self, label: String) -> AssembledItem {
        let selector = self.selector.clone();
        Pending(label, Box::new(move |extent| {
            match extent {
                Extent::Empty => {
                    panic!("oh no!");
                }
                Extent::MinMax(min, max) => {
                    make_cmd_block(
                        selector, vec!(),
                        Fill(
                            min.as_abs(), max.as_abs(),
                            "minecraft:obsidian".to_string(),
                            None, None, None))
                }
            }
        }))
    }

}

fn make_cmd_block(selector: Selector, conds: Vec<Cond>, cmd: Command) -> Block {
    let mut final_cmd = cmd;
    for cond in conds.into_iter() {
        let mut sel = selector.clone();
        sel.scores.insert(reg_name(cond.reg), cond.interval);
        final_cmd = Command::Execute(
            Target::Sel(sel), types::REL_ZERO, Box::new(final_cmd));
    }

    let mut nbt = NbtCompound::new();
    nbt.insert("Command".to_string(), Nbt::String(final_cmd.to_string()));
    // FIXME: Add a flag to control whether TrackOutput is on or off by default.
    nbt.insert("TrackOutput".to_string(), Nbt::Byte(1));
    Block {
        id: "minecraft:command_block".to_string(),
        data: 0,
        nbt: nbt,
    }
}

fn reg_name(reg: Register) -> String {
    match reg {
        Register::Gen(n) => format!("r{}", n),
        Register::Pred(n) => format!("p{}", n),
        Register::Spec(s) => s,
    }
}

fn make_command_stats(success_count_tgt: Target, success_count_obj: String) -> Nbt {
    let mut stats = NbtCompound::new();
    stats.insert("SuccessCountName".to_string(), Nbt::String(success_count_tgt.to_string()));
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
