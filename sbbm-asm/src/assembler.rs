use ast::{CommandBlockOut, Cond, Op, Register, Statement};
use ast::Op::*;
use ast::Statement::*;
use commands::{
    Command, Objective, PlayerOp, Selector, SelectorName, SelectorTeam, Target,
    Team, players, self};
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
    obj_bit_comp: Objective,
    obj_bit_num: Objective,
    obj_tmp0: Objective,
    obj_tmp1: Objective,
    obj_tmp2: Objective,
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
            obj_bit_comp: "BitComponent".to_string(),
            obj_bit_num: "BitNumber".to_string(),
            obj_tmp0: "t0".to_string(),
            obj_tmp1: "t1".to_string(),
            obj_tmp2: "t2".to_string(),
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

    fn assemble_instr(&mut self, conds: Vec<Cond>, op: Op) {
        use commands::PlayerOp as PlOp;

        match op {
            AddRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Add, &src),
            AddRI(dst, imm) => self.emit_radd(&conds, &dst, imm),
            AddXR(tgt, obj, src, success) =>
                self.emit_xr(&conds, &tgt, &obj, PlOp::Add, &src, &success),
            SubRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Sub, &src),
            SubRI(dst, imm) => self.emit_rsub(&conds, &dst, imm),
            SubXR(tgt, obj, src, success) =>
                self.emit_xr(&conds, &tgt, &obj, PlOp::Sub, &src, &success),
            AndRR(dst, src) => self.emit_and_rr(&conds, &dst, &src),
            OrrRR(dst, src) => self.emit_orr_rr(&conds, &dst, &src),
            EorRR(dst, src) => self.emit_eor_rr(&conds, &dst, &src),
            AsrRR(dst, src) => self.emit_asr_rr(&conds, &dst, &src),
            LsrRR(dst, src) => self.emit_lsr_rr(&conds, &dst, &src),
            LslRR(dst, src) => self.emit_lsl_rr(&conds, &dst, &src),
            MovRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Asn, &src),
            MovRI(dst, imm) => self.emit_rset(&conds, &dst, imm),
            MovRX(dst, tgt, obj) =>
                self.emit_rx(&conds, &dst, PlOp::Asn, &tgt, &obj),
            MovXR(tgt, obj, src, success) =>
                self.emit_xr(&conds, &tgt, &obj, PlOp::Asn, &src, &success),
            MulRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Mul, &src),
            SdivRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Div, &src),
            UdivRR(dst, src) => self.emit_udiv(conds, dst, src),
            SremRR(dst, src) => self.emit_rr(&conds, &dst, PlOp::Rem, &src),
            UremRR(dst, src) => self.emit_urem(conds, dst, src),
            Srng(dst, tst, min, max) => self.emit_srng(conds, dst, tst, min, max),
            BrL(label) => self.emit_br_l(conds, label),
            BrR(reg) => self.emit_br_r(conds, reg),
            RawCmd(outs, cmd) => {
                let mut block = make_cmd_block(self.selector.clone(), conds, Raw(cmd));
                self.add_command_stats(
                    &mut block, make_command_stats(self.target.clone(), outs));
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

    fn emit_rr(
        &mut self, conds: &Vec<Cond>, dst: &Register, op: PlayerOp,
        src: &Register)
    {
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            self.make_op_cmd_rr(dst.clone(), op, src.clone()));
        self.emit(Complete(block));
    }

    fn emit_xr(
        &mut self, conds: &Vec<Cond>, tgt: &Target, obj: &Objective,
        op: PlayerOp, src: &Register, success: &Register)
    {
        let mut block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            self.make_op_cmd_xr(tgt.clone(), obj.clone(), op, src.clone()));
        self.add_success_count(&mut block, success.clone());
        self.emit(Complete(block));
    }

    fn emit_rx(
        &mut self, conds: &Vec<Cond>, dst: &Register, op: PlayerOp,
        tgt: &Target, obj: &Objective)
    {
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            self.make_op_cmd_rx(dst.clone(), op, tgt.clone(), obj.clone()));
        self.emit(Complete(block));
    }

    fn emit_rset(&mut self, conds: &Vec<Cond>, dst: &Register, value: i32) {
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::set(self.target.clone(), reg_name(dst.clone()), value, None));
        self.emit(Complete(block));
    }

    fn emit_radd(&mut self, conds: &Vec<Cond>, dst: &Register, count: i32) {
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::add(self.target.clone(), reg_name(dst.clone()), count, None));
        self.emit(Complete(block));
    }

    fn emit_rsub(&mut self, conds: &Vec<Cond>, dst: &Register, count: i32) {
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::remove(self.target.clone(), reg_name(dst.clone()), count, None));
        self.emit(Complete(block));
    }

    fn emit_and_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        let t0_obj = self.obj_tmp0.clone();
        let t1_obj = self.obj_tmp1.clone();

        self.expand_bits(conds.clone(), dst.clone(), t0_obj.clone());
        self.expand_bits(conds.clone(), src.clone(), t1_obj.clone());
        // 'and' the bits together.
        self.bit_vec_op(conds.clone(), t0_obj.clone(), PlayerOp::Mul, t1_obj);
        self.accum_bits(conds.clone(), dst.clone(), t0_obj);
    }

    fn emit_orr_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        let t0_obj = self.obj_tmp0.clone();
        let t1_obj = self.obj_tmp1.clone();

        self.expand_bits(conds.clone(), dst.clone(), t0_obj.clone());
        self.expand_bits(conds.clone(), src.clone(), t1_obj.clone());
        // 'orr' the bits together.
        self.bit_vec_op(conds.clone(), t0_obj.clone(), PlayerOp::Max, t1_obj);
        self.accum_bits(conds.clone(), dst.clone(), t0_obj);
    }

    fn emit_eor_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        let t0_obj = self.obj_tmp0.clone();
        let t1_obj = self.obj_tmp1.clone();

        self.expand_bits(conds.clone(), dst.clone(), t0_obj.clone());
        self.expand_bits(conds.clone(), src.clone(), t1_obj.clone());
        // 'eor' the bits together.
        self.bit_vec_op(conds.clone(), t0_obj.clone(), PlayerOp::Add, t1_obj);
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), players::rem_op(
                self.tgt_bit_all.clone(), t0_obj.clone(),
                self.target.clone(), self.obj_two.clone()));
        self.emit(Complete(block));
        self.accum_bits(conds.clone(), dst.clone(), t0_obj);
    }

    fn emit_asr_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        self.raw_shift_right(conds.clone(), dst.clone(), src.clone());

        let t0_obj = self.obj_tmp0.clone();
        let lt_zero_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(dst.clone(), 0));
            c };
        let t0 = Register::Spec(t0_obj.clone());

        let sign_bits_tgt = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            scores: {
                let mut s = HashMap::new();
                s.insert(t0_obj, Interval::Min(-1));
                s },
            ..Selector::entity()
        });
        // if dst < 0 execute-in-bitwise-entities: computer t0 += entity BitComponent
        let block = make_cmd_block(
            self.selector.clone(), lt_zero_conds, Execute(
                sign_bits_tgt, REL_ZERO,
                Box::new(self.make_op_cmd_rx(
                    t0.clone(), PlayerOp::Add,
                    self.tgt_bit_one.clone(), self.obj_bit_comp.clone()))));
        self.emit(Complete(block));

        // copy computer t0 to dst
        self.emit_rr(&conds, dst, PlayerOp::Asn, &t0);
    }

    fn emit_lsr_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        self.raw_shift_right(conds.clone(), dst.clone(), src.clone());

        let tmp0 = self.obj_tmp0.clone();
        let mut lt_zero_conds = conds.clone();
        lt_zero_conds.push(Cond::lt(dst.clone(), 0));
        let t0 = Register::Spec(tmp0.clone());

        let high_bit_tgt = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            count: Some(1),
            scores: {
                let mut s = HashMap::new();
                s.insert(tmp0, Interval::Bounded(-1, -1));
                s },
            ..Selector::entity()
        });
        // if dst < 0 computer t0 += entity[high-bit] BitComponent
        let block = make_cmd_block(
            self.selector.clone(), lt_zero_conds, self.make_op_cmd_rx(
                t0.clone(), PlayerOp::Add,
                high_bit_tgt, self.obj_bit_comp.clone()));
        self.emit(Complete(block));

        // copy computer t0 to dst
        self.emit_rr(&conds, dst, PlayerOp::Asn, &t0);
    }

    fn emit_lsl_rr(&mut self, conds: &Vec<Cond>, dst: &Register, src: &Register) {
        self.uses_bitwise = true;

        self.activate_bitwise_entities(conds.clone(), src.clone());

        let tmp0 = self.obj_tmp0.clone();
        let two_reg = Register::Spec(self.obj_two.clone());

        let active_bit_tgt = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            scores: {
                let mut s = HashMap::new();
                s.insert(tmp0, Interval::Min(0));
                s },
            ..Selector::entity()
        });
        // execute-in-bitwise-entities: dst *= TWO
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), Execute(
                active_bit_tgt, REL_ZERO,
                Box::new(self.make_op_cmd_rr(
                    dst.clone(), PlayerOp::Mul, two_reg))));
        self.emit(Complete(block));
    }

    fn emit_srng(
        &mut self, conds: Vec<Cond>, dst: Register, test: Register,
        min: Option<i32>, max: Option<i32>)
    {
        let one_conds = {
            let mut c = conds.clone();
            if let Some(interval) = Interval::new(min, max) {
                c.push(Cond::new(test, interval));
            } else {
                // TODO: Issue a warning.
            }
            c };

        self.emit_rset(&conds, &dst, 0);
        self.emit_rset(&one_conds, &dst, 1);
    }

    fn emit_br_l(&mut self, conds: Vec<Cond>, label: String) {
        // FIXME: formalize unique label generation.
        let cont_label = format!("{}_cont_{}", label, self.unique);
        self.unique += 1;

        let test_reg = Register::Spec("TEST".to_string());
        self.emit_rset(&vec!(), &test_reg, 0);
        self.emit_rset(&conds, &test_reg, 1);

        let selector = self.selector.clone();
        let test_reg_copy = test_reg.clone();
        self.emit(Pending(label, Box::new(move |extent| {
            match extent {
                Extent::Empty => {
                    panic!("oh no!");
                }
                Extent::MinMax(min, max) => {
                    make_cmd_block(
                        selector, vec!(Cond::eq(test_reg_copy, 1)),
                        Fill(
                            min.as_abs(), max.as_abs(),
                            "minecraft:redstone_block".to_string(),
                            None, None, None))
                }
            }
        })));

        let selector = self.selector.clone();
        let test_reg_copy = test_reg.clone();
        self.emit(Pending(cont_label.clone(), Box::new(move |extent| {
            match extent {
                Extent::Empty => {
                    panic!("oh no!");
                }
                Extent::MinMax(min, max) => {
                    make_cmd_block(
                        selector, vec!(Cond::eq(test_reg_copy, 0)),
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

    fn emit_br_r(&mut self, conds: Vec<Cond>, reg: Register) {
        let block = make_cmd_block(
            self.selector.clone(), conds,
            Say(format!("FIXME: emit BrR to {}", reg_name(reg))));
        self.emit(Complete(block));
        self.emit(Terminal);
    }

    fn expand_bits(&mut self, conds: Vec<Cond>, reg: Register, bit_obj: Objective) {
        let tgt_all = self.tgt_bit_all.clone();
        let lt_zero_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(reg.clone(), 0));
            c
        };

        // Set all bit entities' bit_obj to the value to be expanded, reg.
        // Like this: [11, 11, 11, 11]
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            self.make_op_cmd_xr(
                tgt_all.clone(), bit_obj.clone(), PlayerOp::Asn, reg.clone()));
        self.emit(Complete(block));

        // If reg is negative, flip the sign of all temp values. This causes the
        // high bit to always end up zero, so that is handled later.
        let min_reg = Register::Spec(self.obj_min.clone());
        let block = make_cmd_block(
            self.selector.clone(), lt_zero_conds.clone(), self.make_op_cmd_xr(
                tgt_all.clone(), bit_obj.clone(), PlayerOp::Sub, min_reg));
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

        // If reg is negative, set the high bit to one.
        let tgt_high = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            scores: {
                let mut s = HashMap::new();
                s.insert(self.obj_bit_num.clone(), Interval::Bounded(31, 31));
                s },
            ..Selector::entity()
        });
        let block = make_cmd_block(
            self.selector.clone(), lt_zero_conds.clone(),
            players::set(tgt_high, bit_obj, 1, None));
        self.emit(Complete(block));
    }

    fn accum_bits(&mut self, conds: Vec<Cond>, dst: Register, bit_obj: String) {
        // Multiply all bit entities' bit_obj by their bit component.
        // Like this: [1, 0, 1, 1] * [8, 4, 2, 1] = [8, 0, 2, 1]
        let bit_comp = self.obj_bit_comp.clone();
        self.bit_vec_op(conds.clone(), bit_obj.clone(), PlayerOp::Mul, bit_comp);

        // Zero the dst register.
        self.emit_rset(&conds, &dst, 0);

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
        // execute @e[team=BITWISE] ~ ~ ~
        //   scoreboard players operation
        //     @e[team=BITWISE,c=1] lhs *= @e[team=BITWISE,c=1] rhs
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
        let tmp0 = self.obj_tmp0.clone();

        // SIMD copy bitwise entities' BitNumber to tmp0
        self.bit_vec_op(conds.clone(), tmp0.clone(), PlayerOp::Asn, bit_num);

        // Vector-scalar remove 32 from bitwise entities' tmp0
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(),
            players::remove(self.tgt_bit_all.clone(), tmp0.clone(), 32, None));
        self.emit(Complete(block));

        // Vector-scalar add shift amount to bitwise entities' tmp0.
        // This makes all active shifters greater than or equal to zero.
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), self.make_op_cmd_xr(
                self.tgt_bit_all.clone(), tmp0.clone(), PlayerOp::Add, amount));
        self.emit(Complete(block));
    }

    fn raw_shift_right(&mut self, conds: Vec<Cond>, dst: Register, src: Register) {
        let tmp0 = self.obj_tmp0.clone();
        let mut lt_zero_conds = conds.clone();
        lt_zero_conds.push(Cond::lt(dst.clone(), 0));

        let t0 = Register::Spec(tmp0.clone());
        let two_reg = Register::Spec(self.obj_two.clone());
        let min_reg = Register::Spec(self.obj_min.clone());

        // Copy to t0
        self.emit_rr(&conds, &t0, PlayerOp::Asn, &dst);

        // if dst < 0, t0 -= i32::MIN
        self.emit_rr(&lt_zero_conds, &t0, PlayerOp::Sub, &min_reg);

        self.activate_bitwise_entities(conds.clone(), src);

        let active_bit_tgt = Target::Sel(Selector {
            team: Some(SelectorTeam::On(self.team_bit.clone())),
            scores: {
                let mut s = HashMap::new();
                s.insert(tmp0, Interval::Min(0));
                s },
            ..Selector::entity()
        });
        // execute-in-active-bitwise-entities: divide computer t0 by TWO
        let block = make_cmd_block(
            self.selector.clone(), conds.clone(), Execute(
                active_bit_tgt, REL_ZERO,
                Box::new(self.make_op_cmd_rr(
                    t0.clone(), PlayerOp::Div, two_reg))));
        self.emit(Complete(block));
    }

    fn emit_udiv(&mut self, conds: Vec<Cond>, dst: Register, src: Register) {
        let t0 = Register::Spec(self.obj_tmp0.clone());
        let t1 = Register::Spec(self.obj_tmp1.clone());
        let t2 = Register::Spec(self.obj_tmp2.clone());
        let min_reg = Register::Spec(self.obj_min.clone());
        let two_reg = Register::Spec(self.obj_two.clone());

        let src_pos_conds = {
            let mut c = conds.clone();
            c.push(Cond::ge(src.clone(), 0));
            c };

        let neg_pos_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(t0.clone(), 0));
            c.push(Cond::ge(src.clone(), 0));
            c };

        self.emit_rr(&conds, &t0, PlayerOp::Asn, &dst);

        // If needed, adjust dst to fit in 31 bits
        // logical shift right 1
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Add, &min_reg);
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Div, &two_reg);
        self.emit_radd(&neg_pos_conds, &dst, 1 << 30);
        // Save the current value of dst, so we can get the remainder later.
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Asn, &dst);

        // Perform the 31-bit by 31-bit division
        self.emit_rr(&src_pos_conds, &dst, PlayerOp::Div, &src);

        // If dst was adjusted to 31 bits, adjust the result to 32 bits and
        // perform the final round of division manually.
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Mul, &two_reg);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Rem, &src);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Mul, &two_reg);
        self.emit_rr(&neg_pos_conds, &t2, PlayerOp::Asn, &t0);
        self.emit_rr(&neg_pos_conds, &t2, PlayerOp::Add, &min_reg);
        self.emit_rr(&neg_pos_conds, &t2, PlayerOp::Rem, &two_reg);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Add, &t2);
        self.emit_rr(&neg_pos_conds, &t2, PlayerOp::Asn, &t1);
        self.emit_rr(&neg_pos_conds, &t2, PlayerOp::Sub, &src);
        self.emit_radd(&neg_pos_conds, &dst, 1);
        self.emit_rsub(&{
            let mut c = neg_pos_conds.clone();
            c.push(Cond::ge(t1.clone(), 0));
            c.push(Cond::lt(t2.clone(), 0));
            c }, &dst, 1);

        let src_neg_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(src.clone(), 0));
            c };

        // If src's high bit is set (negative), src dominates.
        self.emit_rset(&src_neg_conds, &dst, 0);
        //// Unless dst's (now in t0) high bit is set, and dst is larger.
        self.emit_rr(&src_neg_conds, &t1, PlayerOp::Asn, &t0);
        self.emit_rr(&src_neg_conds, &t1, PlayerOp::Sub, &src);
        self.emit_rset(&{
            let mut c = src_neg_conds.clone();
            c.push(Cond::lt(t0.clone(), 0));
            c.push(Cond::ge(t1.clone(), 0));
            c }, &dst, 1);
    }

    fn emit_urem(&mut self, conds: Vec<Cond>, dst: Register, src: Register) {
        let t0 = Register::Spec(self.obj_tmp0.clone());
        let t1 = Register::Spec(self.obj_tmp1.clone());
        let min_reg = Register::Spec(self.obj_min.clone());
        let two_reg = Register::Spec(self.obj_two.clone());

        // Save the original value of dst for later comparisons.
        self.emit_rr(&conds, &t0, PlayerOp::Asn, &dst);

        let neg_neg_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(dst.clone(), 0));
            c.push(Cond::lt(src.clone(), 0));
            c };

        // Calculate a potential remainder, but if it is negative put
        // things back like they were.  (This looks strange because dst
        // is checked twice in a row the same way, but its value
        // changes in the meantime.)
        self.emit_rr(&neg_neg_conds, &dst, PlayerOp::Sub, &src);
        self.emit_rr(&neg_neg_conds, &dst, PlayerOp::Add, &src);

        let src_pos_conds = {
            let mut c = conds.clone();
            c.push(Cond::ge(src.clone(), 0));
            c };

        let neg_pos_conds = {
            let mut c = conds.clone();
            c.push(Cond::lt(t0.clone(), 0));
            c.push(Cond::ge(src.clone(), 0));
            c };

        // If needed, adjust dst to fit in 31 bits
        // logical shift right 1
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Add, &min_reg);
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Div, &two_reg);
        self.emit_radd(&neg_pos_conds, &dst, 1 << 30);

        // Perform the 31-bit by 31-bit remainder.
        self.emit_rr(&src_pos_conds, &dst, PlayerOp::Rem, &src);

        // If dst was adjusted to 31 bits, adjust the result to 32 bits
        // and perform the final round of division manually.
        // (There is room for simplification here, I think.  The last
        // four lines may be able to eliminate their use of t1, and
        // some of the operations that go with it.)
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Mul, &two_reg);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Asn, &t0);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Add, &min_reg);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Rem, &two_reg);
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Add, &t1);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Asn, &dst);
        self.emit_rr(&neg_pos_conds, &t1, PlayerOp::Sub, &src);
        self.emit_rr(&{
            let mut c = neg_pos_conds.clone();
            c.push(Cond::lt(t1.clone(), 0));
            c.push(Cond::ge(dst.clone(), 0));
            c }, &dst, PlayerOp::Add, &src);
        self.emit_rr(&neg_pos_conds, &dst, PlayerOp::Sub, &src);
    }

    fn add_success_count(&self, block: &mut Block, reg: Register) {
        let outs = vec!((CommandBlockOut::SuccessCount, reg));
        self.add_command_stats(
            block, make_command_stats(self.target.clone(), outs));
    }

    fn add_command_stats(&self, block: &mut Block, stats: Nbt)
    {
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
    let cmd_str = commands::escape(&final_cmd.to_string()[..]);
    nbt.insert("Command".to_string(), Nbt::String(cmd_str));
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

fn make_command_stats(
    target: Target, outs: Vec<(CommandBlockOut, Register)>) -> Nbt
{
    let mut stats = NbtCompound::new();
    for (out, reg) in outs.into_iter() {
        stats.insert(out.selector().to_string(), Nbt::String(target.to_string()));
        stats.insert(out.objective().to_string(), Nbt::String(reg_name(reg)));
    }
    Nbt::Compound(stats)
}

impl<S : Iterator<Item=Statement>> Iterator for Assembler<S> {
    type Item = AssembledItem;

    fn next(&mut self) -> Option<AssembledItem> {
        if self.done {
            return None;
        }

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
