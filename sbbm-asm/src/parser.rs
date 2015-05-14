use ast::{CommandBlockOut, Cond, Op, Register, Statement};
use ast::Op::*;
use ast::Statement::*;
use commands::{Objective, Target};
use lexer::{Lexer, SpannedToken, Token};
use lexer::Token::*;
use types::Interval;

use std::collections::VecDeque;
use std::fmt::Display;
use std::str::FromStr;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: VecDeque<SpannedToken>,
}

type ParseResult<T> = Result<T, String>;

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
            lookahead: VecDeque::new(),
        }
    }

    fn cur(&mut self) -> SpannedToken {
        if self.lookahead.is_empty() {
            match self.lexer.next() {
                Ok(token) => self.lookahead.push_back(token),
                Err(msg) => panic!(msg),
            }
        }
        // FIXME: This is inefficient, but I am not good enough with lifetimes
        // yet to do better.
        self.lookahead[0].clone()
    }

    fn accept(&mut self) -> SpannedToken {
        match self.lookahead.pop_front() {
            Some(token) => token,
            None => panic!("nothing to accept"),
        }
    }

    fn expect_tok(&mut self, token: Token) -> ParseResult<()> {
        if self.cur().item == token {
            self.accept();
            Ok(())
        } else {
            // FIXME: Switch to Display instead of Debug
            Err(format!("expected {:?} but found {:?}", token, self.cur()))
        }
    }

    pub fn parse_program(&mut self) -> Vec<Statement> {
        let stmts = self.parse_items();
        // FIXME: try!
        self.expect_tok(Eof).unwrap();
        stmts
    }

    fn parse_items(&mut self) -> Vec<Statement> {
        let mut stmts = vec!();
        while self.cur().item != Eof {
            match self.parse_item() {
                Some(stmt) => stmts.push(stmt),
                None => (),
            }
        }
        stmts
    }

    fn parse_item(&mut self) -> Option<Statement> {
        match self.cur().item {
            Label(label) => {
                self.accept();
                Some(LabelStmt(label[..label.len()-1].to_string()))
            }
            Ident(_) | Raw(_, _) => {
                let op = self.parse_op().unwrap();
                Some(Instr(vec!(), op))
            }
            LBrace => {
                let conds = self.parse_conds().unwrap();
                let op = self.parse_op().unwrap();
                Some(Instr(conds, op))
            }
            Meta(_) => {
                loop {
                    self.accept();
                    if let Newline(_) = self.cur().item {
                        break;
                    }
                }
                self.accept();
                None
            }
            Newline(_) => {
                self.accept();
                None
            }
            _ => panic!("unexpected token: {:?}", self.cur()),
        }
    }

    fn parse_any_reg(&mut self) -> ParseResult<Register> {
        if let Some(reg) = try_parse_reg(self.cur().item) {
            self.accept();
            Ok(reg)
        } else {
            // FIXME: Move to Display, rather than Debug
            Err(format!("expected register but found {:?}", self.cur()))
        }
    }

    fn parse_any_target(&mut self) -> ParseResult<Target> {
        match self.cur().item {
            Ident(raw_sel) | Selector(raw_sel) => {
                self.accept();
                Ok(Target::Raw(raw_sel))
            }
            _ => {
                // FIXME: Move to Display, rather than Debug
                Err(format!("expected selector but found {:?}", self.cur()))
            }
        }
    }

    fn parse_int<T>(&mut self) -> ParseResult<T>
        where T : FromStr, T::Err : Display
    {
        match self.cur().item {
            LitInt(s) => {
                // No need to handle errors: the lexer has already ensured that
                // s contains a valid int prefixed with #.
                self.accept();
                if let Ok(value) = s[1..].parse::<T>() {
                    Ok(value)
                } else {
                    // FIXME: It would be nice to just call .unwrap, but I can't
                    // seem to get the type constraints right for it.
                    unreachable!();
                }
            }
            // FIXME: Print with Display, rather than Debug
            _ => Err(format!("expected integer but found {:?}", self.cur())),
        }
    }

    fn parse_opt_int<T>(&mut self) -> ParseResult<Option<T>>
        where T : FromStr, T::Err : Display
    {
        let cur = self.cur();

        if let Ok(value) = self.parse_int() {
            Ok(Some(value))
        } else if (cur.item == Star) {
            self.accept();
            Ok(None)
        } else {
            // FIXME: Print with Display, rather than Debug
            Err(format!("expected optional integer but found {:?}", self.cur()))
        }
    }

    fn parse_objective(&mut self) -> ParseResult<String> {
        match self.cur().item {
            Ident(obj) => { self.accept(); Ok(obj) }
            // FIXME: Print with Display, rather than Debug
            _ => Err(format!("expected objective but found {:?}", self.cur())),
        }
    }

    fn parse_conds(&mut self) -> ParseResult<Vec<Cond>> {
        let mut conds = vec!();
        while self.cur().item == LBrace {
            self.accept();
            let reg = try!(self.parse_any_reg());
            try!(self.expect_tok(Comma));
            let min = try!(self.parse_opt_int());
            try!(self.expect_tok(Comma));
            let max = try!(self.parse_opt_int());
            try!(self.expect_tok(RBrace));
            if let Some(interval) = Interval::new(min, max) {
                conds.push(Cond::new(reg, interval));
            } else {
                // TODO: Issue warning.
            }
        }
        Ok(conds)
    }

    fn parse_op(&mut self) -> ParseResult<Op> {
        match self.cur().item {
            Ident(mnemonic) => {
                let res = match &mnemonic[..] {
                    "ldr" => self.parse_ldr(),
                    m @ "add" => self.parse_addsub(m, AddRR, AddRI, AddXI, AddXR),
                    m @ "sub" => self.parse_addsub(m, SubRR, SubRI, SubXI, SubXR),
                    m @ "and" => self.parse_instr_rr(m, AndRR),
                    m @ "orr" => self.parse_instr_rr(m, OrrRR),
                    m @ "eor" => self.parse_instr_rr(m, EorRR),
                    m @ "asr" => self.parse_instr_rr(m, AsrRR),
                    m @ "lsr" => self.parse_instr_rr(m, LsrRR),
                    m @ "lsl" => self.parse_instr_rr(m, LslRR),
                    "mov" => self.parse_mov(),
                    m @ "mul" => self.parse_instr_rr(m, MulRR),
                    m @ "sdiv" => self.parse_instr_rr(m, SdivRR),
                    m @ "udiv" => self.parse_instr_rr(m, UdivRR),
                    m @ "srem" => self.parse_instr_rr(m, SremRR),
                    m @ "urem" => self.parse_instr_rr(m, UremRR),
                    m @ "srng" => self.parse_rng::<_, i32>(m, Srng),
                    m @ "urng" => self.parse_rng::<_, u32>(m, Urng),
                    m @ "b" => self.parse_branch(m, BrR, BrL),
                    m @ "bl" => self.parse_branch(m, BrLnkR, BrLnkL),
                    "halt" => self.parse_halt(),
                    _ => Err(format!("unknown mnemonic: {:?}", self.cur())),
                };
                res
            }
            Raw(mods, raw) => {
                self.accept();
                let mut raw_outs = vec!();
                for (c, reg) in mods.into_iter() {
                    let m = match c {
                        's' => CommandBlockOut::SuccessCount,
                        'b' => CommandBlockOut::AffectedBlocks,
                        'e' => CommandBlockOut::AffectedEntities,
                        'i' => CommandBlockOut::AffectedItems,
                        'q' => CommandBlockOut::QueryResult,
                        _ => panic!(
                            "unknown raw modifier: {} in {:?}",
                            c, self.cur())
                    };
                    raw_outs.push((m, try_parse_reg(reg).unwrap()));
                }
                Ok(RawCmd(raw_outs, raw))
            }
            // FIXME: Move to Display instead of Debug.
            _ => Err(format!("expected mnemonic but found {:?}", self.cur())),
        }
    }

    fn parse_instr_rr<F>(&mut self, mnemo: &str, op: F) -> ParseResult<Op>
        where F : FnOnce(Register, Register) -> Op
    {
        try!(self.expect_tok(Ident(mnemo.to_string())));
        let dst = try!(self.parse_any_reg());
        try!(self.expect_tok(Comma));
        let src = try!(self.parse_any_reg());
        Ok(op(dst, src))
    }

    fn parse_ldr(&mut self) -> ParseResult<Op> {
        try!(self.expect_tok(Ident("ldr".to_string())));
        let dst = try!(self.parse_any_reg());
        try!(self.expect_tok(Comma));

        match self.cur().item {
            LBracket => {
                self.accept();
                let src = try!(self.parse_any_reg());
                try!(self.expect_tok(RBracket));
                Ok(LdrRR(dst, src))
            }
            LabelRef(lblref) => {
                let src = lblref[1..].to_string();
                self.accept();
                Ok(LdrRL(dst, src))
            }
            // FIXME: Do better with this error message.
            _ => Err(format!("invalid ldr format {:?}", self.cur())),
        }
    }

    fn parse_addsub<RR, RI, XI, XR>(
        &mut self, mnemo: &str, rr: RR, ri: RI, xi: XI, xr: XR) -> ParseResult<Op>
        where RR : FnOnce(Register, Register) -> Op,
              RI : FnOnce(Register, i32) -> Op,
              XI : FnOnce(Target, Objective, i32, Register) -> Op,
              XR : FnOnce(Target, Objective, Register, Register) -> Op
    {
        try!(self.expect_tok(Ident(mnemo.to_string())));

        if let Ok(dst) = self.parse_any_reg() {
            try!(self.expect_tok(Comma));
            if let Ok(src) = self.parse_any_reg() {
                Ok(rr(dst, src))
            } else if let Ok(imm) = self.parse_int() {
                Ok(ri(dst, imm))
            } else {
                unimplemented!();
            }
        } else if let Ok(target) = self.parse_any_target() {
            try!(self.expect_tok(Comma));
            let obj = try!(self.parse_objective());
            try!(self.expect_tok(Comma));

            if let Ok(imm) = self.parse_int() {
                try!(self.expect_tok(Comma));
                let out_reg = try!(self.parse_any_reg());
                Ok(xi(target, obj, imm, out_reg))
            } else if let Ok(reg) = self.parse_any_reg() {
                try!(self.expect_tok(Comma));
                let out_reg = try!(self.parse_any_reg());
                Ok(xr(target, obj, reg, out_reg))
            } else {
                // FIXME: Print with Display rather than Debug.
                Err(format!(
                    "expected register or immediate but found {:?}",
                    self.cur()))
            }
        } else {
            // FIXME: Print with Display rather than Debug.
            Err(format!(
                "expected register or target but found {:?}", self.cur()))
        }
    }

    fn parse_mov(&mut self) -> ParseResult<Op> {
        try!(self.expect_tok(Ident("mov".to_string())));

        if let Ok(dst) = self.parse_any_reg() {
            try!(self.expect_tok(Comma));
            if let Ok(src) = self.parse_any_reg() {
                Ok(MovRR(dst, src))
            } else if let Ok(imm) = self.parse_int() {
                Ok(MovRI(dst, imm))
            } else if let Ok(target) = self.parse_any_target() {
                try!(self.expect_tok(Comma));
                let obj = try!(self.parse_objective());
                Ok(MovRX(dst, target, obj))
            } else {
                // FIXME: Print with Display rather than Debug.
                Err(format!(
                    "expected register, immediate, or selector but found {:?}",
                    self.cur()))
            }
        } else if let Ok(target) = self.parse_any_target() {
            try!(self.expect_tok(Comma));
            let obj = try!(self.parse_objective());
            try!(self.expect_tok(Comma));

            if let Ok(src) = self.parse_any_reg() {
                try!(self.expect_tok(Comma));
                let out_reg = try!(self.parse_any_reg());
                Ok(MovXR(target, obj, src, out_reg))
            } else if let Ok(imm) = self.parse_int() {
                try!(self.expect_tok(Comma));
                let out_reg = try!(self.parse_any_reg());
                Ok(MovXI(target, obj, imm, out_reg))
            } else {
                // FIXME: Print with Display rather than Debug.
                Err(format!(
                    "expected register or immediate but found {:?}",
                    self.cur()))
            }
        } else {
            // FIXME: Print with Display rather than Debug.
            Err(format!(
                "expected register or target but found {:?}",
                self.cur()))
        }
    }

    fn parse_rng<F, T>(&mut self, mnemo: &str, op: F) -> ParseResult<Op>
        where F : FnOnce(Register, Register, Option<T>, Option<T>) -> Op,
              T : FromStr, T::Err : Display
    {
        try!(self.expect_tok(Ident(mnemo.to_string())));
        let dst = try!(self.parse_any_reg());
        try!(self.expect_tok(Comma));
        let reg = try!(self.parse_any_reg());
        try!(self.expect_tok(Comma));
        let min = try!(self.parse_opt_int());
        try!(self.expect_tok(Comma));
        let max = try!(self.parse_opt_int());

        Ok(op(dst, reg, min, max))
    }

    fn parse_branch<R, L>(
        &mut self, mnemo: &str, regop: R, lblop: L) -> ParseResult<Op>
        where R : FnOnce(Register) -> Op,
    L : FnOnce(String) -> Op
    {
        try!(self.expect_tok(Ident(mnemo.to_string())));

        if let Ok(reg) = self.parse_any_reg() {
            Ok(regop(reg))
        } else {
            match self.cur().item {
                LabelRef(label) => {
                    self.accept();
                    Ok(lblop(label[1..].to_string()))
                }
                _ => {
                    // FIXME: Print with Display, rather than Debug.
                    let msg = format!(
                        "expected register or label but found: {:?}",
                        self.cur());
                    Err(msg)
                }
            }
        }
    }

    fn parse_halt(&mut self) -> ParseResult<Op> {
        try!(self.expect_tok(Ident("halt".to_string())));
        Ok(Halt)
    }
}

fn try_parse_reg(token: Token) -> Option<Register> {
    match token {
        GenReg(reg) => {
            let num = reg[1..].parse::<i32>().unwrap();
            Some(Register::Gen(num))
        }
        PredReg(reg) => {
            let num = reg[1..].parse::<i32>().unwrap();
            Some(Register::Pred(num))
        }
        SpecReg(reg) => {
            Some(Register::Spec(reg))
        }
        _ => None,
    }
}

#[test]
fn test_label() {
    let mut parser = Parser::new(Lexer::mem("foo:"));
    assert_eq!(
        vec!(LabelStmt("foo".to_string())),
        parser.parse_program());
}

#[test]
fn test_cond_op() {
    let mut parser = Parser::new(Lexer::mem("{p0, #0, #0} ldr r0, [r1]"));
    assert_eq!(
        vec!(Instr(
            vec!(Cond::eq(Register::Pred(0), 0)),
            LdrRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_ldr_rr() {
    let mut parser = Parser::new(Lexer::mem("ldr r0, [r1]"));
    assert_eq!(
        vec!(Instr(vec!(), LdrRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_ldr_rl() {
    let mut parser = Parser::new(Lexer::mem("ldr r0, =foo"));
    assert_eq!(
        vec!(Instr(vec!(), LdrRL(Register::Gen(0), "foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_add_rr() {
    let mut parser = Parser::new(Lexer::mem("add r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), AddRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_add_xi() {
    let mut parser = Parser::new(Lexer::mem("add @a, foo, #10, r0"));
    assert_eq!(
        vec!(Instr(vec!(), AddXI(
            Target::Raw("@a".to_string()), "foo".to_string(), 10, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_add_xr() {
    let mut parser = Parser::new(Lexer::mem("add @a, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), AddXR(
            Target::Raw("@a".to_string()), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_sub_rr() {
    let mut parser = Parser::new(Lexer::mem("sub r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), SubRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_sub_xi() {
    let mut parser = Parser::new(Lexer::mem("sub @a, foo, #10, r0"));
    assert_eq!(
        vec!(Instr(vec!(), SubXI(
            Target::Raw("@a".to_string()), "foo".to_string(), 10, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_sub_xr() {
    let mut parser = Parser::new(Lexer::mem("sub @a, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), SubXR(
            Target::Raw("@a".to_string()), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_rr() {
    let mut parser = Parser::new(Lexer::mem("mov r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), MovRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_ri() {
    let mut parser = Parser::new(Lexer::mem("mov r0, #37"));
    assert_eq!(
        vec!(Instr(vec!(), MovRI(Register::Gen(0), 37))),
        parser.parse_program());
}

#[test]
fn test_mov_rx() {
    let mut parser = Parser::new(Lexer::mem("mov r0, @r, foo"));
    assert_eq!(
        vec!(Instr(vec!(), MovRX(Register::Gen(0), Target::Raw("@r".to_string()), "foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_mov_xr() {
    let mut parser = Parser::new(Lexer::mem("mov @r, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), MovXR(Target::Raw("@r".to_string()), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_xi() {
    let mut parser = Parser::new(Lexer::mem("mov @r, foo, #15, r0"));
    assert_eq!(
        vec!(Instr(vec!(), MovXI(Target::Raw("@r".to_string()), "foo".to_string(), 15, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_mul() {
    let mut parser = Parser::new(Lexer::mem("mul r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), MulRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_sdiv() {
    let mut parser = Parser::new(Lexer::mem("sdiv r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), SdivRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_udiv() {
    let mut parser = Parser::new(Lexer::mem("udiv r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), UdivRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_br_r() {
    let mut parser = Parser::new(Lexer::mem("b lr"));
    assert_eq!(
        vec!(Instr(vec!(), BrR(Register::Spec("lr".to_string())))),
        parser.parse_program());
}

#[test]
fn test_br_l() {
    let mut parser = Parser::new(Lexer::mem("b =foo"));
    assert_eq!(
        vec!(Instr(vec!(), BrL("foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_br_lnk_r() {
    let mut parser = Parser::new(Lexer::mem("bl lr"));
    assert_eq!(
        vec!(Instr(vec!(), BrLnkR(Register::Spec("lr".to_string())))),
        parser.parse_program());
}

#[test]
fn test_srng() {
    let mut parser = Parser::new(Lexer::mem("srng p0, r0, #0, #1"));
    assert_eq!(
        vec!(Instr(vec!(), Srng(Register::Pred(0), Register::Gen(0), Some(0), Some(1)))),
        parser.parse_program());
}

#[test]
fn test_urng() {
    let mut parser = Parser::new(Lexer::mem("urng p0, r0, #0, *"));
    assert_eq!(
        vec!(Instr(vec!(), Urng(Register::Pred(0), Register::Gen(0), Some(0), None))),
        parser.parse_program());
}

#[test]
fn test_halt() {
    let mut parser = Parser::new(Lexer::mem("halt"));
    assert_eq!(vec!(Instr(vec!(), Halt)), parser.parse_program());
}

#[test]
fn test_raw_cmd() {
    let mut parser = Parser::new(Lexer::mem("raw foo bar baz"));
    assert_eq!(
        vec!(Instr(vec!(), RawCmd(vec!(), "foo bar baz".to_string()))),
        parser.parse_program());
}

#[test]
fn test_raw_cmd_outs() {
    use ast::CommandBlockOut::*;
    let mut parser = Parser::new(Lexer::mem("raw~siq r0, r1, r2, foo bar baz"));
    let outs = vec!(
        (SuccessCount, Register::Gen(0)),
        (AffectedItems, Register::Gen(1)),
        (QueryResult, Register::Gen(2)));
    assert_eq!(
        vec!(Instr(vec!(), RawCmd(outs, "foo bar baz".to_string()))),
        parser.parse_program());
}

#[test]
fn test_raw_cmd_cond() {
    let mut parser = Parser::new(Lexer::mem("{p0, #1, #1} raw foo bar baz"));
    assert_eq!(
        vec!(Instr(
            vec!(Cond::eq(Register::Pred(0), 1)),
            RawCmd(vec!(), "foo bar baz".to_string()))),
        parser.parse_program());
}
