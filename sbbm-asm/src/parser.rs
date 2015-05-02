use std::collections::VecDeque;
use std::fmt::Display;
use std::str::FromStr;
use ast::Cond;
use ast::Op;
use ast::Op::*;
use ast::Register;
use ast::Statement;
use ast::Statement::*;
use lexer::Lexer;
use lexer::SpannedToken;
use lexer::Token;
use lexer::Token::*;

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

    fn expect_tok(&mut self, token: Token) {
        if self.cur().item != token {
            // FIXME: Don't panic.
            // FIXME: Switch to Display instead of Debug
            panic!(format!("expected {:?} but found {:?}", token, self.cur()))
        }
        self.accept();
    }

    pub fn parse_program(&mut self) -> Vec<Statement> {
        let stmts = self.parse_items();
        self.expect_tok(Eof);
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
            Ident(_) => {
                let op = self.parse_op();
                Some(Instr(vec!(), op))
            }
            LBrace => {
                let conds = self.parse_conds();
                let op = self.parse_op();
                Some(Instr(conds, op))
            }
            Raw(raw) => {
                self.accept();
                Some(Instr(vec!(), RawCmd(raw)))
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
        match self.cur().item {
            GenReg(reg) => {
                // FIXME: try!
                let num = reg[1..].parse::<i32>().unwrap();
                self.accept();
                Ok(Register::Gen(num))
            }
            PredReg(reg) => {
                // FIXME: try!
                let num = reg[1..].parse::<i32>().unwrap();
                self.accept();
                Ok(Register::Pred(num))
            }
            SpecReg(reg) => {
                self.accept();
                Ok(Register::Spec(reg))
            }
            // FIXME: Move to Display, rather than Debug
            _ => Err(format!("expected register but found {:?}", self.cur())),
        }
    }

    fn parse_int<T>(&mut self) -> ParseResult<T>
        where T : FromStr, T::Err : Display
    {
        match self.cur().item {
            LitInt(s) => {
                // FIXME: try!
                match s[1..].parse::<T>() {
                    Ok(num) => {
                        self.accept();
                        Ok(num)
                    }
                    Err(e) => Err(format!("{}", e)),
                }
            }
            // FIXME: Move to Display, rather than Debug
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
            // FIXME: Move to Display, rather than Debug
            Err(format!("expected optional integer but found {:?}", self.cur()))
        }
    }

    fn parse_objective(&mut self) -> ParseResult<String> {
        match self.cur().item {
            Ident(obj) => { self.accept(); Ok(obj) }
            // FIXME: Move to Display, rather than Debug
            _ => Err(format!("expected objective but found {:?}", self.cur())),
        }
    }

    fn parse_conds(&mut self) -> Vec<Cond> {
        let mut conds = vec!();
        while self.cur().item == LBrace {
            self.accept();
            // FIXME: try!
            let reg = self.parse_any_reg().unwrap();
            self.expect_tok(Comma);
            // FIXME: try!
            let min = self.parse_opt_int().unwrap();
            self.expect_tok(Comma);
            // FIXME: try!
            let max = self.parse_opt_int().unwrap();
            self.expect_tok(RBrace);
            conds.push(Cond { reg: reg, min: min, max: max });
        }
        conds
    }

    fn parse_op(&mut self) -> Op {
        match self.cur().item {
            Ident(mnemonic) => {
                match &mnemonic[..] {
                    "ldr" => self.parse_ldr(),
                    "add" => self.parse_add(),
                    "sub" => self.parse_sub(),
                    "mov" => self.parse_mov(),
                    "mul" => self.parse_mul(),
                    "sdiv" => self.parse_sdiv(),
                    "udiv" => self.parse_udiv(),
                    "srng" => self.parse_srng(),
                    "urng" => self.parse_urng(),
                    "b" => self.parse_b(),
                    "bl" => self.parse_bl(),
                    "halt" => self.parse_halt(),
                    // FIXME: Don't panic, return ParseResult
                    _ => panic!("unknown mnemonic: {}", mnemonic),
                }
            }
            Raw(raw) => {
                self.accept();
                RawCmd(raw)
            }
            // FIXME: Move to Display instead of Debug.
            // FIXME: Return ParseResult
            _ => panic!("expected mnemonic but found {:?}", self.cur()),
        }
    }

    fn parse_ldr(&mut self) -> Op {
        self.expect_tok(Ident("ldr".to_string()));
        // FIXME: try!
        let dst = self.parse_any_reg().unwrap();
        self.expect_tok(Comma);

        match self.cur().item {
            LBracket => {
                self.accept();
                let src = self.parse_any_reg().unwrap();
                self.expect_tok(RBracket);
                Op::LdrRR(dst, src)
            }
            LabelRef(lblref) => {
                let src = lblref[1..].to_string();
                self.accept();
                Op::LdrRL(dst, src)
            }
            // FIXME: Don't panic.  Do better with the error message.
            _ => panic!("invalid ldr format {:?}", self.cur()),
        }
    }

    // FIXME: Unify the handling of parse_add and parse_sub?
    fn parse_add(&mut self) -> Op {
        self.expect_tok(Ident("add".to_string()));

        if let Ok(dst) = self.parse_any_reg() {
            self.expect_tok(Comma);
            if let Ok(src) = self.parse_any_reg() {
                AddRR(dst, src)
            } else {
                unimplemented!();
            }
        } else {
            match self.cur().item {
                Selector(sel) => {
                    self.accept();
                    self.expect_tok(Comma);
                    // FIXME: try!
                    let obj = self.parse_objective().unwrap();
                    self.expect_tok(Comma);
                    if let Ok(imm) = self.parse_int() {
                        // FIXME: try!
                        self.expect_tok(Comma);
                        // FIXME: try!
                        let out_reg = self.parse_any_reg().unwrap();
                        AddXI(sel, obj, imm, out_reg)
                    } else if let Ok(reg) = self.parse_any_reg() {
                        // FIXME: try!
                        self.expect_tok(Comma);
                        // FIXME: try!
                        let out_reg = self.parse_any_reg().unwrap();
                        AddXR(sel, obj, reg, out_reg)
                    } else {
                        // FIXME: Don't panic.
                        panic!("expected register or immediate but found {:?}", self.cur());
                    }
                }
                // FIXME: Don't panic.
                _ => panic!("expected selector but found {:?}", self.cur()),
            }
        }
    }

    fn parse_sub(&mut self) -> Op {
        self.expect_tok(Ident("sub".to_string()));

        if let Ok(dst) = self.parse_any_reg() {
            // FIXME: try!
            self.expect_tok(Comma);
            if let Ok(src) = self.parse_any_reg() {
                SubRR(dst, src)
            } else {
                unimplemented!();
            }
        } else {
            match self.cur().item {
                Selector(sel) => {
                    self.accept();
                    // FIXME: try!
                    self.expect_tok(Comma);
                    // FIXME: try!
                    let obj = self.parse_objective().unwrap();
                    // FIXME: try!
                    self.expect_tok(Comma);
                    if let Ok(imm) = self.parse_int() {
                        // FIXME: try!
                        self.expect_tok(Comma);
                        // FIXME: try!
                        let out_reg = self.parse_any_reg().unwrap();
                        SubXI(sel, obj, imm, out_reg)
                    } else if let Ok(reg) = self.parse_any_reg() {
                        // FIXME: try!
                        self.expect_tok(Comma);
                        // FIXME: try!
                        let out_reg = self.parse_any_reg().unwrap();
                        SubXR(sel, obj, reg, out_reg)
                    } else {
                        // FIXME: Don't panic.
                        panic!("expected register or immediate but found {:?}", self.cur());
                    }
                }
                // FIXME: Don't panic.
                _ => panic!("expected selector but found {:?}", self.cur()),
            }
        }
    }

    fn parse_mov(&mut self) -> Op {
        self.expect_tok(Ident("mov".to_string()));

        if let Ok(dst) = self.parse_any_reg() {
            // FIXME: try!
            self.expect_tok(Comma);
            if let Ok(src) = self.parse_any_reg() {
                MovRR(dst, src)
            } else if let Ok(imm) = self.parse_int() {
                MovRI(dst, imm)
            } else if let Selector(sel) = self.cur().item {
                self.accept();
                // FIXME: try!
                self.expect_tok(Comma);
                // FIXME: try!
                let obj = self.parse_objective().unwrap();
                MovRX(dst, sel, obj)
            } else {
                // FIXME: Don't panic.
                panic!("expected register, immediate, or selector but found {:?}", self.cur());
            }
        } else if let Selector(sel) = self.cur().item {
            self.accept();
            // FIXME: try!
            self.expect_tok(Comma);
            // FIXME: try!
            let obj = self.parse_objective().unwrap();
            // FIXME: try!
            self.expect_tok(Comma);

            if let Ok(src) = self.parse_any_reg() {
                // FIXME: try!
                self.expect_tok(Comma);
                // FIXME: try!
                let out_reg = self.parse_any_reg().unwrap();
                MovXR(sel, obj, src, out_reg)
            } else if let Ok(imm) = self.parse_int() {
                // FIXME: try!
                self.expect_tok(Comma);
                // FIXME: try!
                let out_reg = self.parse_any_reg().unwrap();
                MovXI(sel, obj, imm, out_reg)
            } else {
                panic!("expected register or immediate but found {:?}", self.cur());
            }
        } else {
            // FIXME: Don't panic.
            panic!("expected register or selector but found {:?}", self.cur());
        }
    }

    fn parse_rr(&mut self) -> ParseResult<(Register, Register)> {
        let dst = try!(self.parse_any_reg());
        // FIXME: try!
        self.expect_tok(Comma);
        let src = try!(self.parse_any_reg());
        Ok((dst, src))
    }

    fn parse_mul(&mut self) -> Op {
        self.expect_tok(Ident("mul".to_string()));
        // FIXME: try!
        let (dst, src) = self.parse_rr().unwrap();
        MulRR(dst, src)
    }

    fn parse_sdiv(&mut self) -> Op {
        self.expect_tok(Ident("sdiv".to_string()));
        // FIXME: try!
        let (dst, src) = self.parse_rr().unwrap();
        SdivRR(dst, src)
    }

    fn parse_udiv(&mut self) -> Op {
        self.expect_tok(Ident("udiv".to_string()));
        // FIXME: try!
        let (dst, src) = self.parse_rr().unwrap();
        UdivRR(dst, src)
    }

    fn parse_rng_args<T>(&mut self) ->
        ParseResult<(Register, Register, Option<T>, Option<T>)>
        where T : FromStr, T::Err : Display
    {
        let dst = try!(self.parse_any_reg());
        // FIXME: try!
        self.expect_tok(Comma);
        let reg = try!(self.parse_any_reg());
        // FIXME: try!
        self.expect_tok(Comma);
        let min = try!(self.parse_opt_int());
        // FIXME: try!
        self.expect_tok(Comma);
        let max = try!(self.parse_opt_int());

        Ok((dst, reg, min, max))
    }

    fn parse_srng(&mut self) -> Op {
        self.expect_tok(Ident("srng".to_string()));
        // FIXME: try!
        let (dst, reg, min, max) = self.parse_rng_args().unwrap();
        Srng(dst, reg, min, max)
    }

    fn parse_urng(&mut self) -> Op {
        self.expect_tok(Ident("urng".to_string()));
        // FIXME: try!
        let (dst, reg, min, max) = self.parse_rng_args().unwrap();
        Urng(dst, reg, min, max)
    }

    fn parse_halt(&mut self) -> Op {
        self.expect_tok(Ident("halt".to_string()));
        Halt
    }

    // FIXME: Unify the handling of parse_b and parse_bl

    fn parse_b(&mut self) -> Op {
        self.expect_tok(Ident("b".to_string()));
        if let Ok(reg) = self.parse_any_reg() {
            BrR(reg)
        } else {
            match self.cur().item {
                LabelRef(label) => {
                    self.accept();
                    BrL(label[1..].to_string())
                }
                // FIXME: Don't panic.
                _ => panic!("expected register or label but found: {:?}", self.cur()),
            }
        }
    }

    fn parse_bl(&mut self) -> Op {
        self.expect_tok(Ident("bl".to_string()));
        if let Ok(reg) = self.parse_any_reg() {
            BrLnkR(reg)
        } else {
            match self.cur().item {
                LabelRef(label) => {
                    self.accept();
                    BrLnkL(label[1..].to_string())
                }
                // FIXME: Don't panic.
                _ => panic!("expected register or label but found: {:?}", self.cur()),
            }
        }
    }
}

#[test]
fn test_label() {
    let mut parser = Parser::new(Lexer::new("foo:"));
    assert_eq!(
        vec!(LabelStmt("foo".to_string())),
        parser.parse_program());
}

#[test]
fn test_cond_op() {
    let mut parser = Parser::new(Lexer::new("{p0, #0, #0} ldr r0, [r1]"));
    assert_eq!(
        vec!(Instr(
            vec!(Cond { reg: Register::Pred(0), min: 0, max: 0 }),
            LdrRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_ldr_rr() {
    let mut parser = Parser::new(Lexer::new("ldr r0, [r1]"));
    assert_eq!(
        vec!(Instr(vec!(), LdrRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_ldr_rl() {
    let mut parser = Parser::new(Lexer::new("ldr r0, =foo"));
    assert_eq!(
        vec!(Instr(vec!(), LdrRL(Register::Gen(0), "foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_add_rr() {
    let mut parser = Parser::new(Lexer::new("add r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), AddRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_add_xi() {
    let mut parser = Parser::new(Lexer::new("add @a, foo, #10, r0"));
    assert_eq!(
        vec!(Instr(vec!(), AddXI("@a".to_string(), "foo".to_string(), 10, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_add_xr() {
    let mut parser = Parser::new(Lexer::new("add @a, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), AddXR(
            "@a".to_string(), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_sub_rr() {
    let mut parser = Parser::new(Lexer::new("sub r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), SubRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}


#[test]
fn test_sub_xi() {
    let mut parser = Parser::new(Lexer::new("sub @a, foo, #10, r0"));
    assert_eq!(
        vec!(Instr(vec!(), SubXI("@a".to_string(), "foo".to_string(), 10, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_sub_xr() {
    let mut parser = Parser::new(Lexer::new("sub @a, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), SubXR(
            "@a".to_string(), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_rr() {
    let mut parser = Parser::new(Lexer::new("mov r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), MovRR(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_ri() {
    let mut parser = Parser::new(Lexer::new("mov r0, #37"));
    assert_eq!(
        vec!(Instr(vec!(), MovRI(Register::Gen(0), 37))),
        parser.parse_program());
}

#[test]
fn test_mov_rx() {
    let mut parser = Parser::new(Lexer::new("mov r0, @r, foo"));
    assert_eq!(
        vec!(Instr(vec!(), MovRX(Register::Gen(0), "@r".to_string(), "foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_mov_xr() {
    let mut parser = Parser::new(Lexer::new("mov @r, foo, r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), MovXR("@r".to_string(), "foo".to_string(), Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_mov_xi() {
    let mut parser = Parser::new(Lexer::new("mov @r, foo, #15, r0"));
    assert_eq!(
        vec!(Instr(vec!(), MovXI("@r".to_string(), "foo".to_string(), 15, Register::Gen(0)))),
        parser.parse_program());
}

#[test]
fn test_mul() {
    let mut parser = Parser::new(Lexer::new("mul r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), Mul(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_sdiv() {
    let mut parser = Parser::new(Lexer::new("sdiv r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), Sdiv(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_udiv() {
    let mut parser = Parser::new(Lexer::new("udiv r0, r1"));
    assert_eq!(
        vec!(Instr(vec!(), Udiv(Register::Gen(0), Register::Gen(1)))),
        parser.parse_program());
}

#[test]
fn test_br_r() {
    let mut parser = Parser::new(Lexer::new("b lr"));
    assert_eq!(
        vec!(Instr(vec!(), BrR(Register::Spec("lr".to_string())))),
        parser.parse_program());
}

#[test]
fn test_br_l() {
    let mut parser = Parser::new(Lexer::new("b =foo"));
    assert_eq!(
        vec!(Instr(vec!(), BrL("foo".to_string()))),
        parser.parse_program());
}

#[test]
fn test_br_lnk_r() {
    let mut parser = Parser::new(Lexer::new("bl lr"));
    assert_eq!(
        vec!(Instr(vec!(), BrLnkR(Register::Spec("lr".to_string())))),
        parser.parse_program());
}

#[test]
fn test_srng() {
    let mut parser = Parser::new(Lexer::new("srng p0, r0, #0, #1"));
    assert_eq!(
        vec!(Instr(vec!(), Srng(Register::Pred(0), Register::Gen(0), Some(0), Some(1)))),
        parser.parse_program());
}

#[test]
fn test_urng() {
    let mut parser = Parser::new(Lexer::new("urng p0, r0, #0, *"));
    assert_eq!(
        vec!(Instr(vec!(), Urng(Register::Pred(0), Register::Gen(0), Some(0), None))),
        parser.parse_program());
}

#[test]
fn test_halt() {
    let mut parser = Parser::new(Lexer::new("halt"));
    assert_eq!(vec!(Instr(vec!(), Halt)), parser.parse_program());
}

#[test]
fn test_raw_cmd() {
    let mut parser = Parser::new(Lexer::new("raw foo bar baz"));
    assert_eq!(
        vec!(Instr(vec!(), RawCmd("foo bar baz".to_string()))),
        parser.parse_program());
}

#[test]
fn test_raw_cmd_cond() {
    let mut parser = Parser::new(Lexer::new("{p0, #1, #1} raw foo bar baz"));
    assert_eq!(
        vec!(Instr(
            vec!(Cond { reg: Register::Pred(0), min: 1, max: 1 }),
            RawCmd("foo bar baz".to_string()))),
        parser.parse_program());
}
