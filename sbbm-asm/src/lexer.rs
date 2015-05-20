use self::Token::*;

use regex::Regex;
use std::collections::VecDeque;
use std::fmt;

// FIXME: Work out the lifetime issues and use &'a str for value, then
//        re-derive Copy.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Newline(String),
    Comma,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Star,
    LParen,
    RParen,
    LitStr(String),
    LitInt(String),
    Selector(String),
    Attr(String),
    Label(String),
    LabelRef(String),
    Meta(String),
    Ident(String),
    GenReg(String),
    PredReg(String),
    SpecReg(String),
    Def(String, String),
    Raw(Vec<(char, Token)>, String),
    Eof,
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T, Loc> {
    pub item: T,
    pub start: Loc,
    pub end: Loc,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Location {
    pub offset: usize,
    pub col: usize,
    pub line: usize,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

pub type SpannedToken = Spanned<Token, Location>;

pub type LexResult<T> = Result<T, String>;
struct StateFn(fn(&mut Lexer) -> StateFn);

pub struct Lexer<'a> {
    input: &'a str,
    filename: &'a str,
    pos: usize,
    width: usize,
    mark: Location,
    state: StateFn,
    tokbuf: VecDeque<LexResult<SpannedToken>>,
    line: usize,

    line_start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, filename: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            filename: filename,
            pos: 0,
            width: 0,
            mark: Location { offset: 0, col: 0, line: 0 },
            state: StateFn(lex_start),
            tokbuf: VecDeque::new(),
            line: 0,
            line_start: 0,
        }
    }

    pub fn mem(input: &'a str) -> Lexer<'a> {
        Lexer::new(input, "<memory>")
    }

    pub fn next(&mut self) -> LexResult<SpannedToken> {
        loop {
            if let Some(tok) = self.tokbuf.pop_front() {
                return tok;
            }
            let StateFn(f) = self.state;
            self.state = f(self);
        }
    }

    fn rest(&self) -> &str {
        &self.input[self.pos..]
    }

    fn piece(&self) -> &str {
        let end = self.pos + self.width;
        &self.input[self.mark.offset..end]
    }

    fn accept<Pred>(&mut self, pred: Pred) -> Option<char>
        where Pred : FnOnce(char) -> LexResult<()>
    {
        match self.rest().chars().next() {
            None => {
                self.pos = self.input.len();
                self.width = 0;
                None
            }
            Some(c) => {
                match pred(c) {
                    Ok(_) => {
                        self.pos += c.len_utf8();
                        Some(c)
                    }
                    Err(_) => None,
                }
            }
        }
    }

    fn accept_any(&mut self) -> Option<char> {
        self.accept(move |_| Ok(()))
    }

    fn accept_char(&mut self, c: char) -> bool {
        let pred = move |x| if x == c { Ok(()) } else { Err("".to_string()) };
        self.accept(pred).is_some()
    }

    fn loc(&self) -> Location {
        Location {
            offset: self.pos,
            col: self.pos - self.line_start,
            line: self.line,
        }
    }

    fn emit(&mut self, token: Token) {
        let end = self.loc();
        let spanned = SpannedToken {
            item: token,
            start: self.mark,
            end: end,
        };
        self.tokbuf.push_back(Ok(spanned));
        self.mark = end
    }

    // FIXME: Switch to a nicer error type than String.
    fn error(&mut self, msg: String) {
        let err = format!(
            "{}:{}: {} {}", self.filename, self.mark, self.loc(), msg);
        self.tokbuf.push_back(Err(err));
        self.mark = self.loc();
    }

    fn skip(&mut self) {
        self.mark = self.loc();
    }

    fn expect_char(&mut self, c: char) -> LexResult<()> {
        self.expect(move |x: char| {
            if x == c {
                Ok(())
            } else {
                // FIXME: Change to a different kind of Result to allow
                //        formatting an error message.
                Err(format!("expected '{}' but found '{}'", c, x))
            }
        })
    }

    fn expect<Pred>(&mut self, pred: Pred) -> LexResult<()>
        where Pred : FnOnce(char) -> LexResult<()>
    {
        match self.rest().chars().next() {
            None => Err("unexpected eof".to_string()),
            Some(c) => {
                let res = pred(c);
                if res.is_ok() {
                    self.accept_any();
                }
                res
            }
        }
    }

    fn zero_or_more<Pred>(&mut self, pred: Pred)
        where Pred : Fn(char) -> LexResult<()>
    {
        loop {
            match self.rest().chars().next() {
                None => { break; }
                Some(c) => match pred(c) {
                    Ok(_) => { self.accept_any(); }
                    Err(_) => { break; }
                }
            }
        }
    }

    fn one_or_more<Pred>(&mut self, pred: Pred) -> LexResult<()>
        where Pred : Fn(char) -> LexResult<()>
    {
        try!(self.expect(&pred));
        self.zero_or_more(pred);
        Ok(())
    }
}

pub fn unquote(s: &str) -> String {
    // FIXME: Process escape codes, verify existence of opening and closing ",
    //        return Option<String> or a Result type.
    s[1..s.len()-1].to_string()
}

macro_rules! try_lex {
    ($lexer:expr, $e:expr) => {
        match $e {
            Ok(item) => item,
            Err(msg) => {
                $lexer.error(msg);
                return StateFn(lex_error);
            }
        }
    }
}

fn is_whitespace(c: char) -> LexResult<()> {
    if c.is_whitespace() {
        Ok(())
    } else {
        Err(format!("expected whitespace but found '{}'", c))
    }
}

fn is_alpha(c: char) -> LexResult<()> {
    match c {
        'a' ... 'z' | 'A' ... 'Z' => Ok(()),
        _ => Err(format!("expected alpha but found '{}'", c)),
    }
}

fn is_digit(c: char) -> LexResult<()> {
    match c {
        '0' ... '9' => Ok(()),
        _ => Err(format!("expected digit but found '{}'", c)),
    }
}

fn is_ident_start(c: char) -> LexResult<()> {
    match c {
        'a'...'z' | 'A'...'Z' | '_' | '.' | '$' => Ok (()),
        _ => Err(format!("expected the start of an ident but found '{}'", c)),
    }
}

fn is_ident_rest(c: char) -> LexResult<()> {
    match c {
        'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '-' | '.' | '$' => Ok (()),
        _ => Err(format!("expected the rest of an ident but found '{}'", c)),
    }
}

fn is_not_newline(c: char) -> LexResult<()> {
    match c {
        '\r' | '\n' => Err("".to_string()),
        _ => Ok(()),
    }
}

fn is_selector_start(c: char) -> LexResult<()> {
    match c {
        'a' | 'p' | 'r' | 'e' => Ok (()),
        _ => Err("".to_string())
    }
}

static GEN_REG_REGEX : Regex = regex!(r"^r\d+$");
static PRED_REG_REGEX : Regex = regex!(r"^p\d+$");

fn classify_register(s: &str) -> Option<Token> {
    if GEN_REG_REGEX.is_match(s) {
        let reg = s.to_string();
        Some(GenReg(reg))
    } else if PRED_REG_REGEX.is_match(s) {
        let reg = s.to_string();
        Some(PredReg(reg))
    } else if s == "lr" || s == "sp" {
        let reg = s.to_string();
        Some(SpecReg(reg))
    } else {
        None
    }
}

fn lex_start(lexer: &mut Lexer) -> StateFn {
    match lexer.rest().chars().next() {
        None => StateFn(lex_end),
        Some(c) => match c {
            '{' => { lexer.accept_any(); lexer.emit(LBrace); StateFn(lex_start) }
            '}' => { lexer.accept_any(); lexer.emit(RBrace); StateFn(lex_start) }
            '(' => { lexer.accept_any(); lexer.emit(LParen); StateFn(lex_start) }
            ')' => { lexer.accept_any(); lexer.emit(RParen); StateFn(lex_start) }
            '*' => { lexer.accept_any(); lexer.emit(Star); StateFn(lex_start) }
            ',' => { lexer.accept_any(); lexer.emit(Comma); StateFn(lex_start) }
            '[' => { lexer.accept_any(); lexer.emit(LBracket); StateFn(lex_start) }
            ']' => { lexer.accept_any(); lexer.emit(RBracket); StateFn(lex_start) }
            ';' => { StateFn(lex_comment) }
            '"' => { StateFn(lex_lit_str) }
            '#' => { StateFn(lex_lit_int) }
            '@' => { StateFn(lex_selector_or_attr) }
            '\r' | '\n' => { StateFn(lex_newline) }
            '=' => { StateFn(lex_label_ref) }
            c if is_whitespace(c).is_ok() => { StateFn(lex_whitespace) }
            _ => { StateFn(lex_ident_like) }
        }
    }
}

fn lex_lit_str(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect_char('"'));
    // FIXME: support escapes
    lexer.zero_or_more(|c| if c != '"' {
        Ok(())
    } else {
        Err("".to_string())
    });
    try_lex!(lexer, lexer.expect_char('"'));

    let litstr = lexer.piece().to_string();
    lexer.emit(LitStr(litstr));
    StateFn(lex_start)
}

fn lex_lit_int(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect_char('#'));
    lexer.accept_char('-');
    try_lex!(lexer, lexer.one_or_more(is_digit));

    let litint = lexer.piece().to_string();
    lexer.emit(LitInt(litint));
    StateFn(lex_start)
}

fn lex_selector_or_attr(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect_char('@'));

    if let Some(_) = lexer.accept(is_selector_start) {
        if lexer.accept(is_ident_rest).is_some() {
            lexer.zero_or_more(is_ident_rest);
            let attr = lexer.piece().to_string();
            lexer.emit(Attr(attr));
        } else {
            if lexer.accept_char('[') {
                // FIXME: Support quoted ]
                lexer.zero_or_more(|c| if c != ']' { Ok (()) } else { Err("".to_string()) });
                try_lex!(lexer, lexer.expect_char(']'));
            }
            let selector = lexer.piece().to_string();
            lexer.emit(Selector(selector));
        }
    } else {
        try_lex!(lexer, lexer.expect(is_ident_start));
        lexer.zero_or_more(is_ident_rest);

        let attr = lexer.piece().to_string();
        lexer.emit(Attr(attr));
    }
    StateFn(lex_start)
}

fn lex_ident_like(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect(is_ident_start));
    lexer.zero_or_more(is_ident_rest);
    if lexer.accept_char(':') {
        let label = lexer.piece().to_string();
        lexer.emit(Label(label));
    } else {
        if lexer.piece().starts_with(".") {
            let meta = lexer.piece().to_string();
            lexer.emit(Meta(meta));
        } else if lexer.piece() == "def" {
            return StateFn(lex_def)
        } else if lexer.piece() == "raw" {
            return StateFn(lex_raw)
        } else if let Some(reg) = classify_register(lexer.piece()) {
            lexer.emit(reg);
        } else {
            let ident = lexer.piece().to_string();
            lexer.emit(Ident(ident));
        }
    }

    StateFn(lex_start)
}

fn lex_def(lexer: &mut Lexer) -> StateFn {
    // FIXME: Assert that lexer.piece() == "def"
    lexer.skip();
    try_lex!(lexer, lexer.one_or_more(is_whitespace));
    lexer.skip();

    try_lex!(lexer, lexer.expect(is_ident_start));
    lexer.zero_or_more(is_ident_rest);

    let name = lexer.piece().to_string();
    lexer.skip();

    lexer.zero_or_more(is_whitespace);
    try_lex!(lexer, lexer.expect_char(','));
    lexer.zero_or_more(is_whitespace);
    lexer.skip();

    lexer.zero_or_more(is_not_newline);
    let value = lexer.piece().to_string();

    lexer.emit(Def(name, value));
    StateFn(lex_start)
}

fn lex_raw(lexer: &mut Lexer) -> StateFn {
    // FIXME: Assert that lexer.piece() == "raw"
    lexer.skip();

    let mut out_chars = vec!();
    if lexer.accept_char('~') {
        lexer.skip();

        while let Some(c) = lexer.accept(is_alpha) {
            out_chars.push(c);
        }
        lexer.skip();
    }

    try_lex!(lexer, lexer.one_or_more(is_whitespace));
    lexer.skip();

    let mut outs = vec!();
    for c in out_chars.into_iter() {
        lexer.zero_or_more(is_whitespace);
        lexer.skip();

        try_lex!(lexer, lexer.expect(is_ident_start));
        lexer.zero_or_more(is_ident_rest);
        if let Some(reg) = classify_register(lexer.piece()) {
            outs.push((c, reg));
            lexer.skip();
        } else {
            let err = format!("expected register but found '{}'", lexer.piece());
            lexer.error(err);
            return StateFn(lex_error)
        }

        lexer.zero_or_more(is_whitespace);
        lexer.skip();

        try_lex!(lexer, lexer.expect_char(','));
    }

    lexer.zero_or_more(is_whitespace);
    lexer.skip();

    lexer.zero_or_more(is_not_newline);

    let raw = lexer.piece().to_string();
    lexer.emit(Raw(outs, raw));
    StateFn(lex_start)
}

fn lex_label_ref(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect_char('='));
    try_lex!(lexer, lexer.expect(is_ident_start));
    lexer.zero_or_more(is_ident_rest);

    let label_ref = lexer.piece().to_string();
    lexer.emit(LabelRef(label_ref));
    StateFn(lex_start)
}

fn lex_whitespace(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.one_or_more(is_whitespace));
    lexer.skip();

    StateFn(lex_start)
}

fn lex_newline(lexer: &mut Lexer) -> StateFn {
    lexer.accept_char('\r');
    try_lex!(lexer, lexer.expect_char('\n'));

    let newline = lexer.piece().to_string();
    lexer.emit(Newline(newline));

    lexer.line += 1;
    lexer.line_start = lexer.pos;

    StateFn(lex_start)
}

fn lex_comment(lexer: &mut Lexer) -> StateFn {
    try_lex!(lexer, lexer.expect_char(';'));
    lexer.zero_or_more(is_not_newline);
    lexer.skip();

    StateFn(lex_start)
}

#[allow(unconditional_recursion)]
fn lex_end<'a>(lexer: &mut Lexer<'a>) -> StateFn {
    lexer.emit(Eof);
    StateFn(lex_end)
}

#[allow(unconditional_recursion)]
fn lex_error(lexer: &mut Lexer) -> StateFn {
    lexer.emit(Error);
    StateFn(lex_error)
}

macro_rules! first_tok {
    ($input:expr) => {{
        let mut lexer = Lexer::mem($input);
        lexer.next().unwrap().item
    }}
}

#[test]
fn test_punctuation() {
    assert_eq!(first_tok!("{"), LBrace);
    assert_eq!(first_tok!("}"), RBrace);
    assert_eq!(first_tok!("("), LParen);
    assert_eq!(first_tok!(")"), RParen);
    assert_eq!(first_tok!("*"), Star);
    assert_eq!(first_tok!(","), Comma);
    assert_eq!(first_tok!("["), LBracket);
    assert_eq!(first_tok!("]"), RBracket);
}

#[test]
fn test_label_ref() {
    assert_eq!(first_tok!("=foo"), LabelRef("=foo".to_string()));
}

#[test]
fn test_label() {
    assert_eq!(first_tok!("foo:"), Label("foo:".to_string()));
}

#[test]
fn test_ident() {
    assert_eq!(first_tok!("add"), Ident("add".to_string()));
}

#[test]
fn test_def() {
    assert_eq!(
        first_tok!("def foo, bar"),
        Def("foo".to_string(), "bar".to_string()));
}

#[test]
fn test_raw() {
    assert_eq!(
        first_tok!("raw foo bar baz"),
        Raw(vec!(), "foo bar baz".to_string()));
}

#[test]
fn test_raw_outs() {
    let outs = vec!(
        ('s', GenReg("r0".to_string())),
        ('a', GenReg("r1".to_string())),
        ('i', GenReg("r2".to_string())));
    assert_eq!(
        first_tok!("raw~sai r0, r1, r2, foo bar baz"),
        Raw(outs, "foo bar baz".to_string()));
}

#[test]
fn test_lit_str() {
    assert_eq!(
        first_tok!("\"foo bar baz\""),
        LitStr("\"foo bar baz\"".to_string()));
}

#[test]
fn test_lit_int() {
    assert_eq!(first_tok!("#219381"), LitInt("#219381".to_string()));
    assert_eq!(first_tok!("#-100"), LitInt("#-100".to_string()));
}

#[test]
fn test_selector() {
    assert_eq!(
        first_tok!("@a[score_Foo=100]"),
        Selector("@a[score_Foo=100]".to_string()));
}

#[test]
fn test_attr() {
    assert_eq!(first_tok!("@foo"), Attr("@foo".to_string()));
    assert_eq!(first_tok!("@aroo"), Attr("@aroo".to_string()));
}

#[test]
fn test_register() {
    assert_eq!(first_tok!("r0"), GenReg("r0".to_string()));
}

#[test]
fn test_predicate_reg() {
    assert_eq!(first_tok!("p0"), PredReg("p0".to_string()));
}

#[test]
fn test_special_reg() {
    assert_eq!(first_tok!("lr"), SpecReg("lr".to_string()));
}

#[test]
fn test_whitespace() {
    let mut lexer = Lexer::mem("foo \tbar");
    assert_eq!(lexer.next().unwrap().item, Ident("foo".to_string()));
    assert_eq!(lexer.next().unwrap().item, Ident("bar".to_string()));
}

#[test]
fn test_newline() {
    let mut lexer = Lexer::mem("foo\nbar\r\n\n");
    assert_eq!(lexer.next().unwrap().item, Ident("foo".to_string()));
    assert_eq!(lexer.next().unwrap().item, Newline("\n".to_string()));
    assert_eq!(lexer.next().unwrap().item, Ident("bar".to_string()));
    assert_eq!(lexer.next().unwrap().item, Newline("\r\n".to_string()));
    assert_eq!(lexer.next().unwrap().item, Newline("\n".to_string()));
}

#[test]
fn test_comment() {
    let mut lexer = Lexer::mem("; foo\nbar");
    assert_eq!(lexer.next().unwrap().item, Newline("\n".to_string()));
    assert_eq!(lexer.next().unwrap().item, Ident("bar".to_string()));
}
