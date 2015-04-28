#![feature(plugin_registrar, quote, rustc_private)]
#![crate_type = "dylib"]

extern crate syntax;
extern crate rustc;

use syntax::ast;
use syntax::codemap::{Span, DUMMY_SP};
use syntax::ext::base;
use syntax::ext::base::{ExtCtxt, MacEager};
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::parse::parser::Parser;
use syntax::parse::token::DelimToken;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use rustc::plugin::Registry;
use syntax::print::pprust;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("sbbm", expand_sbbm_macro);
}

fn str_lit(cx: &mut ExtCtxt, s: &str) -> P<ast::Expr> {
    let raw = token::intern_and_get_ident(s);
    cx.expr_lit(DUMMY_SP, ast::LitStr(raw, ast::CookedStr))
}

fn expand_sbbm_macro(
    cx: &mut ExtCtxt, _: Span, tts: &[ast::TokenTree])
    -> Box<base::MacResult + 'static>
{
    let (objectives, selectors) = parse_tts(cx, tts);

    let mut items = vec!();
    for objective in objectives {
        let name = cx.ident_of(objective.name.as_ref());
        let criteria = str_lit(cx, objective.criteria.as_ref());
        if let Some(display_name) = objective.display_name {
            let display_name = str_lit(cx, display_name.as_ref());
            items.push(quote_item!(
                cx, sbbm_objective!($name, $criteria, $display_name);
            ).unwrap());
        } else {
            items.push(quote_item!(
                cx, sbbm_objective!($name, $criteria);
            ).unwrap());
        }
    }

    for selector in selectors {
        let sel_id = cx.ident_of(selector.name.as_ref());
        let sel_str = str_lit(cx, selector.selector.as_ref());

        let decl = quote_item!(cx, sbbm_sel_decl!($sel_id, $sel_str);).unwrap();
        items.push(decl);
    }

    MacEager::items(SmallVector::many(items))
}

struct Objective {
    name: String,
    criteria: String,
    display_name: Option<String>,
}

struct Selector {
    name: String,
    selector: String,
}

fn parse_tts(cx: &ExtCtxt,
             tts: &[ast::TokenTree]) -> (Vec<Objective>, Vec<Selector>) {
    let p = &mut cx.new_parser_from_tts(tts);

    let mut objectives = vec!();
    let mut selectors = vec!();

    while p.token != token::Eof {
        match p.token {
            token::Ident(ident, _) => {
                match token::get_ident(ident).as_ref() {
                    "objectives" => objectives = parse_objectives(p),
                    "selectors" => selectors = parse_selectors(p),
                    _ => {
                        let _ = p.bump();
                        p.error("what!")
                    }
                }
            }
            _ => {
                let _ = p.bump();
                p.error("and how!")
            }
        }
    }

    (objectives, selectors)
}

fn parse_objectives(p: &mut Parser) -> Vec<Objective> {
    let mut objectives = vec!();

    // REVIEW: Should check for Ident("objectives", false) rather than bumping.
    let _ = p.bump();
    let _ = p.expect(&token::OpenDelim(DelimToken::Brace));
    while p.token != token::CloseDelim(DelimToken::Brace) {
        let objective = parse_objective(p);
        objectives.push(objective);
    }
    let _ = p.expect(&token::CloseDelim(DelimToken::Brace));

    objectives
}

fn parse_selectors(p: &mut Parser) -> Vec<Selector> {
    let mut selectors = vec!();

    let _ = p.bump();
    let _ = p.expect(&token::OpenDelim(DelimToken::Brace));
    while p.token != token::CloseDelim(DelimToken::Brace) {
        let selector = parse_selector(p);
        selectors.push(selector);
    }
    let _ = p.expect(&token::CloseDelim(DelimToken::Brace));

    selectors
}

fn parse_objective(p: &mut Parser) -> Objective {
    let name = match p.expect_ident() {
        Some(s) => s,
        None => {
            let _ = p.bump();
            "#error#".to_string()
        }
    };
    let _ = p.expect(&token::Colon);
    let criteria = p.expect_str_lit().unwrap_or("#error#".to_string());

    let display_name = if p.token == token::Comma {
        let _ = p.expect(&token::Comma);
        Some(p.expect_str_lit().unwrap_or("#error#".to_string()))
    } else {
        None
    };
    let _ = p.expect(&token::Semi);

    Objective {
        name: name,
        criteria: criteria,
        display_name: display_name,
    }
}

fn parse_selector(p: &mut Parser) -> Selector {
    let name = match p.expect_ident() {
        Some(s) => s,
        None => {
            let _ = p.bump();
            "#error#".to_string()
        }
    };
    let _ = p.expect(&token::Colon);
    let selector = p.expect_str_lit().unwrap_or("#error#".to_string());
    let _ = p.expect(&token::Semi);

    Selector {
        name: name,
        selector: selector,
    }
}

trait ParserExt {
    fn error(&self, m: &str);
    fn expect_ident(&mut self) -> Option<String>;
    fn expect_str_lit(&mut self) -> Option<String>;
}

impl<'a> ParserExt for Parser<'a> {
    fn error(&self, m: &str) {
        self.span_err(self.span, m)
    }

    fn expect_ident(&mut self) -> Option<String> {
        match self.token {
            token::Ident(ident, _) => {
                let _ = self.bump();
                Some(ident.as_str().to_string())
            },
            _ => {
                let tok_str = pprust::token_to_string(&self.token);
                let msg = format!("expected identifier but found `{}`",
                                  tok_str);
                self.error(msg.as_ref());
                None
            },
        }
    }

    fn expect_str_lit(&mut self) -> Option<String> {
        let expr = self.parse_expr();

        let s = match expr.node {
            ast::ExprLit(ref lit) => match lit.node {
                ast::LitStr(ref s, _) => Some(s.to_string()),
                _ => None
            },
            _ => None
        };

        if s.is_none() {
            let msg = format!("expected string literal but found `{}`",
                              pprust::expr_to_string(&expr));
            self.span_err(expr.span, msg.as_ref());
        }
        s
    }
}
