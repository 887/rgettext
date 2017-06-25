#![crate_type="dylib"]
#![feature(plugin_registrar, quote, rustc_private)]
#![feature(slice_patterns)]

extern crate syntax;
extern crate syntax_pos;
#[macro_use]
extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_errors;

#[macro_use]
extern crate lazy_static;

use std::sync::RwLock;

use syntax::tokenstream::TokenTree;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult, DummyResult};
use syntax::parse::token::Token;
use syntax::ast::Expr;
use syntax::ptr::P;
use syntax_pos::Span;
use rustc_errors::DiagnosticBuilder;
use rustc_plugin::Registry;
use rustc::hir::Crate;
use rustc::lint::{LintPass, LintArray, LateLintPass, LateContext};

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("g", expand_g);
    reg.register_macro("ng", expand_ng);
    reg.register_macro("dg", expand_dg);
    reg.register_macro("dng", expand_dng);
    reg.register_macro("dcg", expand_dcg);
    reg.register_macro("dcng", expand_dcng);
    reg.register_late_lint_pass(Box::new(FakeLint));
}

macro_rules! emittry {
    ($e: expr, $sp: expr) => {
        match $e {
            Ok(r) => r,
            Err(mut e) => {
                e.emit();
                return DummyResult::any($sp);
            }
        }
    }
}

lazy_static! {
    static ref TEXTDOMAIN: RwLock<Option<String>> = RwLock::new(None);
    static ref POT: RwLock<Vec<Msg>> = RwLock::new(Vec::new());
}

#[derive(Debug)]
struct Reference {
    file: String,
    line: usize,
}

#[derive(Debug)]
struct Msg {
    translator_comments: Vec<String>, // #
    extracted_comments: Vec<String>, // #.
    reference: Vec<Reference>, // #:
    flag: Vec<String>, // #,
    previous: Vec<String>, // #|
    msgctxt: Option<String>,
    msgid: String,
    msgid_plural: Option<String>,
    msgstr: Vec<String>,
}

declare_lint! {
    FAKE_LINT,
    Allow,
    ""
}

pub struct FakeLint;

impl LintPass for FakeLint {
    fn get_lints(&self) -> LintArray {
        lint_array![FAKE_LINT]
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for FakeLint {
    fn check_crate(&mut self, _cx: &LateContext<'a, 'tcx>, _krate: &'tcx Crate) {
        println!("result: {:?}", POT.read().unwrap());
    }
}

fn expand_g(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs { singular, .. } = emittry!(parse(cx, sp, args, GettextFn::G), sp);

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::gettext($singular);
    }))
}

fn expand_ng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs {
        singular,
        plural,
        n,
        ..
    } = emittry!(parse(cx, sp, args, GettextFn::Ng), sp);
    let plural = plural.unwrap();
    let n = n.unwrap();

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::ngettext($singular, $plural, $n);
    }))
}

fn expand_dg(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs { domain, singular, .. } = emittry!(parse(cx, sp, args, GettextFn::Dg), sp);
    let domain = domain.unwrap();

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::dgettext($domain, $singular);
    }))
}

fn expand_dng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs {
        domain,
        singular,
        plural,
        n,
        ..
    } = emittry!(parse(cx, sp, args, GettextFn::Dng), sp);
    let domain = domain.unwrap();
    let plural = plural.unwrap();
    let n = n.unwrap();

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::dngettext($domain, $singular, $plural, $n);
    }))
}

fn expand_dcg(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs {
        domain,
        singular,
        category,
        ..
    } = emittry!(parse(cx, sp, args, GettextFn::Dcg), sp);
    let domain = domain.unwrap();
    let category = category.unwrap();

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::dcgettext($domain, $singular, $category);
    }))
}

fn expand_dcng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let GettextArgs {
        domain,
        singular,
        plural,
        n,
        category,
    } = emittry!(parse(cx, sp, args, GettextFn::Dcng), sp);
    let domain = domain.unwrap();
    let plural = plural.unwrap();
    let n = n.unwrap();
    let category = category.unwrap();

    MacEager::expr(quote_expr!(cx, {
        ::gettextrs::dcngettext($domain, $singular, $plural, $n, $category);
    }))
}

enum GettextFn {
    G,
    Ng,
    Dg,
    Dng,
    Dcg,
    Dcng,
}

struct GettextArgs {
    domain: Option<P<Expr>>,
    singular: String,
    plural: Option<String>,
    n: Option<P<Expr>>,
    category: Option<P<Expr>>,
}

fn parse<'a>(
    cx: &'a mut ExtCtxt,
    sp: Span,
    args: &[TokenTree],
    target: GettextFn,
) -> Result<GettextArgs, DiagnosticBuilder<'a>> {
    use GettextFn::*;

    let mut parser = cx.new_parser_from_tts(args);

    if parser.token == Token::Eof {
        let n = match target {
            G => 1,
            Ng | Dcg => 3,
            Dg => 2,
            Dng => 4,
            Dcng => 5,
        };
        let m = format!("this macro takes {} or {} parameter\
                         but {} parameters were supplied", n, n + 1, 0);
        let mut e = cx.struct_span_err(sp, &m);
        e.span_label(sp, format!("expected {} or {} parameter", n, n + 1));
        return Err(e);
    }

    let domain = match target {
        Dg | Dng | Dcg | Dcng => {
            let r = parser.parse_expr()?;
            parser.expect(&Token::Comma)?;
            Some(r)
        }
        _ => None,
    };
    let msgid = parser.parse_str()?.0.as_str().to_string();
    parser.expect_one_of(&[Token::Comma, Token::Eof], &[])?;
    let msgid_plural = match target {
        Ng | Dng | Dcng => {
            let r = parser.parse_str()?.0.as_str().to_string();
            parser.expect_one_of(&[Token::Comma, Token::Eof], &[])?;
            Some(r)
        }
        _ => None,
    };
    let n = match target {
        Ng | Dng | Dcng => {
            let r = parser.parse_expr()?;
            parser.expect_one_of(&[Token::Comma, Token::Eof], &[])?;
            Some(r)
        }
        _ => None,
    };
    let category = match target {
        Dcg | Dcng => {
            let r = parser.parse_expr()?;
            parser.expect_one_of(&[Token::Comma, Token::Eof], &[])?;
            Some(r)
        }
        _ => None,
    };
    if parser.token != Token::Eof {
        parser.unexpected()?;
    }

    let fl = cx.codemap().span_to_lines(sp).unwrap();
    let refence = Reference {
        file: fl.file.name.to_owned(),
        line: fl.lines.first().unwrap().line_index,
    };
    let msg = Msg {
        translator_comments: Vec::new(),
        extracted_comments: Vec::new(),
        reference: vec![refence],
        flag: Vec::new(),
        previous: Vec::new(),
        msgctxt: None,
        msgid: msgid.clone(),
        msgid_plural: msgid_plural.clone(),
        msgstr: Vec::new(),
    };
    POT.write().unwrap().push(msg);
    println!(
        "{}: {:?}",
        fl.file.name,
        fl.lines.first().unwrap().line_index
    );
    Ok(GettextArgs {
        domain: domain,
        singular: msgid,
        plural: msgid_plural,
        n: n,
        category: category,
    })
}
