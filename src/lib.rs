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
use syntax::parse::{parser, token};
use syntax_pos::Span;
use rustc_errors::DiagnosticBuilder;
use rustc_plugin::Registry;
use rustc::hir::Crate;
use rustc::lint::{LintPass, LintArray, LateLintPass, LateContext};

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("textdomain", expand_textdomain);
    reg.register_macro("g", expand_g);
    // reg.register_macro("ng", expand_g);
    // reg.register_macro("dg", expand_g);
    // reg.register_macro("dng", expand_g);
    // reg.register_macro("dcg", expand_g);
    // reg.register_macro("dcng", expand_g);
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

fn check_textdomain<'a>(cx: &'a mut ExtCtxt, sp: Span) -> Result<(), DiagnosticBuilder<'a>> {
    if TEXTDOMAIN.read().unwrap().is_none() {
        Err(cx.struct_span_err(sp, "must call textdomain first!"))
    } else {
        Ok(())
    }
}

fn expand_g(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    emittry!(check_textdomain(cx, sp), sp);
    let cm = cx.codemap();
    let fl = cm.span_to_lines(sp).unwrap();
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
        msgid: "ahhh".to_string(),
        msgid_plural: None,
        msgstr: Vec::new(),
    };
    POT.write().unwrap().push(msg);
    println!(
        "{}: {:?}",
        fl.file.name,
        fl.lines.first().unwrap().line_index
    );

    DummyResult::any(sp)
}

fn expand_textdomain(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = cx.new_parser_from_tts(args);
    let item_name = emittry!(parser.parse_str(), sp).0.as_str();
    emittry!(expect_eof(&mut parser), sp);
    *TEXTDOMAIN.write().unwrap() = Some(item_name.to_string());

    let e = quote_expr!(cx, {
        ::gettextrs::textdomain($item_name);
    });
    MacEager::expr(e)
}

fn expect_eof<'a>(parser: &mut parser::Parser<'a>) -> Result<(), DiagnosticBuilder<'a>> {
    if parser.token != token::Token::Eof {
        parser.unexpected()
    } else {
        Ok(())
    }
}
