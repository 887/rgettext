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
extern crate chrono;

use std::sync::RwLock;
use std::fs::{create_dir, File};
use std::path::Path;
use std::env;
use std::io::prelude::*;

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

mod po;

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

static POT_DEFAULT: &str = r#"# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"Report-Msgid-Bugs-To: \n"
"POT-Creation-Date: YEAR-MO-DA HO:MI+ZONE\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"

"#;

lazy_static! {
    static ref TEXTDOMAIN: RwLock<String> = RwLock::new(env::var("CARGO_PKG_NAME").unwrap());
    static ref POT: RwLock<Vec<po::Msg>> = RwLock::new(Vec::new());
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
    fn check_crate(&mut self, _cx: &LateContext<'a, 'tcx>, krate: &'tcx Crate) {
        let is_bin_crate = krate.items.iter().any(|(_id, item)| {
            item.attrs
                .iter()
                .any(|attr| attr.path.to_string() == "main")
        });
        if !is_bin_crate {
            return; // Do not create .pot for a library
        }
        let dir = format!("{}/{}", env::var("CARGO_MANIFEST_DIR").unwrap(), "i18n");
        if !Path::new(&dir).exists() {
            create_dir(&dir).unwrap();
        }
        let pot = format!("{}/{}.pot", &dir, &*TEXTDOMAIN.read().unwrap());
        let pot_path = Path::new(&pot);
        if !pot_path.exists() {
            let mut file = File::create(&pot_path).unwrap();
            file.write(POT_DEFAULT.as_bytes()).unwrap();
        }
        let date = chrono::Local::now().format("%F %R%z").to_string();
        // TODO: Parse and update .pot file

        println!("textdomain: {}", &*TEXTDOMAIN.read().unwrap());
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

#[derive(Clone, Copy)]
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
    let refence = po::Reference {
        file: fl.file.name.to_owned(),
        line: fl.lines.first().unwrap().line_index,
    };
    let msg = po::Msg {
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
