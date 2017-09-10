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
#[macro_use]
extern crate nom;

use std::sync::RwLock;
use std::fs::{create_dir, File};
use std::path::Path;
use std::env;
use std::io::prelude::*;
use std::collections::BTreeMap;

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

use chrono::prelude::*;

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

/// `try!` for plugin macro
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
    static ref TEXTDOMAIN: RwLock<String> = RwLock::new(env::var("CARGO_PKG_NAME").unwrap());
    static ref POT_BUF: RwLock<Vec<po::Msg>> = RwLock::new(Vec::new());
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
            item.attrs.iter().any(
                |attr| attr.path.to_string() == "main",
            )
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
            create_default_pot_file(&pot_path).unwrap();
        }
        let mut pot_file = File::open(&pot_path).unwrap();
        let mut pot_contents = String::new();
        pot_file.read_to_string(&mut pot_contents).unwrap();
        let pot = po::parse(&pot_contents).unwrap();
        let new_pot = merge_pot_buf(&mut POT_BUF.write().unwrap());
        let pot = po::merge_po(pot, new_pot);
        let mut pot_file = File::create(&pot_path).unwrap();
        pot_file.write_all(po::to_string(pot).as_bytes()).unwrap();

        // TODO: merge all language .po
    }
}

fn create_default_pot_file(path: &Path) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    let pkg_name = env::var("CARGO_PKG_NAME").unwrap();
    let pkg_version = env::var("CARGO_PKG_VERSION").unwrap();
    let homepage = env::var("CARGO_PKG_HOMEPAGE").unwrap();
    let authors = env::var("CARGO_PKG_AUTHORS").unwrap();
    let authors = authors.split(":");
    let year = chrono::Local::now().year();
    let date = chrono::Local::now().format("%F %R%z").to_string();

    writeln!(
        &mut file,
        "# This file is distributed under the same license as the {} package.",
        pkg_name
    )?;
    for author in authors {
        writeln!(&mut file, "# {}, {}.", author, year)?;
    }
    writeln!(&mut file, "#")?;
    writeln!(&mut file, "#, fuzzy")?;
    writeln!(&mut file, r#"msgid """#)?;
    writeln!(&mut file, r#"msgstr """#)?;
    writeln!(
        &mut file,
        r#""Project-Id-Version: {} {}\n""#,
        pkg_name,
        pkg_version
    )?;
    writeln!(&mut file, r#""Report-Msgid-Bugs-To: {}\n""#, homepage)?;
    writeln!(&mut file, r#""POT-Creation-Date: {}\n""#, date)?;
    writeln!(&mut file, r#""PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n""#)?;
    writeln!(
        &mut file,
        r#""Last-Translator: FULL NAME <EMAIL@ADDRESS>\n""#
    )?;
    writeln!(&mut file, r#""Language-Team: LANGUAGE <LL@li.org>\n""#)?;
    writeln!(&mut file, r#""Language: \n""#)?;
    writeln!(&mut file, r#""MIME-Version: 1.0\n""#)?;
    writeln!(&mut file, r#""Content-Type: text/plain; charset=UTF-8\n""#)?;
    writeln!(&mut file, r#""Content-Transfer-Encoding: 8bit\n""#)?;

    Ok(())
}

#[allow(unused_variables)]
fn merge_pot_buf(buf: &mut Vec<po::Msg>) -> po::Po {
    let mut result = po::Po::new();
    while let Some(po::Msg {
                       translator_comments,
                       extracted_comments,
                       reference,
                       flag,
                       previous,
                       msgctxt,
                       msgid,
                       msgid_plural,
                       msgstr,
                   }) = buf.pop()
    {
        let id = po::MsgIdentifer(msgid.clone(), msgctxt.clone());
        let msg = result.entry(id).or_insert_with(po::Msg::default);
        msg.msgid = msgid;
        msg.msgctxt = msgctxt;
        msg.translator_comments.extend(translator_comments);
        msg.extracted_comments.extend(extracted_comments);
        msg.reference.extend(reference);
        msg.flag.extend(flag);
        msg.previous.extend(previous);
        if let Some(v) = msgid_plural {
            msg.msgid_plural.get_or_insert(v);
        }
        msg.msgstr.insert(0, "".into());
    }
    result
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
        msgstr: BTreeMap::new(),
    };
    POT_BUF.write().unwrap().push(msg);
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
