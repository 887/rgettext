use std::collections::BTreeMap;
use std::mem;

use nom::{digit, space};

#[derive(Debug, Default)]
pub struct Reference {
    pub file: String,
    pub line: usize,
}

#[derive(Debug, Default)]
pub struct Msg {
    pub translator_comments: Vec<String>, // #
    pub extracted_comments: Vec<String>, // #.
    pub reference: Vec<Reference>, // #:
    pub flag: Vec<String>, // #,
    pub previous: Vec<String>, // #|
    pub msgctxt: Option<String>,
    pub msgid: String,
    pub msgid_plural: Option<String>,
    pub msgstr: BTreeMap<usize, String>,
}

named!(index<usize>, map_res!(
    map_res!(
        digit,
        ::std::str::from_utf8
    ),
    ::std::str::FromStr::from_str
));

named!(esc<String>, map_res!(
    escaped_transform!(alt!(tag!("\\\"") | is_not!("\"")), '\\',
                       alt!(
                           tag!("\\")   => { |_| &b"\\"[..] }
                           | tag!("\"") => { |_| &b"\""[..] }
                           | tag!("n")  => { |_| &b"\n"[..] }
                       )),
    String::from_utf8));

named!(parse_string<String>, do_parse!(
    many0!(space)
        >> s: delimited!(tag!("\""), opt!(esc), tag!("\""))
        >> many0!(space)
        >> (s.unwrap_or_default())
));

named!(msgstr<MsgType>, do_parse!(
    tag!("msgstr")
        >> n: opt!(delimited!(tag!("["), index, tag!("]")))
        >> (MsgType::Msgstr(n.unwrap_or(0)))
));

named!(msgctxt<MsgType>, do_parse!(tag!("msgctxt") >> (MsgType::Msgctxt)));
named!(msgid<MsgType>, do_parse!(tag!("msgid") >> (MsgType::Msgid)));
named!(msgid_plural<MsgType>, do_parse!(tag!("msgid_plural") >> (MsgType::MsgidPlural)));

named!(parse_msg_type<MsgType>, alt!(msgstr | msgctxt | msgid | msgid_plural));

pub fn parse(s: &str) -> Result<BTreeMap<String, Msg>, ParseError> {
    let mut result = BTreeMap::new();
    let mut tmp = Msg::default();
    let mut tmp_is_complete = false;
    let mut multi_line: Option<MsgType> = None;
    let mut had_msgid = false;
    for (n, line) in s.lines().enumerate() {
        let n = n + 1;
        if line.is_empty() {
            if !tmp_is_complete {
                return Err(ParseError::new("unexpected empty line", n));
            }
            tmp_is_complete = false;
            multi_line = None;
            had_msgid = false;
            let mut msg = Msg::default();
            mem::swap(&mut msg, &mut tmp);
            let id = msg.msgid.clone();
            result.insert(id, msg);
            continue;
        }

        if line.starts_with('#') {
            multi_line = None;
            match line.split_whitespace().next().unwrap() {
                "#" => {
                    let v = line.trim_left_matches("# ").to_owned();
                    tmp.translator_comments.push(v)
                }
                "#." => {
                    let v = line.trim_left_matches("#. ").to_owned();
                    tmp.extracted_comments.push(v)
                }
                "#:" => {
                    let s = line.trim_left_matches("#: ").to_owned();
                    let mut v = parse_reference(&s).map_err(|e| ParseError::new(e, n))?;
                    tmp.reference.append(&mut v)
                }
                "#," => {
                    let v = line.trim_left_matches("#, ").to_owned();
                    tmp.flag.push(v)
                }
                "#|" => {
                    let v = line.trim_left_matches("#| ").to_owned();
                    tmp.previous.push(v)
                }
                x => {
                    let e = ParseError::new(format!("illegal comment line `{}`", x), n);
                    return Err(e);
                }
            }
            continue;
        }
        if line.starts_with('"') && line.ends_with('"') && multi_line.is_some() {
            use self::MsgType::*;
            let s = parse_string(line.as_bytes())
                .to_result()
                .map_err(|e|{println!("{:?}", e); ParseError::new("illegal string", n)})?;
            match multi_line.unwrap() {
                Msgctxt => tmp.msgctxt.as_mut().unwrap().push_str(&s),
                Msgid => tmp.msgid.push_str(&s),
                MsgidPlural => tmp.msgid_plural.as_mut().unwrap().push_str(&s),
                Msgstr(n) => tmp.msgstr.get_mut(&n).unwrap().push_str(&s),
            }
            continue;
        }
        if let Some(MsgType::Msgid) = multi_line {
            if result.contains_key(&tmp.msgid) {
                let e = ParseError::new(format!("duplicate msgid `{}`", tmp.msgid), n - 1);
                return Err(e);
            }
        }
        if line.starts_with("msg") {
            use self::MsgType::*;
            let r = parse_msg_type(line.as_bytes());
            if r.is_err() {
                let e = ParseError::new("illegal field", n);
                return Err(e);
            }
            let (next, t) = r.unwrap();
            let s = parse_string(next)
                .to_result()
                .map_err(|_| ParseError::new("illegal string", n))?;
            match t {
                Msgctxt => {
                    if !tmp.msgctxt.is_none() {
                        return t.to_duplicate_err(n);
                    }
                    tmp.msgctxt = Some(s);
                    multi_line = Some(t);
                }
                Msgid => {
                    if had_msgid {
                        return t.to_duplicate_err(n);
                    }
                    had_msgid = true;
                    tmp.msgid.push_str(&s);
                    multi_line = Some(t);
                }
                MsgidPlural => {
                    if !tmp.msgid_plural.is_none() {
                        return t.to_duplicate_err(n);
                    }
                    tmp.msgid_plural = Some(s);
                    multi_line = Some(t);
                }
                Msgstr(n) => {
                    if tmp.msgstr.contains_key(&n) {
                        return t.to_duplicate_err(n);
                    }
                    tmp.msgstr.insert(n, s);
                    multi_line = Some(t);
                }
            }
            if !tmp_is_complete && had_msgid && !tmp.msgstr.is_empty() {
                tmp_is_complete = true;
            }
            continue;
        }
        let e = ParseError::new("illegal line", n);
        return Err(e);
    }
    Ok(result)
}

fn parse_reference(s: &str) -> Result<Vec<Reference>, &'static str> {
    s.split_whitespace()
        .map(move |r| {
            let mut iter = r.split(':');
            let file = iter.next();
            let line = iter.next();
            let eof = iter.next();
            if file.is_none() || line.is_none() || eof.is_some() {
                return Err("illegal reference");
            }
            let file = file.unwrap().to_string();
            let line = line.unwrap()
                .parse()
                .map_err(move |_| "illegal line number of reference")?;
            Ok(Reference { file, line })
        })
        .collect()
}

#[derive(Copy, Clone)]
enum MsgType {
    Msgctxt,
    Msgid,
    MsgidPlural,
    Msgstr(usize),
}

impl MsgType {
    fn to_duplicate_err<R>(&self, line: usize) -> Result<R, ParseError> {
        Err(ParseError::new(format!("duplicate field `{}`", self), line))
    }
}

impl ::std::fmt::Display for MsgType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        use self::MsgType::*;
        match *self {
            Msgctxt => write!(f, "msgctxt"),
            Msgid => write!(f, "msgid"),
            MsgidPlural => write!(f, "msgid_plural"),
            Msgstr(n) => write!(f, "msgstr[{}]", n),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub error: String,
    pub line: usize,
}

impl ParseError {
    fn new<E: Into<String>>(error: E, line: usize) -> ParseError {
        ParseError {
            error: error.into(),
            line: line,
        }
    }
}
