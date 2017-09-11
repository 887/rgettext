use std::collections::BTreeMap;
use std::mem;
use std::fmt;

use nom::{digit, space};

pub type Po = BTreeMap<MsgIdentifer, Msg>;

#[derive(Debug, Default, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Reference {
    pub file: String,
    pub line: usize,
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.file, self.line)
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
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

impl Msg {
    pub fn id(&self) -> MsgIdentifer {
        MsgIdentifer(self.msgid.clone(), self.msgctxt.clone())
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct MsgIdentifer(pub String, pub Option<String>);

named!(index<usize>, map_res!(
    map_res!(
        digit,
        ::std::str::from_utf8
    ),
    ::std::str::FromStr::from_str
));

named!(esc<String>, map_res!(
    escaped_transform!(take_until_either!("\"\\"), '\\',
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

named!(parse_msg_type<MsgType>, alt!(msgstr | msgctxt | msgid_plural | msgid));

pub fn parse(s: &str) -> Result<Po, ParseError> {
    let mut result = Po::new();
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
            result.insert(msg.id(), msg);
            continue;
        }

        if line.starts_with('#') {
            multi_line = None;
            match line.split_whitespace().next().unwrap() {
                "#" => {
                    let v = line.split_at(1).1.trim().to_owned();
                    tmp.translator_comments.push(v)
                }
                "#." => {
                    let v = line.split_at(2).1.trim().to_owned();
                    tmp.extracted_comments.push(v)
                }
                "#:" => {
                    let s = line.split_at(2).1.trim().to_owned();
                    let mut v = parse_reference(&s).map_err(|e| ParseError::new(e, n))?;
                    tmp.reference.append(&mut v)
                }
                "#," => {
                    let v = line.split_at(2).1.trim().to_owned();
                    tmp.flag.push(v)
                }
                "#|" => {
                    let v = line.split_at(2).1.trim().to_owned();
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
            let s = parse_string(line.as_bytes()).to_full_result().map_err(
                |e| {
                    println!("{:?}", e);
                    ParseError::new("illegal string", n)
                },
            )?;
            match multi_line.unwrap() {
                Msgctxt => tmp.msgctxt.as_mut().unwrap().push_str(&s),
                Msgid => tmp.msgid.push_str(&s),
                MsgidPlural => tmp.msgid_plural.as_mut().unwrap().push_str(&s),
                Msgstr(n) => tmp.msgstr.get_mut(&n).unwrap().push_str(&s),
            }
            continue;
        }
        if let Some(MsgType::Msgid) = multi_line {
            if result.contains_key(&tmp.id()) {
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
            let s = parse_string(next).to_full_result().map_err(|_| {
                ParseError::new("illegal string", n)
            })?;
            match t {
                Msgctxt => {
                    if tmp.msgctxt.is_some() {
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
                    if tmp.msgid_plural.is_some() {
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
    if tmp_is_complete {
        result.insert(tmp.id(), tmp);
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
            let line = line.unwrap().parse().map_err(
                move |_| "illegal line number of reference",
            )?;
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

impl fmt::Display for MsgType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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

#[allow(unused_variables)]
pub fn merge_po(mut old: Po, mut new: Po) -> Po {
    for (id, msg) in new.iter_mut() {
        if let Some(Msg {
                        translator_comments,
                        extracted_comments,
                        reference,
                        flag,
                        previous,
                        msgctxt,
                        msgid,
                        msgid_plural,
                        mut msgstr,
                    }) = old.remove(&id)
        {
            msg.translator_comments.extend(translator_comments);
            if msg.msgid_plural.is_some() {
                msg.msgstr.extend(msgstr);
            } else {
                msg.msgstr.insert(0, msgstr.remove(&0).unwrap());
            }
        }
    }
    if let Some(metadata) = old.remove(&MsgIdentifer("".into(), None)) {
        new.insert(metadata.id(), metadata);
    }
    new
}

pub fn to_string(po: Po) -> String {
    use std::fmt::Write;
    let mut result = String::new();
    for Msg {
        translator_comments,
        extracted_comments,
        reference,
        flag,
        previous,
        msgctxt,
        msgid,
        msgid_plural,
        mut msgstr,
    } in po.into_iter().map(|v| v.1)
    {
        for line in translator_comments {
            if line.is_empty() {
                writeln!(&mut result, "#").unwrap();
            } else {
                writeln!(&mut result, "# {}", line).unwrap();
            }
        }
        for line in extracted_comments {
            if line.is_empty() {
                writeln!(&mut result, "#.").unwrap();
            } else {
                writeln!(&mut result, "#. {}", line).unwrap();
            }
        }
        write_comment_with_limit(&mut result, "#:", " ", 80, &reference).unwrap();
        write_comment_with_limit(&mut result, "#,", ", ", 80, &flag).unwrap();
        for line in previous {
            writeln!(&mut result, "#| {}", line).unwrap();
        }
        if let Some(v) = msgctxt {
            writeln!(&mut result, "msgctxt {}", v).unwrap();
        }
        write_string_with_limit(&mut result, "msgid", 80, msgid).unwrap();
        if let Some(v) = msgid_plural {
            write_string_with_limit(&mut result, "msgid_plural", 80, v).unwrap();
            for (i, s) in msgstr {
                write_string_with_limit(&mut result, &format!("msgstr[{}]", i), 80, s).unwrap();
            }
        } else {
            write_string_with_limit(&mut result, "msgstr", 80, msgstr.remove(&0).unwrap()).unwrap();
        }
        result.push('\n');
    }
    result
}

fn write_comment_with_limit<W: fmt::Write, T: ToString>(
    dst: &mut W,
    tag: &str,
    separator: &str,
    width_limit: usize,
    data: &[T],
) -> Result<(), fmt::Error> {
    if data.is_empty() {
        return Ok(());
    }
    let mut width = 0;
    for comment in data {
        let comment = comment.to_string();
        let comment_width = comment.chars().count();
        if width == 0 {
            write!(dst, "{} {}", tag, comment)?;
            width += comment_width;
        } else if width + comment_width <= width_limit {
            write!(dst, "{}{}", separator, comment)?;
            width += comment_width;
        } else {
            write!(dst, "\n{} {}",tag, comment)?;
            width = comment_width;
        }
    }
    write!(dst, "\n")
}

fn write_string_with_limit<W: fmt::Write>(
    dst: &mut W,
    keyword: &str,
    width_limit: usize,
    s: String,
) -> Result<(), fmt::Error> {
    write!(dst, "{} ", keyword)?;
    if s.chars().count() > width_limit {
        writeln!(dst, "\"\"")?;
    }
    let mut width = 0;
    let mut buf = String::new();
    dst.write_char('"')?;
    for c in s.chars() {
        match c {
            '\n' => {
                write!(dst, "{}\\n\"\n\"", buf)?;
                buf.clear();
                width = 0;
            }
            '\\' => buf.push_str(r#"\\"#),
            '"' => buf.push_str(r#"\""#),
            ' ' => {
                let buf_width = buf.chars().count();
                if width + buf_width > width_limit {
                    if width == 0 {
                        write!(dst, "{} \"\n\"", buf)?;
                        width = 0;
                    } else {
                        write!(dst, "\"\n\"{} ", buf)?;
                        width = buf_width;
                    }
                } else {
                    write!(dst, "{} ", buf)?;
                    width += buf_width + 1;
                }
                buf.clear();
            }
            c => buf.push(c),
        }
    }
    writeln!(dst, "{}\"", buf)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_write_string_with_limit() {
        let mut result = String::new();
        let src = "blahblahblah blahblah\n\
                   blah blah blah blah\n\
                   blah\n\
                   \"blah\""
            .to_string();
        write_string_with_limit(&mut result, "msgid", 10, src).unwrap();
        let dst = r#"msgid ""
"blahblahblah "
"blahblah\n"
"blah blah "
"blah blah\n"
"blah\n"
"\"blah\""
"#
            .to_string();
        assert_eq!(result, dst);
    }

    #[test]
    fn test_parse() {
        let src = r#"#: file.rs:1
msgid "id"
msgstr "str"
"#;
        let res = parse(src).unwrap();
        let mut dst = Po::new();
        let mut msg = Msg::default();
        msg.msgid = "id".into();
        msg.msgstr.insert(0, "str".into());
        msg.reference.push(Reference {
            file: "file.rs".into(),
            line: 1,
        });
        dst.insert(msg.id(), msg);

        assert_eq!(res, dst);
    }

    #[test]
    fn test_parse_string() {
        let r = parse_string(b"\"\"").to_full_result().unwrap();
        assert_eq!(r, "".to_string());
    }

    #[test]
    fn test_parse_esc() {
        let r = esc(r#"😺\n"#.as_bytes()).to_full_result().unwrap();
        assert_eq!(r, "😺\n".to_string());
    }
}
