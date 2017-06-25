use std::collections::BTreeMap;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::fs::File;
use std::mem;

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
    pub msgstr: Vec<String>,
}

pub fn parse(path: &str) -> io::Result<BTreeMap<String, Msg>> {
    let file = File::open(path)?;
    let mut result = BTreeMap::new();
    let mut tmp = Msg::default();
    let mut tmp_is_complete = false;
    let mut multi_line: Option<u8> = None;
    for (n, line) in BufReader::new(file).lines().enumerate() {
        let line = line?;
        if line.is_empty() {
            if !tmp_is_complete {
                return Err(make_parse_err("unexpected empty line", path, n));
            }
            tmp_is_complete = false;
            multi_line = None;
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
                    let mut v = parse_reference(&s).map_err(|e| make_parse_err(e, path, n))?;
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
                    let e = make_parse_err(format!("illegal comment line `{}`", x), path, n);
                    return Err(e);
                }
            }
        } else if line.starts_with('"') && line.ends_with('"') && multi_line.is_some() {
            match multi_line.unwrap() {
                0 => tmp.msgctxt.as_mut().unwrap().push_str(&line),
                1 => tmp.msgid.push_str(&line),
                2 => tmp.msgid_plural.as_mut().unwrap().push_str(&line),
                3 => tmp.msgstr.last_mut().unwrap().push_str(&line),
                _ => unreachable!(),
            }
        } else if line.starts_with("msg") {
            let k = line.split_whitespace().next().unwrap();
            match k {
                "msgctxt" => {
                    let ctxt = line.chars().skip(8).collect();
                    if !tmp.msgctxt.is_none() {
                        let e = make_parse_err("duplicate field `msgctxt`", path, n);
                        return Err(e);
                    }
                    tmp.msgctxt = Some(ctxt);
                }
                "msgid" => {
                    let id: String = line.chars().skip(6).collect();
                    if result.contains_key(&id) {
                        let e = make_parse_err(format!("duplicate msgid `{}`", &id), path, n);
                        return Err(e);
                    }
                    if !tmp.msgid.is_empty() {
                        let e = make_parse_err("duplicate field `msgid`", path, n);
                        return Err(e);
                    }
                    tmp.msgid.push_str(&id);
                }
                "msgid_plural" => {
                    let id_plural = line.chars().skip(13).collect();
                    if !tmp.msgid_plural.is_none() {
                        let e = make_parse_err("duplicate field `msgid_plural`", path, n);
                        return Err(e);
                    }
                    tmp.msgid_plural = Some(id_plural);
                }
                _ if k.starts_with("msgstr") => {
                    let iter = k.chars().skip(6);
                }
                _ => {
                    let e = make_parse_err(format!("illegal field `{}`", k), path, n);
                    return Err(e);
                }
            }
            if !tmp_is_complete && !tmp.msgid.is_empty() && !tmp.msgstr.is_empty() {
                tmp_is_complete = true;
            }
        } else {
            let e = make_parse_err("illegal line", path, n);
            return Err(e);
        }
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

fn make_parse_err<E: ::std::fmt::Display>(
    error: E,
    file_name: &str,
    line_number: usize,
) -> io::Error {
    let m = format!("{} at {}:{}", error, file_name, line_number);
    io::Error::new(io::ErrorKind::InvalidData, m)
}
