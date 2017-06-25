#![feature(plugin)]
#![plugin(rgettext)]

extern crate gettextrs;

#[test]
fn base() {
    g!("text");
    ng!("one", "two", 1);
    dg!("domain", "text");
    dng!("domain", "one", "two", 1);
    dcg!("domain", "text", gettextrs::LocaleCategory::LcAll);
    dcng!("domain", "one", "two", 1, gettextrs::LocaleCategory::LcAll);
}
