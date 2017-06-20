#![feature(plugin)]
#![plugin(rgettext)]

extern crate gettextrs;

#[test]
fn base() {
    textdomain!("gettext");
    g!();
}

#[test]
fn base2() {
    textdomain!("gettext");
    g!();
}
