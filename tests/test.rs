#![feature(plugin)]
#![plugin(rgettext)]

extern crate gettextrs;

#[test]
fn g() {
    textdomain!("gettext");
    g!("");
}

#[test]
fn ng() {
    textdomain!("gettext");
    ng!("one", "two", 1);
}
