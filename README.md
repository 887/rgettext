# rgettext

A compiler plugin to improve [*GNU Gettext*](https://www.gnu.org/software/gettext/) compatibility with Rust.

## Example

```rust
#![feature(plugin)]
#![plugin(rgettext)]

extern crate gettextrs;
use gettextrs::*;

fn main() {
    setlocale(LocaleCategory::LcAll, std::env::var("LANG").unwrap());
    bindtextdomain(env!("CARGO_PKG_NAME"), "/usr/local/share/locale");
    g!("Hello!");
}
```

Will automatically generate `i18n/<pkgname>.pot` and update all `.po` in `i18n/po/` when compile.

## Usage

Compiler plugin can only be used in Rust nightly, so you need a nightly `rustc`.

Then add the following to your `Cargo.toml`:

```toml
[dependencies]
gettext-rs = "0.3"

[dev-dependencies]
rgettext = "0.1"
```

And your source:

```rust
#![feature(plugin)]
#![plugin(rgettext)]

extern crate gettextrs;
```
