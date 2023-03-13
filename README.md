# macro-rules-rt

[![Crates.io](https://img.shields.io/crates/v/macro-rules-rt.svg)](https://crates.io/crates/macro-rules-rt)
[![Docs.rs](https://docs.rs/macro-rules-rt/badge.svg)](https://docs.rs/macro-rules-rt/)
[![Actions Status](https://github.com/frozenlib/macro-rules-rt/workflows/CI/badge.svg)](https://github.com/frozenlib/macro-rules-rt/actions)

`macro-rules-rt` is a crate that transforms Rust source code using the syntax used in `macro-rules`.

## Example

```rust
use macro_rules_rt::{Matcher, Rule, Transcriber};
use quote::quote;

let from = Matcher::from_token_stream(quote!($e:literal))?;
let to = Transcriber::from_token_stream(quote!($e * 5))?;
let rule = Rule::new(from, to)?;
let source = quote!(1 + 2);
let result = rule.replace_all(source);
assert_eq!(result.to_string(), quote!(1 * 5 + 2 * 5).to_string());
Ok(())
```

## License

This project is dual licensed under Apache-2.0/MIT. See the two LICENSE-\* files for details.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
