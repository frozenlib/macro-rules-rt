# macro-rules-emulator

[![Crates.io](https://img.shields.io/crates/v/macro-rules-emulator.svg)](https://crates.io/crates/macro-rules-emulator)
[![Docs.rs](https://docs.rs/macro-rules-emulator/badge.svg)](https://docs.rs/macro-rules-emulator/)
[![Actions Status](https://github.com/frozenlib/macro-rules-emulator/workflows/CI/badge.svg)](https://github.com/frozenlib/macro-rules-emulator/actions)

`macro-rules-emulator` is an emulator of Rust's `macro_rules` and can be used to convert `TokenStream`.

## Example

```rust
use macro_rules_emulator::{Pattern, Rule};
use quote::quote;

let from = Pattern::new(quote!($e:literal))?;
let to = Rule::new(from, quote!($e * 5))?;
let source = quote!(1 + 2);
let result = to.replace_all(source);
assert_eq!(result.to_string(), quote!(1 * 5 + 2 * 5).to_string());
```

## License

This project is dual licensed under Apache-2.0/MIT. See the two LICENSE-\* files for details.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
