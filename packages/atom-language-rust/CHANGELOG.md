# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning][].

## [0.10.0][] - 2016-10-02

### Adds
- Convenience snippets for the `break` and `continue` keywords ([#9][]).

## [0.9.0][] - 2016-08-20

### Changes
- Match rules for block comments to identify nested blocks so that they
  are styled consistently.

### Fixes
- A package activation error for certain distributions (namely via
  `community` from AUR) of Atom ([#8][]).

## [0.8.1][] - 2016-07-07

### Fixes
- Matching for certain keyword expressions that look like macro invocations
  ([#7][]).

## [0.8.0][] - 2016-05-21

### Adds
- Match rules for escape characters inside character and string literals.
- Configuration option to style token characters.

### Fixes
- Matching for function parameter names from terminating early when a
  tuple parameter type is encountered.
- Matching for `where` clauses that begin on another line to the item which
  they are specified for ([#6][]).

## [0.7.0][] - 2016-04-17

### Adds
- Various match rules to cover punctuation characters to better support
  syntax themes that base their styling around them.
- Matching for metavariables (macros) inside item definition match groups.

### Changes
- Match rules for macros-related syntax to be simpler, which includes the
  removal of some scope names.
- Matching for macro invocations to no longer be restricted to only those
  part of the Rust standard library.

### Removes
- Matching for the `Sized` trait.

### Fixes
- Comment matching for function related blocks.
- Matching for function types inside certain groups.

## [0.6.0][] - 2016-03-27

### Adds
- Separated match rule for `mod` definitions.
- Separated match rule for the `where` clause.
- Match rule for type parameters.
- Match rule for tuple struct parameters.
- Match rule for modifier-like keywords.
- Match rule for function parameters.
- Match rule for `impl` definitions.

### Changes
- Matching for the `as` keyword to be considered in `use` statements.
- Match rules for attributes to be combined into one that covers both inner
  and outer attributes along with attribute literal values.
- Names of macro definitions to use same scope as functions.
- Scope names assigned to several language items.

### Fixes
- Comment matching across many multi-line match rules that were not catching
  comments at all.
- Toggle line comment command in Atom ([#3][]).

## [0.5.0][] - 2016-03-05

### Adds
- ESLint configuration for the project.

### Changes
- Primitive type matching to be ignored when part of a path.
- Grammar scope assigned to `self` and `Self`.

### Fixes
- A reported package activation error ([#1][]).
- Leading documentation block lines not having Markdown constructs matched.
- Linting errors resulting from ESLint configuration.

### Removes
- Matching for `String`, raw pointers and lambda expressions.

## [0.4.0][] - 2016-02-21

### Adds
- Basic set of code snippets.
- Some test specifications for byte and character literals.
- Match rules for Markdown in documentation blocks.
- Configuration option to style documentation blocks.
- Error suppression for bad configuration option values.

## [0.3.0][] - 2016-02-14

### Adds
- Integration with Travis CI.
- Basic test specifications for attributes.
- Ability to toggle and configure styles for extended grammar.
- Match rules for macros grammar.
- Parameter matching for `where` clauses.

### Changes

- Parameter and lifetime matching to be more robust.

## [0.2.0][] - 2016-02-06

### Adds
- Match rules for hexadecimal, octal and binary literals.
- A project change log.

### Changes
- Attribute matching to be more flexible.
- Byte, character and string literal matching to be more flexible and to
  also mark invalid values.
- Auto-indentation settings to be more user friendly.

### Fixes
- Incorrect matching of namespace paths as function parameters.

## 0.1.0 - 2016-01-31

### Adds
- Initial project files.

[0.10.0]: https://github.com/miqid/atom-language-rust/compare/0.9.0...0.10.0
[0.9.0]: https://github.com/miqid/atom-language-rust/compare/0.8.1...0.9.0
[0.8.1]: https://github.com/miqid/atom-language-rust/compare/0.8.0...0.8.1
[0.8.0]: https://github.com/miqid/atom-language-rust/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/miqid/atom-language-rust/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/miqid/atom-language-rust/compare/0.5.0...0.6.0
[0.5.0]: https://github.com/miqid/atom-language-rust/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/miqid/atom-language-rust/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/miqid/atom-language-rust/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/miqid/atom-language-rust/compare/0.1.0...0.2.0
[Semantic Versioning]: http://semver.org/

[#8]: https://github.com/miqid/atom-language-rust/issues/8
[#7]: https://github.com/miqid/atom-language-rust/issues/7
[#6]: https://github.com/miqid/atom-language-rust/issues/6
[#3]: https://github.com/miqid/atom-language-rust/issues/3
[#1]: https://github.com/miqid/atom-language-rust/issues/1

[#9]: https://github.com/miqid/atom-language-rust/pull/9
