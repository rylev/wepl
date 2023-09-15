# Value Literals

This document specifies how value literals for wit types are encoded.

|Type|Example Values
|---|---
|Bools|`true`, `false`
|Ints|`123`, `-9`
|Floats|`3.14`, `nan`, `-inf`
|Chars|`'x'`, `'☃︎'`, `'\x00'`
|Strings|`"abc"`
|Tuples|`(123, "abc")`
|Lists|`[1, 2, 3]`
|Records|`{field-a: 1, field-b: "two"}`
|Variants|`forever`, `days(30)`
|Enums|`south`, `west`
|Options|`"bare-form"`, `some("variant-form")`, `none`
|Results|`ok(1)`, `err("oops")`
|Flags|`{read, write}`
|Resources|`stream(1)`(?)


## Bools

Bools are encoded as literals `false` or `true`.

## Integers

Integers are encoded as JSON numbers (without fraction or exponent).

## Floats

Floats are encoded as JSON numbers or one of three literals
encoding special vaules: `nan`, `inf`, `-inf`

## Chars

Chars are encoded as `'<char>'`, where `<char>` is one of:

- a single UTF-8-encoded USV
- an escaped ASCII char: `\'`, `\"`, `\\`, `\n`, `\r`, `\t`, `\x<2 hex digits>`

> Escapes `\`→`\\` and `'`→`\'` are mandatory for chars.

## Strings

Strings are encoded as a double-quote-delimited sequence of `<char>` (as above).

> Escapes `\` -> `\\` and `""` -> `\"` are mandatory for strings.

## Tuples

Tuples are encoded as a sequence of parenthesized,
comma-separated values. Trailing commas are permitted.

`tuple<u8, string>` -> `(123, "abc",)`

## Lists

Lists are encoded as a sequence of square-bracket-enclosed,
comma-separated values. Trailing commas are permitted.

`list<char>` -> `['a', 'b', 'c',]`

## Records

Records are encoded as a set of curly-brace-enclosed, 
comma-separated entries. Entries consist of a kebab-case
field name, a colon, and a value. Trailing commas are
permitted. Fields with `option` value `none` may be
omitted entirely.

```clike
record example {
  required-field: u8,
  optional-field: option<u8>,
}
```
-> `{required-field: 123}`

## Variants

Variants are encoded as a kebab-case case name. If the
case has a payload, the name is followed by the
parenthesized payload value.

`variant error { eof, other(string) }` -> `other("oops")`

## Enums

Enums are encoded in their variant form.

`enum hand { left, right }` -> `left`

## Options

Options may be encoded in their variant form
(`some(1)`, `none`). A `some` value may also be encoded as
the value itself, which may be applied recursively to nested 
options.

`option<u8>` -> `123` = `some(123)`
`option<option<u8>>` -> `123` = `some(123)` = `some(some(123))`

> Note that certain nested option values can only be
> expressed in the variant encoding form, like `some(none)`.


## Results

Results are encoded in their variant form (`ok(1)`, `err("oops")`). 
An `ok` value may also be encoded as
the value itself, which may be applied recursively to nested 
results.

`result<u8, string>` -> `123` = `ok(123)`
`result<result<u8>, string>` -> `123` = `ok(123)` = `ok(ok(123))`

> Note that certain nested result values can only be
> expressed in the variant encoding form, like `ok(err("oops"))`.


## Flags

Flags are encoded as a set of curly-brace-enclosed,
comma-separated, kebab-case names. Trailing commas are
permitted.

`flags perms { read, write, exec }` -> `{read, write,}`

## Resources

At this time resources cannot be expressed as literals.