# WEPL

The WebAssembly Component repl.

## Example

```bash
$ wepl mycomponent.wasm
> .exports
uppercase: func(input: string) -> string
> uppercase
uppercase: func(input: string) -> string
> s = "hello"
s: string
> uppercase(s)
"HELLO"
```

## Built-in Functions

Built-in functions can be called by using the `.` prefix.

Supported functions include:
* `.inspect $item`: inspect an item `$item` in scope (`?` is alias for this built-in)
* `.imports`: print a list of all the component's imports
* `.exports`: print a list of all the component's exports
* `.link $function $wasm`: satisfy the imported function `$func` with an export from the wasm component `$wasm`
* `.compose $adapter`: satisfy imports with the supplied adapter module (e.g., to compose with [`WASI-Virt`](https://github.com/bytecodealliance/WASI-Virt) adapter)

## Features

* Exported function evaluation
* Listing imports and exports
* Variable assignment
* Type checking
* Satisfying imports with other WebAssembly Components
* Basic component composition
