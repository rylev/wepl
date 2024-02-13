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
* `.imports`: print a list of all the component's imports
* `.exports`: print a list of all the component's exports
* `.link $function $wasm`: satisfy the imported function `$func` with an export from the wasm component `$wasm`
* `.compose $adapter`: satisfy imports with the supplied adapter module (e.g., to compose with [`WASI-Virt`](https://github.com/bytecodealliance/WASI-Virt) adapter)
* `.type $type`: inspect a type's `$type` definition in scope
* `.help`: print help information (`?` is alias for this built-in)

## Features

* Exported function evaluation
* Listing imports and exports
* Variable assignment
* Type checking
* Satisfying imports with other WebAssembly Components
* Basic component composition

## Compatibility

`wepl` is currently tied to the wasmtime 17 release. Components that work with that release should work in `wepl`.
