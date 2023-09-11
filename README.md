# WEPL

The WebAssembly Component repl.

## Example

```bash
$ wepl mycomponent.wasm
> .exports
hello-world: function
uppercase: function
> ?uppercase
uppercase: func(input: String) -> String
> uppercase("hello")
"HELLO"
```

## Built-in Functions

Built-in functions can be called by using the `.` prefix.

Supported functions include:
* `.inspect`: inspect a name in scope (`?` is alias for this built-in)
* `.imports`: print a list of all the component's imports
* `.exports`: print a list of all the component's exports
* `.link $function $wasm`: satisfy the imported function `$func` with an export from the wasm component `$wasm`

## Features

* Function Evaluation
* Listing Exports
