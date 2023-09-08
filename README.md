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

## Features

* Function Evaluation
* Listing Exports
