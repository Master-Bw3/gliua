# Gliua

Gliua is a library for running Uiua code from gleam 

## Installation

Add `gliua` to to your Gleam project.

```sh
gleam add gliua
```

## Usage

```gleam
  import gliua/builder
  import gliua/runtime
  import gliua/decode

  let eval_result =
    []
    |> builder.push_int(2)
    |> builder.push_int(1)
    |> builder.join()
    |> builder.run_str("⊟.")
    |> builder.evaluate()

  let assert Ok(runtime) = eval_result

  runtime.stack(runtime)
    |> decode.stack_1(decode.rows(decode.rows(decode.int)))
  //Ok([[1, 2], [1, 2]])
```

## Targets

Gliua currently only targets Erlang