<img src="https://github.com/user-attachments/assets/8d07d3bd-1dc7-4156-9a40-314a38ebe540" alt="React-tRace logo">

React-tRace is a React hooks reference interpreter based on a formal semantics. It interprets React-like components, tracking render cycles, state updates, and effect executions to detect inefficient re-renders at runtime.

---

[![Builds, tests & co](https://github.com/React-Analysis/react-trace/actions/workflows/ci.yml/badge.svg)](https://github.com/React-Analysis/react-trace/actions/workflows/ci.yml)

## What does it do?

- **Interprets React components**: Executes a subset of React written in ML-like syntax or (experimental) JS syntax
- **Tracks rendering behavior**: Models component renders, state updates, and effect execution
- **Detects inefficiencies**: Identifies unnecessary re-renders and problematic update patterns

React-tRace implements core React hooks functionality (useState, useEffect) and maintains a component tree to model how React would process your components.

## Developing

[OCaml 5.2.0](https://ocaml.org/releases/5.2.0) should be installed.

```sh
opam update
opam switch create . ocaml-base-compiler.5.2.0
```

Confirm that the new opam switch is activated using `opam switch` and install the dependencies:

```sh
opam install --deps-only --with-test .
```

## Build

React-tRace can be built for both native and JavaScript (via Js_of_ocaml) targets:

### Native Build

```sh
dune build bin/native/main.exe
```

This builds the native executable that you can run on your local machine. Add `--release` flag for optimized output.

### JavaScript Build

```sh
dune build bin/js/main.bc.js
```

This builds a JavaScript version using Js_of_ocaml, which can be used in web browsers or Node.js environments. Add `--release` flag for optimized output.

### Full Build

To build everything:

```sh
dune build
```

## Running

You can run React-tRace with dune:

```sh
dune exec react_trace -- samples/simple.ml
```

Or directly with the built executable:

```sh
./_build/default/bin/native/main.exe samples/simple.ml
```

## Testing

Tests can be run with:

```sh
dune runtest
```

The corresponding React/JS tests are available [here](https://anonymous.4open.science/r/react-trace-testsuites-2AF8/README.md).

## Command Line Options

React-tRace supports several command-line options:

- `-pp`: Pretty-print the program in S-expression format
- `-parse-js`: Parse JavaScript/TypeScript files using Flow
- `-verbose`: Enable verbose logging for detailed output
- `-report`: Generate and display view trees during execution
- `-fuel [n]`: Limit execution to a specified number of steps

Example with options:

```sh
dune exec react_trace -- -report samples/complex.ml
```

## Writing React-like Components

React-tRace supports writing React-like components in an ML-like syntax. Here's a simple example:

```ocaml
let Counter initial =
  let (count, setCount) = useState initial in
  view [count]
;;
view [Counter 0]
```

### Supported Features

- **Components**: Define using ML-style `let Component arg = ...` syntax
- **Hooks**:
  - `useState`: `let (state, setState) = useState initialValue`
  - `useEffect`: `useEffect (body)`
    - `body` does not need to be wrapped in a callback
- **View elements**: Use `view [child1, child2, ...]` to render components

### Examples

A component with state and effect:

```ocaml
let Counter initial =
  let (count, setCount) = useState initial in
  useEffect (setCount (fun c -> c + 1));
  view [count]
;;
view [Counter 0]
```

Conditional rendering:

```ocaml
let Component x =
  if x > 0 then
    view [42]
  else
    view [0]
;;
```

React-tRace also supports parsing JavaScript/TypeScript files with the `-parse-js` option, enabling you to analyze React code closer to what you'd write in a real application.

## Acknowledments

I thank [Susana Balderas](https://susybachi.my.canva.site/) for the awesome logo!
