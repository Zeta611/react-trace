type stateEntry = {
  "label": string,
  "value": string,
  "queue_size": int,
}

type decisionInfo = {
  "chk": bool,
  "eff": bool,
}

type rec tree = {
  "path": string,
  "name": string,
  "children": array<tree>,
  "st_store": option<array<stateEntry>>,
  "eff_q_size": option<int>,
  "dec": option<decisionInfo>,
  "arg": option<string>,
}

type sourceLoc = {
  "start_line": int,
  "start_col": int,
  "end_line": int,
  "end_col": int,
}

type entry = {
  msg: string,
  tree: tree,
  source_loc: option<sourceLoc>,
}
type result = {
  checkpoints?: array<entry>,
  log?: string,
  error?: string,
}

@module("./react-trace.bc.js")
external _run: (int, string) => result = "run"

let run = (fuel, value) => {
  try {
    let result = _run(fuel, value)
    result.checkpoints->Option.forEach(Array.reverse)
    result
  } catch {
  | Exn.Error(e) => {
      Console.error(e)
      {error: "Runtime error"}
    }
  }
}
