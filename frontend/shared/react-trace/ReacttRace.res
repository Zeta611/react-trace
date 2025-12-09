type stateEntry = {label: string, value: string, queue_size: int}

type decisionInfo = {chk: bool, eff: bool}

type rec stree = {
  path: string,
  name: string,
  children: array<stree>,
  st_store?: array<stateEntry>,
  eff_q_size?: int,
  dec?: decisionInfo,
  arg?: string,
  handler?: int,
}

type sourceLoc = {start_line: int, start_col: int, end_line: int, end_col: int}

type entry = {
  msg: string,
  stree: stree,
  source_loc?: sourceLoc,
}
type result = {
  checkpoints?: array<entry>,
  log?: string,
  error?: string,
}

@module("./react-trace.bc.js")
external _run: (int, array<int>, string) => result = "run"

let run = (fuel, events, code) => {
  try {
    let result = _run(fuel, events, code)
    result.checkpoints->Option.forEach(Array.reverse)
    result
  } catch {
  | JsExn(e) => {
      Console.error(e)
      {error: "Runtime error"}
    }
  }
}
