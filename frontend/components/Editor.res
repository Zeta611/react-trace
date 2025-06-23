@@directive("'use client';")

let sample = `
let C x =
  let (s, setS) = useState x in
  if s = 42 then
    setS (fun s -> s + 1);
  s
;;
let D _ =
  let (s, setS) = useState true in
  useEffect (setS (fun _ -> false));
  [C 42, if s then 1 else 0]
;;
D ()
`->String.trim

let useCenteredTree = () => {
  open ReactD3Tree
  let (translate, setTranslate) = React.useState(() => {x: 0., y: 0.})
  let containerRef = ReactDOM.Ref.callbackDomRef(container => {
    switch container {
    | Value(container) => {
        open Webapi.Dom
        let rect = container->Element.getBoundingClientRect
        let width = rect->DomRect.width
        let height = rect->DomRect.height
        let x = width /. 2.
        let y = height /. 10.
        if translate.x !== x || translate.y !== y {
          setTranslate(_ => {x, y})
        }
      }
    | _ => ()
    }
  })
  (translate, containerRef)
}

let javascript = LangJavaScript.javascript()
let vim = CodeMirrorVim.vim()

@module("../shared/syntax/language.js")
external core: 'lang = "core"
let coreLang = core()

let replaceEmojis = (s: string) =>
  s
  ->String.replace(":event:", "⚡")
  ->String.replace(":retry:", "🔁")
  ->String.replace(":check:", "🏗️")
  ->String.replace(":finish:", "✅")
  ->String.replace(":cancel:", "⏩")
  ->String.replace(":effects:", "⚙️")
  ->String.replace(":default:", "🔄")

@react.component
let make = () => {
  let fuel = 0 // 0 means unlimited fuel
  let (code, setCode) = React.useState(() => sample)
  let (recording, setRecording) = React.useState(() => ReacttRace.run(fuel, code))

  let (currentStep, setCurrentStep) = React.useState(() => 0)
  let (report, treeData, steps) = switch recording {
  | {checkpoints} => {
      open ReacttRace
      (
        checkpoints
        ->Array.map(x => x.msg->replaceEmojis)
        ->Array.slice(~start=0, ~end=currentStep)
        ->Array.join("\n"),
        if currentStep == 0 {
          ({}: ReactD3Tree.data)
        } else {
          // Field names are shared, so we can use the same record type
          switch checkpoints[currentStep - 1] {
          | Some({tree}) => tree->Obj.magic
          | None => ({}: ReactD3Tree.data) // Intermediate state
          }
        },
        checkpoints->Array.length,
      )
    }
  | {error} => (error, {}, 0)
  | _ => assert(false)
  }

  let onChange = value => {
    setCode(_ => value)
    setCurrentStep(_ => steps)
    setRecording(_ => ReacttRace.run(fuel, value))
  }

  let (jsMode, setJSMode) = React.useState(() => false)
  let (vimMode, setVimMode) = React.useState(() => false)
  let extensions = []->Array.concatMany([
    // vim extension should be the first one
    if vimMode {
      [vim]
    } else {
      []
    },
    if jsMode {
      [javascript]
    } else {
      [coreLang]
    },
    [CodeMirrorThemeTokyoNightDay.tokyoNightDay, HookLabelPlugin.plugin],
  ])

  let (translate, containerRef) = useCenteredTree()

  <Resizable.PanelGroup direction="vertical">
    <Resizable.Panel>
      <Resizable.PanelGroup direction="horizontal">
        <Resizable.Panel>
          <div className="flex h-full flex-col">
            <div
              className="pb-1 pr-3 flex place-content-end items-center gap-x-4 text-sm font-medium">
              <div className="flex gap-x-2">
                <Checkbox
                  id="check-vim-mode"
                  checked=vimMode
                  onCheckedChange={checked => setVimMode(_ => checked)}
                />
                <label
                  htmlFor="check-vim-mode"
                  className="leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
                  {"Vim Keymap"->React.string}
                </label>
              </div>
              <div className="flex gap-x-2">
                <Checkbox
                  id="check-js-mode"
                  checked=jsMode
                  onCheckedChange={checked => setJSMode(_ => checked)}
                />
                <label
                  htmlFor="check-js-mode"
                  className="leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70">
                  {"JS Syntax"->React.string}
                </label>
              </div>
            </div>
            <ReactCodeMirror
              value=code onChange extensions className="h-full text-base font-mono" height="100%"
            />
          </div>
        </Resizable.Panel>
        <Resizable.Handle withHandle=true />
        <Resizable.Panel>
          <div className="rounded-lg h-full resize-y overflow-hidden border" ref=containerRef>
            <ReactD3Tree translate data=treeData orientation="vertical" />
          </div>
        </Resizable.Panel>
      </Resizable.PanelGroup>
    </Resizable.Panel>
    <Resizable.Handle withHandle=true />
    <Resizable.Panel>
      <div className="flex flex-col h-full gap-4 p-3">
        <Slider
          className="w-full"
          value=[currentStep]
          onValueChange={vs => setCurrentStep(_ => vs->Array.getUnsafe(0))}
          step=1
          max=steps
        />
        <div className="text-base font-sans text-gray-800 whitespace-pre-wrap">
          {report->React.string}
        </div>
      </div>
    </Resizable.Panel>
  </Resizable.PanelGroup>
}
