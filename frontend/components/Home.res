@@directive("'use client';")

@val external process: 'process = "process"

let lazyEditor = Next.Dynamic.dynamic(
  async () => await import(Editor.make),
  {
    ssr: false,
    loading: () =>
      <div className="flex flex-col gap-4">
        <Skeleton className="h-96" />
        <Skeleton className="h-48" />
      </div>,
  },
)

@react.component
let make = () => {
  let editor = React.createElement(lazyEditor, {})

  <main className="flex h-screen flex-col gap-3">
    <div className="flex px-3 pt-3 items-end place-content-between">
      <img src="/react-trace.svg" alt="React-tRace" className="inline-block w-[165px]" />
      <a href="https://github.com/Zeta611/react-trace" target="_blank" className="pb-2">
        <Lucide.Github className="size-7" />
      </a>
    </div>
    <p className="px-3">
      {"React-tRace is a React Hooks interpreter and a visualizer based on a formal semantics of React Hooks. It interprets React-like components, tracking render cycles, state updates, and effect executions. The view hierarchy can be inspected at each execution step, and the entire execution is replayable!"->React.string}
    </p>
    editor
  </main>
}
