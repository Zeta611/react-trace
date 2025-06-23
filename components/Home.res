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

  <main className="flex h-screen flex-col">
    <h1 className="text-4xl p-3 font-serif"> {"React-tRace"->React.string} </h1>
    editor
  </main>
}
