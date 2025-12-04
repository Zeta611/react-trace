"use client";

import CodeMirror from "@uiw/react-codemirror";
import { javascript } from "@codemirror/lang-javascript";
import { useAppState } from "@/store/use-app-state";

export default function CodePane() {
  const code = useAppState.use.code();
  const setCode = useAppState.use.setCode();

  return (
    <div className="h-full w-full">
      <CodeMirror
        value={code}
        height="100%"
        extensions={[javascript({ jsx: true })]}
        onChange={(value) => setCode(value)}
        className="h-full text-base font-mono"
      />
    </div>
  );
}
