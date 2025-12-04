"use client";

import { useState } from "react";
import CodeMirror from "@uiw/react-codemirror";
import { javascript } from "@codemirror/lang-javascript";

export default function CodePane() {
  const [code, setCode] = useState(`// Your code here
`);

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
