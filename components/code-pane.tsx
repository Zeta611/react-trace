"use client";

import { useRef, useEffect } from "react";
import CodeMirror, { ReactCodeMirrorRef } from "@uiw/react-codemirror";
import { javascript } from "@codemirror/lang-javascript";
import { EditorView, Decoration, DecorationSet } from "@codemirror/view";
import { StateField, StateEffect } from "@codemirror/state";
import { useAppState, SourceLoc } from "@/store/use-app-state";

const setHighlight = StateEffect.define<SourceLoc | null>();
const highlightField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(decorations, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setHighlight)) {
        const loc = effect.value;
        if (!loc) {
          return Decoration.none;
        }
        try {
          // Convert line/col to absolute positions
          // Lines are 1-indexed in Location.t, CodeMirror uses 1-indexed lines too
          const doc = tr.state.doc;
          const startLine = Math.min(loc.start_line, doc.lines);
          const endLine = Math.min(loc.end_line, doc.lines);

          const startLineInfo = doc.line(startLine);
          const endLineInfo = doc.line(endLine);

          const from = startLineInfo.from + loc.start_col;
          const to = endLineInfo.from + loc.end_col;

          // Clamp to valid range
          const clampedFrom = Math.max(0, Math.min(from, doc.length));
          const clampedTo = Math.max(clampedFrom, Math.min(to, doc.length));

          if (clampedFrom === clampedTo) {
            return Decoration.none;
          }

          const mark = Decoration.mark({ class: "cm-highlight-current" });
          return Decoration.set([mark.range(clampedFrom, clampedTo)]);
        } catch {
          return Decoration.none;
        }
      }
    }
    return decorations;
  },
  provide: (f) => EditorView.decorations.from(f),
});

const highlightTheme = EditorView.baseTheme({
  ".cm-highlight-current": {
    backgroundColor: "rgba(254, 240, 138, 0.6)",
    borderRadius: "2px",
  },
});

const extensions = [javascript({ jsx: true }), highlightField, highlightTheme];

export default function CodePane() {
  const code = useAppState.use.code();
  const setCode = useAppState.use.setCode();
  const getCurrentLoc = useAppState.use.getCurrentLoc();
  const currentStep = useAppState.use.currentStep();

  const currentLoc = getCurrentLoc();

  const editorRef = useRef<ReactCodeMirrorRef | null>(null);

  // Effect to update highlight when location changes
  // currentStep is included to ensure we re-run when step changes
  useEffect(() => {
    if (editorRef.current?.view) {
      editorRef.current.view.dispatch({
        effects: setHighlight.of(currentLoc),
      });
    }
  }, [currentLoc, currentStep]);

  return (
    <div className="h-full w-full">
      <CodeMirror
        ref={editorRef}
        value={code}
        height="100%"
        extensions={extensions}
        onChange={(value) => setCode(value)}
        onCreateEditor={(view) => {
          view.dispatch({
            effects: setHighlight.of(currentLoc),
          });
        }}
        className="h-full text-base font-mono"
      />
    </div>
  );
}
