import { create, ExtractState, StoreApi, UseBoundStore } from "zustand";
import { combine } from "zustand/middleware";
import { run } from "@/shared/react-trace/ReacttRace.res.js";

// Types matching the ReScript bindings
export type StateEntry = {
  label: string;
  value: string;
  queue_size: number;
};

export type DecisionInfo = {
  chk: boolean;
  eff: boolean;
};

export type Tree = {
  path: string;
  name: string;
  children: Tree[];
  st_store?: StateEntry[];
  eff_q_size?: number;
  dec?: DecisionInfo;
  arg?: string;
  handler?: number;
};

export type SourceLoc = {
  start_line: number;
  start_col: number;
  end_line: number;
  end_col: number;
};

export type Entry = {
  msg: string;
  stree: Tree;
  mounting_forest?: Tree[];
  source_loc?: SourceLoc;
};

export type Recording = {
  checkpoints?: Entry[];
  log?: string;
  error?: string;
};

type WithSelectors<S> = S extends { getState: () => infer T }
  ? S & { use: { [K in keyof T]: () => T[K] } }
  : never;

const createSelectors = <S extends UseBoundStore<StoreApi<object>>>(
  _store: S
) => {
  const store = _store as WithSelectors<typeof _store>;
  store.use = {};
  for (const k of Object.keys(store.getState())) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (store.use as any)[k] = () => store((s) => s[k as keyof typeof s]);
  }

  return store;
};

const sampleCode = `
let Demo x =
  let (s, setS) = useState x in
  let f = fun s -> s + 1 in
  if s = 0 then setS f;
  useEffect (if s = 1 then setS f);
  if s <= 1 then s else
    [s, button (fun _ -> setS f)];;
Demo 0
`.trim();

function runCode(fuel: number, events: number[], code: string): Recording {
  return run(fuel, events, code) as Recording;
}

const fuel = 0; // 0 means unlimited fuel
const initialEvents: number[] = [];
// Run once for initial state
const initialRecording = runCode(fuel, initialEvents, sampleCode);
const initialSteps = initialRecording.checkpoints?.length ?? 0;

const replaceEmojis = (s: string) =>
  s
    .replace(":event:", "⚡")
    .replace(":eloop:", "🎬")
    .replace(":retry:", "🔁")
    .replace(":check:", "🏗️")
    .replace(":finish:", "✅")
    .replace(":cancel:", "⏩")
    .replace(":effects:", "⚙️")
    .replace(":hook:", "🪝")
    .replace(":default:", "💡");

export type AppState = ExtractState<typeof useAppState>;

export const useAppState = createSelectors(
  create(
    combine(
      {
        code: sampleCode,
        events: initialEvents,
        recording: initialRecording,
        currentStep: initialSteps,
        steps: initialRecording.checkpoints?.length ?? 0,
      },
      (set, get) => ({
        // Derived state getters
        getTreeData: () => {
          const { recording, currentStep } = get();
          if (currentStep === 0 || !recording.checkpoints) {
            return null;
          }
          const checkpoint = recording.checkpoints[currentStep - 1];
          return checkpoint?.stree ?? null;
        },
        getMountingForest: () => {
          const { recording, currentStep } = get();
          if (currentStep === 0 || !recording.checkpoints) {
            return null;
          }
          const checkpoint = recording.checkpoints[currentStep - 1];
          return checkpoint?.mounting_forest ?? null;
        },
        getReport: () => {
          const { recording, currentStep } = get();
          if (recording.error) {
            return recording.error;
          }
          if (!recording.checkpoints) {
            return "";
          }
          return recording.checkpoints
            .slice(0, currentStep)
            .map((x) => replaceEmojis(x.msg))
            .join("\n");
        },
        getCurrentLoc: () => {
          const { recording, currentStep } = get();
          if (currentStep === 0 || !recording.checkpoints) {
            return null;
          }
          const checkpoint = recording.checkpoints[currentStep - 1];
          return checkpoint?.source_loc ?? null;
        },

        // Actions
        setCode: (code: string) => {
          const recording = runCode(fuel, [], code);
          const steps = recording.checkpoints?.length ?? 0;
          set({ code, recording, currentStep: steps, steps, events: [] });
        },
        setCurrentStep: (step: number | ((prev: number) => number)) => {
          set((prev) => ({
            ...prev,
            currentStep:
              typeof step === "function" ? step(prev.currentStep) : step,
          }));
        },
        triggerEvent: (handlerIdx: number) => {
          const { code, events } = get();
          const nextEvents = [...events, handlerIdx];
          const recording = runCode(fuel, nextEvents, code);
          const steps = recording.checkpoints?.length ?? 0;
          set({ recording, steps, events: nextEvents });
        },
      })
    )
  )
);
