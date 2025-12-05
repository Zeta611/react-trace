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
};

export type Entry = {
  msg: string;
  tree: Tree;
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
`.trim();

const fuel = 0; // 0 means unlimited fuel

function runCode(code: string): Recording {
  return run(fuel, code) as Recording;
}

// Run once for initial state
const initialRecording = runCode(sampleCode);
const initialSteps = initialRecording.checkpoints?.length ?? 0;

const replaceEmojis = (s: string) =>
  s
    .replace(":event:", "⚡")
    .replace(":retry:", "🔁")
    .replace(":check:", "🏗️")
    .replace(":finish:", "✅")
    .replace(":cancel:", "⏩")
    .replace(":effects:", "⚙️")
    .replace(":default:", "🔄");

export type AppState = ExtractState<typeof useAppState>;

export const useAppState = createSelectors(
  create(
    combine(
      {
        code: sampleCode,
        recording: initialRecording,
        currentStep: initialSteps,
      },
      (set, get) => ({
        // Derived state getters
        getSteps: () => {
          const { recording } = get();
          return recording.checkpoints?.length ?? 0;
        },
        getTreeData: () => {
          const { recording, currentStep } = get();
          if (currentStep === 0 || !recording.checkpoints) {
            return null;
          }
          const checkpoint = recording.checkpoints[currentStep - 1];
          return checkpoint?.tree ?? null;
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

        // Actions
        setCode: (code: string) => {
          const recording = runCode(code);
          const steps = recording.checkpoints?.length ?? 0;
          set({ code, recording, currentStep: steps });
        },
        setCurrentStep: (step: number) => {
          set({ currentStep: step });
        },
      })
    )
  )
);
