import { create, ExtractState, StoreApi, UseBoundStore } from "zustand";
import { combine } from "zustand/middleware";

type WithSelectors<S> = S extends { getState: () => infer T }
  ? S & { use: { [K in keyof T]: () => T[K] } }
  : never;

const createSelectors = <S extends UseBoundStore<StoreApi<object>>>(
  _store: S,
) => {
  const store = _store as WithSelectors<typeof _store>;
  store.use = {};
  for (const k of Object.keys(store.getState())) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (store.use as any)[k] = () => store((s) => s[k as keyof typeof s]);
  }

  return store;
};

function eq<T>(x: T, y: T): boolean {
  return JSON.stringify(x) === JSON.stringify(y);
}

export type AppState = ExtractState<typeof useAppState>;

export const useAppState = createSelectors(
  create(
    combine(
      {
        state: 0,
      },
      (set, get) => ({
        increment: () => set((s) => ({ state: s.state + 1 })),
      })
    )
  )
);
