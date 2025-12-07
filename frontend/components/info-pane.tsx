"use client";

import { useAppState } from "@/store/use-app-state";

export default function InfoPane() {
  const getReport = useAppState.use.getReport();

  const report = getReport();

  return (
    <div className="flex h-full flex-col gap-4 p-3">
      <div className="flex-1 overflow-auto text-base font-sans text-gray-800 whitespace-pre-wrap">
        {report}
      </div>
    </div>
  );
}
