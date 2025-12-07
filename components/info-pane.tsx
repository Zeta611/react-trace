"use client";

import { Slider } from "@/components/retroui/Slider";
import { useAppState } from "@/store/use-app-state";

export default function InfoPane() {
  const currentStep = useAppState.use.currentStep();
  const getSteps = useAppState.use.getSteps();
  const getReport = useAppState.use.getReport();
  const setCurrentStep = useAppState.use.setCurrentStep();

  const steps = getSteps();
  const report = getReport();

  return (
    <div className="flex h-full flex-col gap-4 p-3">
      <Slider
        className="w-full"
        value={[currentStep]}
        onValueChange={(vs) => setCurrentStep(vs[0])}
        min={0}
        max={steps}
        step={1}
        aria-label="Execution Step Slider"
      />
      <div className="flex-1 overflow-auto text-base font-sans text-gray-800 whitespace-pre-wrap">
        {report}
      </div>
    </div>
  );
}
