"use client";

import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";

import InfoPane from "@/components/info-pane";
import CodePane from "@/components/code-pane";
import VizPane from "@/components/viz-pane";
import { Slider } from "@/components/retroui/Slider";
import { useAppState } from "@/store/use-app-state";
import { ChevronLeftIcon, ChevronRightIcon } from "lucide-react";
import { useLongPress } from "@/hooks/use-long-press";

export default function WorkSpace() {
  const currentStep = useAppState.use.currentStep();
  const getSteps = useAppState.use.getSteps();
  const setCurrentStep = useAppState.use.setCurrentStep();

  const steps = getSteps();

  const prevStep = () => {
    setCurrentStep((prev) => Math.max(prev - 1, 0));
  };

  const nextStep = () => {
    setCurrentStep((prev) => Math.min(prev + 1, steps));
  };

  const leftPress = useLongPress(prevStep, { interval: 100, delay: 1000 });
  const rightPress = useLongPress(nextStep, { interval: 100, delay: 1000 });

  return (
    <div className="flex flex-1 flex-col min-h-0">
      <ResizablePanelGroup direction="horizontal" className="flex-1">
        <ResizablePanel defaultSize={30}>
          <CodePane />
        </ResizablePanel>
        <ResizableHandle withHandle />
        <ResizablePanel defaultSize={40}>
          <VizPane />
        </ResizablePanel>
        <ResizableHandle withHandle />
        <ResizablePanel className="relative" defaultSize={30}>
          <InfoPane />
        </ResizablePanel>
      </ResizablePanelGroup>
      <div className="flex border-2 mx-2 mb-2 bg-background p-4 shadow-sm">
        <button className="px-1 -ml-2" {...leftPress}>
          <ChevronLeftIcon className="size-4" />
        </button>
        <button className="px-1 mr-2" {...rightPress}>
          <ChevronRightIcon className="size-4" />
        </button>
        <Slider
          className="w-full"
          value={[currentStep]}
          onValueChange={(vs) => setCurrentStep(vs[0])}
          min={0}
          max={steps}
          step={1}
          aria-label="Execution Step Slider"
        />
      </div>
    </div>
  );
}
