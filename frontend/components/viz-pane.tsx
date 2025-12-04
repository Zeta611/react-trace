"use client";

import { useMemo } from "react";
import ReactFlow, { Background, Controls } from "reactflow";
import "reactflow/dist/style.css";
import { useAppState } from "@/store/use-app-state";
import { treeToFlow } from "@/shared/tree-to-flow";

export default function VizPane() {
  const recording = useAppState.use.recording();
  const currentStep = useAppState.use.currentStep();

  const treeData = useMemo(() => {
    if (currentStep === 0 || !recording.checkpoints) {
      return null;
    }
    const checkpoint = recording.checkpoints[currentStep - 1];
    return checkpoint?.tree ?? null;
  }, [recording, currentStep]);

  const { nodes, edges } = useMemo(() => treeToFlow(treeData), [treeData]);

  return (
    <ReactFlow
      key={currentStep}
      nodes={nodes}
      edges={edges}
      proOptions={{ hideAttribution: true }}
      fitView
    >
      <Background />
      <Controls />
    </ReactFlow>
  );
}
