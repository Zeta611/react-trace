"use client";

import { useMemo, useEffect } from "react";
import {
  ReactFlow,
  Background,
  Controls,
  ReactFlowProvider,
  useNodesState,
  useEdgesState,
  useReactFlow,
  ConnectionLineType,
  MarkerType,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { useAppState } from "@/store/use-app-state";
import { treeToFlow } from "@/shared/tree-to-flow";
import {
  useAutoLayout,
  type LayoutOptions,
} from "@/shared/layout/use-auto-layout";

const proOptions = {
  hideAttribution: true,
};

const layoutOptions: LayoutOptions = {
  algorithm: "dagre",
  direction: "TB",
  spacing: [50, 50],
};

const defaultEdgeOptions = {
  type: "smoothstep",
  markerEnd: { type: MarkerType.ArrowClosed },
  pathOptions: { offset: 5 },
};

function VizPaneInner() {
  const { fitView } = useReactFlow();
  const recording = useAppState.use.recording();
  const currentStep = useAppState.use.currentStep();

  const treeData = useMemo(() => {
    if (currentStep === 0 || !recording.checkpoints) {
      return null;
    }
    const checkpoint = recording.checkpoints[currentStep - 1];
    return checkpoint?.tree ?? null;
  }, [recording, currentStep]);

  const { nodes: initialNodes, edges: initialEdges } = useMemo(
    () => treeToFlow(treeData),
    [treeData]
  );

  const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);

  // Reset nodes and edges when tree data changes
  useEffect(() => {
    setNodes(initialNodes);
    setEdges(initialEdges);
  }, [initialNodes, initialEdges, setNodes, setEdges]);

  // Auto-layout handles positioning
  useAutoLayout(layoutOptions);

  // Fit view when nodes change
  useEffect(() => {
    fitView();
  }, [nodes, fitView]);

  return (
    <ReactFlow
      nodes={nodes}
      edges={edges}
      onNodesChange={onNodesChange}
      onEdgesChange={onEdgesChange}
      nodesDraggable={false}
      defaultEdgeOptions={defaultEdgeOptions}
      connectionLineType={ConnectionLineType.SmoothStep}
      proOptions={proOptions}
      fitView
    >
      <Background />
      <Controls />
    </ReactFlow>
  );
}

export default function VizPane() {
  return (
    <ReactFlowProvider>
      <VizPaneInner />
    </ReactFlowProvider>
  );
}
