"use client";

import { useMemo, useEffect, useCallback, useRef } from "react";
import {
  ReactFlow,
  Background,
  Controls,
  ReactFlowProvider,
  useReactFlow,
  ConnectionLineType,
  MarkerType,
  useNodesState,
  useEdgesState,
  BackgroundVariant,
  type NodeMouseHandler,
  type Edge,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { useAppState } from "@/store/use-app-state";
import { treeToFlow } from "@/shared/tree-to-flow";
import { useExpandCollapse } from "@/shared/layout/use-expand-collapse";
import type { TreeNode } from "@/shared/layout/types";
import TreeNodeComponent from "./tree-node";

const proOptions = {
  hideAttribution: true,
};

const defaultEdgeOptions = {
  type: "smoothstep",
  style: {
    stroke: "var(--border)",
    strokeWidth: 1.5,
  },
  markerEnd: {
    type: MarkerType.ArrowClosed,
    width: 12,
    height: 12,
    color: "var(--border)",
  },
  pathOptions: { offset: 8, borderRadius: 0 },
};

const nodeTypes = {
  treeNode: TreeNodeComponent,
};

function VizPaneInner() {
  const { fitView } = useReactFlow();
  const recording = useAppState.use.recording();
  const currentStep = useAppState.use.currentStep();

  // Track expanded states across tree updates
  const expandedStatesRef = useRef<Map<string, boolean>>(new Map());

  const treeData = useMemo(() => {
    if (currentStep === 0 || !recording.checkpoints) {
      return null;
    }
    const checkpoint = recording.checkpoints[currentStep - 1];
    return checkpoint?.stree ?? null;
  }, [recording, currentStep]);

  const { nodes: rawNodes, edges: newEdges } = useMemo(
    () => treeToFlow(treeData),
    [treeData]
  );

  const [nodes, setNodes] = useNodesState<TreeNode>([]);
  const [edges, setEdges] = useEdgesState<Edge>([]);

  // Sync nodes/edges when tree data changes, preserving expanded states
  useEffect(() => {
    const updatedNodes = rawNodes.map((node) => ({
      ...node,
      data: {
        ...node.data,
        expanded: expandedStatesRef.current.get(node.id) ?? true,
      },
    }));
    setNodes(updatedNodes);
    setEdges(newEdges);
  }, [rawNodes, newEdges, setNodes, setEdges]);

  // Use expand/collapse hook for filtering and layout
  const { nodes: visibleNodes, edges: visibleEdges } = useExpandCollapse(
    nodes,
    edges,
    { treeWidth: 100, treeHeight: 50 }
  );

  // Toggle expanded state on node click
  const onNodeClick: NodeMouseHandler<TreeNode> = useCallback(
    (_, node) => {
      if (!node.data.expandable) return;

      setNodes((nds) =>
        nds.map((n) => {
          if (n.id === node.id) {
            const newExpanded = !n.data.expanded;
            expandedStatesRef.current.set(n.id, newExpanded);
            return {
              ...n,
              data: { ...n.data, expanded: newExpanded },
            };
          }
          return n;
        })
      );
    },
    [setNodes]
  );

  // Fit view when visible nodes count changes
  const nodeCount = visibleNodes.length;
  useEffect(() => {
    fitView({ duration: 150 });
  }, [nodeCount, fitView]);

  return (
    <ReactFlow
      nodes={visibleNodes}
      edges={visibleEdges}
      onNodeClick={onNodeClick}
      nodeTypes={nodeTypes}
      nodesDraggable={false}
      nodesConnectable={false}
      elementsSelectable={false}
      defaultEdgeOptions={defaultEdgeOptions}
      connectionLineType={ConnectionLineType.SmoothStep}
      proOptions={proOptions}
      fitView
      className="[&_.react-flow__controls]:border-2 [&_.react-flow__controls]:border-border [&_.react-flow__controls]:shadow-sm [&_.react-flow__controls]:rounded-none [&_.react-flow__controls-button]:border-border [&_.react-flow__controls-button]:border-t-0 [&_.react-flow__controls-button:first-child]:border-t-2 [&_.react-flow__controls-button]:bg-card [&_.react-flow__controls-button]:fill-foreground [&_.react-flow__controls-button:hover]:bg-muted"
    >
      <Background
        variant={BackgroundVariant.Dots}
        gap={24}
        size={1.5}
        color="var(--muted)"
      />
      <Controls showInteractive={false} />
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
