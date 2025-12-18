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
import { treeToFlow, forestToFlow } from "@/shared/tree-to-flow";
import { useExpandCollapse } from "@/shared/layout/use-expand-collapse";
import { getNodesBounds, offsetNodes } from "@/shared/layout/layout-dagre";
import type { TreeNode, MountingGroupNode } from "@/shared/layout/types";
import TreeNodeComponent from "./tree-node";
import MountingGroupNodeComponent from "./mounting-group-node";

// Union type for all node types used in this flow
type AppNode = TreeNode | MountingGroupNode;

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
  mountingGroup: MountingGroupNodeComponent,
};

const MOUNTING_GROUP_ID = "mounting-group";
const MOUNTING_GROUP_PADDING = 24;
const GAP_BETWEEN_TREES = 120;

function VizPaneInner() {
  const { fitView } = useReactFlow();
  const recording = useAppState.use.recording();
  const currentStep = useAppState.use.currentStep();

  // Track expanded states across tree updates
  const expandedStatesRef = useRef<Map<string, boolean>>(new Map());

  // Get main tree data
  const treeData = useMemo(() => {
    if (currentStep === 0 || !recording.checkpoints) {
      return null;
    }
    const checkpoint = recording.checkpoints[currentStep - 1];
    return checkpoint?.stree ?? null;
  }, [recording, currentStep]);

  // Get mounting forest data
  const mountingForest = useMemo(() => {
    if (currentStep === 0 || !recording.checkpoints) {
      return null;
    }
    const checkpoint = recording.checkpoints[currentStep - 1];
    return checkpoint?.mounting_forest ?? null;
  }, [recording, currentStep]);

  // Convert main tree to flow nodes/edges
  const { nodes: rawNodes, edges: newEdges } = useMemo(
    () => treeToFlow(treeData),
    [treeData]
  );

  // Convert mounting forest to flow nodes/edges
  const { nodes: mountingRawNodes, edges: mountingRawEdges } = useMemo(
    () => forestToFlow(mountingForest, { idPrefix: "mounting:" }),
    [mountingForest]
  );

  const [nodes, setNodes] = useNodesState<TreeNode>([]);
  const [edges, setEdges] = useEdgesState<Edge>([]);

  // State for mounting nodes (separate from main tree)
  const [mountingNodesState, setMountingNodesState] = useNodesState<TreeNode>(
    []
  );
  const [mountingEdgesState, setMountingEdgesState] = useEdgesState<Edge>([]);

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

  // Sync mounting nodes/edges when mounting forest changes, preserving expanded states
  useEffect(() => {
    const updatedMountingNodes = mountingRawNodes.map((node) => ({
      ...node,
      data: {
        ...node.data,
        expanded: expandedStatesRef.current.get(node.id) ?? true,
      },
    }));
    setMountingNodesState(updatedMountingNodes);
    setMountingEdgesState(mountingRawEdges);
  }, [
    mountingRawNodes,
    mountingRawEdges,
    setMountingNodesState,
    setMountingEdgesState,
  ]);

  // Use expand/collapse hook for filtering and layout of main tree
  const { nodes: visibleNodes, edges: visibleEdges } = useExpandCollapse(
    nodes,
    edges,
    { treeWidth: 100, treeHeight: 50 }
  );

  // Apply expand/collapse to mounting nodes
  const { nodes: visibleMountingNodes, edges: visibleMountingEdges } =
    useExpandCollapse(mountingNodesState, mountingEdgesState, {
      treeWidth: 100,
      treeHeight: 50,
    });

  // Layout and position the mounting forest
  const { mountingNodes, mountingEdges, mountingGroupNode } = useMemo(() => {
    if (visibleMountingNodes.length === 0) {
      return { mountingNodes: [], mountingEdges: [], mountingGroupNode: null };
    }

    // Use the already-layouted nodes from useExpandCollapse
    const layoutedMountingNodes = visibleMountingNodes;
    const layoutedMountingEdges = visibleMountingEdges;

    // Get bounds of main tree (if any)
    const mainBounds = getNodesBounds(visibleNodes);

    // Get bounds of mounting forest (before offset)
    const mountingBounds = getNodesBounds(layoutedMountingNodes);

    // Position mounting forest to the right of main tree
    const offsetX =
      visibleNodes.length > 0
        ? mainBounds.maxX + GAP_BETWEEN_TREES - mountingBounds.minX
        : -mountingBounds.minX;
    const offsetY =
      visibleNodes.length > 0
        ? mainBounds.minY - mountingBounds.minY
        : -mountingBounds.minY;

    const positionedMountingNodes = offsetNodes(
      layoutedMountingNodes,
      offsetX,
      offsetY
    );

    // Recalculate bounds after offset
    const finalMountingBounds = getNodesBounds(positionedMountingNodes);

    // Create group node that wraps all mounting nodes
    const groupNode: MountingGroupNode = {
      id: MOUNTING_GROUP_ID,
      type: "mountingGroup",
      position: {
        x: finalMountingBounds.minX - MOUNTING_GROUP_PADDING,
        y: finalMountingBounds.minY - MOUNTING_GROUP_PADDING,
      },
      data: { label: "Mounting..." },
      style: {
        width: finalMountingBounds.width + MOUNTING_GROUP_PADDING * 2,
        height: finalMountingBounds.height + MOUNTING_GROUP_PADDING * 2,
      },
      // Group nodes should be behind other nodes
      zIndex: -1,
    };

    // Set parentId and adjust positions to be relative to the group
    const nodesWithParent = positionedMountingNodes.map((node) => ({
      ...node,
      parentId: MOUNTING_GROUP_ID,
      extent: "parent" as const,
      position: {
        x: node.position.x - groupNode.position.x,
        y: node.position.y - groupNode.position.y,
      },
    }));

    return {
      mountingNodes: nodesWithParent,
      mountingEdges: layoutedMountingEdges,
      mountingGroupNode: groupNode,
    };
  }, [visibleMountingNodes, visibleMountingEdges, visibleNodes]);

  // Combine all nodes and edges
  const allNodes = useMemo((): AppNode[] => {
    const combined: AppNode[] = [...visibleNodes];
    if (mountingGroupNode) {
      combined.push(mountingGroupNode);
    }
    combined.push(...mountingNodes);
    return combined;
  }, [visibleNodes, mountingNodes, mountingGroupNode]);

  const allEdges = useMemo(() => {
    return [...visibleEdges, ...mountingEdges];
  }, [visibleEdges, mountingEdges]);

  // Toggle expanded state on node click (only for tree nodes)
  const onNodeClick: NodeMouseHandler<AppNode> = useCallback(
    (_, node) => {
      // Only handle tree nodes with expandable data
      if (node.type !== "treeNode") return;
      const treeNode = node as TreeNode;
      if (!treeNode.data.expandable) return;

      // Update expanded state in shared ref
      const newExpanded = !treeNode.data.expanded;
      expandedStatesRef.current.set(treeNode.id, newExpanded);

      // Check if this is a mounting node (has "mounting:" prefix)
      const isMountingNode = treeNode.id.startsWith("mounting:");

      const updateNode = (n: TreeNode): TreeNode => {
        if (n.id === treeNode.id) {
          return {
            ...n,
            data: { ...n.data, expanded: newExpanded },
          };
        }
        return n;
      };

      if (isMountingNode) {
        setMountingNodesState((nds) => nds.map(updateNode));
      } else {
        setNodes((nds) => nds.map(updateNode));
      }
    },
    [setNodes, setMountingNodesState]
  );

  // Fit view when visible nodes count changes
  const nodeCount = allNodes.length;
  useEffect(() => {
    fitView({ duration: 150 });
  }, [nodeCount, fitView]);

  return (
    <ReactFlow<AppNode, Edge>
      nodes={allNodes}
      edges={allEdges}
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
