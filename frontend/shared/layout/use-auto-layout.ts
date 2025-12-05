import { useEffect } from "react";
import {
  type Node,
  type Edge,
  useReactFlow,
  useNodesInitialized,
  useStore,
} from "@xyflow/react";

import {
  layoutAlgorithms,
  getSourceHandlePosition,
  getTargetHandlePosition,
  type LayoutAlgorithmOptions,
} from ".";

export type LayoutOptions = {
  algorithm: keyof typeof layoutAlgorithms;
} & LayoutAlgorithmOptions;

export function useAutoLayout(options: LayoutOptions) {
  const { setNodes, setEdges } = useReactFlow();
  const nodesInitialized = useNodesInitialized();
  // Store a map of nodes and edges in the flow. By using a custom equality
  // function, we ensure the layout algorithm only runs when something has
  // changed that should actually trigger a layout change.
  const elements = useStore(
    (state) => ({
      nodes: state.nodes,
      edges: state.edges,
    }),
    compareElements
  );

  useEffect(() => {
    // Only run the layout if there are nodes and they have been initialized
    // with their dimensions
    if (!nodesInitialized || elements.nodes.length === 0) {
      return;
    }

    const runLayout = async () => {
      const layoutAlgorithm = layoutAlgorithms[options.algorithm];
      // Clone nodes and edges to avoid mutating the original elements
      const nodes = elements.nodes.map((node) => ({ ...node }));
      const edges = elements.edges.map((edge) => ({ ...edge }));

      const { nodes: nextNodes, edges: nextEdges } = await layoutAlgorithm(
        nodes,
        edges,
        options
      );

      for (const node of nextNodes) {
        node.style = { ...node.style, opacity: 1 };
        node.sourcePosition = getSourceHandlePosition(options.direction);
        node.targetPosition = getTargetHandlePosition(options.direction);
      }

      for (const edge of nextEdges) {
        edge.style = { ...edge.style, opacity: 1 };
      }

      setNodes(nextNodes);
      setEdges(nextEdges);
    };

    runLayout();
  }, [nodesInitialized, elements, options, setNodes, setEdges]);
}

type Elements = {
  nodes: Array<Node>;
  edges: Array<Edge>;
};

function compareElements(xs: Elements, ys: Elements) {
  return compareNodes(xs.nodes, ys.nodes) && compareEdges(xs.edges, ys.edges);
}

function compareNodes(xs: Array<Node>, ys: Array<Node>) {
  if (xs.length !== ys.length) return false;

  for (let i = 0; i < xs.length; i++) {
    const x = xs[i];
    const y = ys[i];

    if (!y) return false;
    // Don't force a layout change while a user might be resizing/dragging
    if (x.resizing || x.dragging) return true;
    if (
      x.measured?.width !== y.measured?.width ||
      x.measured?.height !== y.measured?.height
    ) {
      return false;
    }
  }

  return true;
}

function compareEdges(xs: Array<Edge>, ys: Array<Edge>) {
  if (xs.length !== ys.length) return false;

  for (let i = 0; i < xs.length; i++) {
    const x = xs[i];
    const y = ys[i];

    if (x.source !== y.source || x.target !== y.target) return false;
    if (x?.sourceHandle !== y?.sourceHandle) return false;
    if (x?.targetHandle !== y?.targetHandle) return false;
  }

  return true;
}
