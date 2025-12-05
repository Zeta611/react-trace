import { useMemo } from "react";
import { Edge } from "@xyflow/react";
import Dagre from "@dagrejs/dagre";

import type { TreeNode } from "./types";

export type UseExpandCollapseOptions = {
  treeWidth?: number;
  treeHeight?: number;
};

function filterCollapsedChildren(dagre: Dagre.graphlib.Graph, node: TreeNode) {
  // Get the children of this node from the dagre graph
  const children = dagre.successors(node.id) as unknown as string[] | undefined;

  // Update this node's data so it knows if it has children and can be expanded
  node.data.expandable = !!children?.length;

  // If the node is collapsed (not expanded) then remove all its children
  // from the graph, including their descendants
  if (!node.data.expanded) {
    while (children?.length) {
      const child = children.pop()!;
      children.push(...(dagre.successors(child) as unknown as string[]));
      dagre.removeNode(child);
    }
  }
}

export function useExpandCollapse(
  nodes: TreeNode[],
  edges: Edge[],
  { treeWidth = 220, treeHeight = 100 }: UseExpandCollapseOptions = {}
): { nodes: TreeNode[]; edges: Edge[] } {
  return useMemo(() => {
    if (nodes.length === 0) return { nodes, edges };

    // Create a new dagre graph instance
    const dagre = new Dagre.graphlib.Graph()
      .setDefaultEdgeLabel(() => ({}))
      .setGraph({ rankdir: "TB" });

    // Add each node to the dagre graph
    for (const node of nodes) {
      dagre.setNode(node.id, {
        width: treeWidth,
        height: treeHeight,
        data: node.data,
      });
    }

    // Add each edge to the dagre graph
    for (const edge of edges) {
      dagre.setEdge(edge.source, edge.target);
    }

    // Filter out collapsed children from the graph
    for (const node of nodes) {
      filterCollapsedChildren(dagre, node);
    }

    // Run the dagre layout algorithm
    Dagre.layout(dagre);

    return {
      // Return layouted nodes, excluding any that were removed
      nodes: nodes.flatMap((node) => {
        // Skip nodes that were filtered out (collapsed)
        if (!dagre.hasNode(node.id)) return [];

        const { x, y } = dagre.node(node.id);
        const position = { x, y };
        // Create a new data object so React detects the change
        const data = { ...node.data };

        return [{ ...node, position, data }];
      }),
      edges: edges.filter(
        (edge) => dagre.hasNode(edge.source) && dagre.hasNode(edge.target)
      ),
    };
  }, [nodes, edges, treeWidth, treeHeight]);
}
