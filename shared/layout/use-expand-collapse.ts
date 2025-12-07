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

    // Create a new dagre graph instance with more spacing
    const dagre = new Dagre.graphlib.Graph()
      .setDefaultEdgeLabel(() => ({}))
      .setGraph({
        rankdir: "TB",
        nodesep: 80, // Horizontal spacing between nodes
        ranksep: 100, // Vertical spacing between ranks
      });

    // Add each node to the dagre graph
    // Use larger dimensions for component nodes (they have tables)
    for (const node of nodes) {
      const isComponentNode = node.data.dec != null;
      dagre.setNode(node.id, {
        width: isComponentNode ? 280 : treeWidth,
        height: isComponentNode ? 200 : treeHeight,
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

    // Collect layouted tree nodes
    const resultNodes: TreeNode[] = [];
    const resultEdges: Edge[] = [];

    for (const node of nodes) {
      // Skip nodes that were filtered out (collapsed)
      if (!dagre.hasNode(node.id)) continue;

      const dagreNode = dagre.node(node.id);
      // Dagre returns center positions, ReactFlow uses top-left
      // Offset by half width/height to center the node
      const position = {
        x: dagreNode.x - dagreNode.width / 2,
        y: dagreNode.y - dagreNode.height / 2,
      };
      const data = { ...node.data };

      resultNodes.push({ ...node, position, data });
    }

    // Add tree edges (filtered to only include visible nodes)
    for (const edge of edges) {
      if (dagre.hasNode(edge.source) && dagre.hasNode(edge.target)) {
        resultEdges.push(edge);
      }
    }

    return {
      nodes: resultNodes,
      edges: resultEdges,
    };
  }, [nodes, edges, treeWidth, treeHeight]);
}
