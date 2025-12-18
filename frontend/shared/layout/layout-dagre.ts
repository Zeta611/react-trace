import { Edge } from "@xyflow/react";
import Dagre from "@dagrejs/dagre";

import type { TreeNode } from "./types";

export type LayoutOptions = {
  treeWidth?: number;
  treeHeight?: number;
};

/**
 * Layout nodes using dagre algorithm.
 * Returns positioned nodes and filtered edges.
 */
export function layoutNodes(
  nodes: TreeNode[],
  edges: Edge[],
  { treeWidth = 220, treeHeight = 100 }: LayoutOptions = {}
): { nodes: TreeNode[]; edges: Edge[] } {
  if (nodes.length === 0) return { nodes, edges };

  // Create a new dagre graph instance
  const dagre = new Dagre.graphlib.Graph()
    .setDefaultEdgeLabel(() => ({}))
    .setGraph({
      rankdir: "TB",
      nodesep: 80,
      ranksep: 100,
    });

  // Add each node to the dagre graph
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

  // Run the dagre layout algorithm
  Dagre.layout(dagre);

  // Collect layouted tree nodes
  const resultNodes: TreeNode[] = [];
  const resultEdges: Edge[] = [];

  for (const node of nodes) {
    if (!dagre.hasNode(node.id)) continue;

    const dagreNode = dagre.node(node.id);
    // Dagre returns center positions, ReactFlow uses top-left
    const position = {
      x: dagreNode.x - dagreNode.width / 2,
      y: dagreNode.y - dagreNode.height / 2,
    };
    const data = { ...node.data };

    resultNodes.push({ ...node, position, data });
  }

  // Add edges (filtered to only include visible nodes)
  for (const edge of edges) {
    if (dagre.hasNode(edge.source) && dagre.hasNode(edge.target)) {
      resultEdges.push(edge);
    }
  }

  return { nodes: resultNodes, edges: resultEdges };
}

/**
 * Get the bounding box of a set of nodes.
 */
export function getNodesBounds(
  nodes: TreeNode[]
): { minX: number; minY: number; maxX: number; maxY: number; width: number; height: number } {
  if (nodes.length === 0) {
    return { minX: 0, minY: 0, maxX: 0, maxY: 0, width: 0, height: 0 };
  }

  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;

  for (const node of nodes) {
    const isComponentNode = node.data.dec != null;
    const width = isComponentNode ? 280 : 100;
    const height = isComponentNode ? 200 : 50;

    minX = Math.min(minX, node.position.x);
    minY = Math.min(minY, node.position.y);
    maxX = Math.max(maxX, node.position.x + width);
    maxY = Math.max(maxY, node.position.y + height);
  }

  return {
    minX,
    minY,
    maxX,
    maxY,
    width: maxX - minX,
    height: maxY - minY,
  };
}

/**
 * Offset all nodes by a given amount.
 */
export function offsetNodes(
  nodes: TreeNode[],
  offsetX: number,
  offsetY: number
): TreeNode[] {
  return nodes.map((node) => ({
    ...node,
    position: {
      x: node.position.x + offsetX,
      y: node.position.y + offsetY,
    },
  }));
}
