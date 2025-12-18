import { Edge } from "@xyflow/react";
import { Tree } from "@/store/use-app-state";
import type { TreeNode } from "./layout/types";

export type TreeToFlowOptions = {
  /** Prefix added to all node/edge IDs to avoid collisions */
  idPrefix?: string;
};

export function treeToFlow(
  tree: Tree | null,
  options: TreeToFlowOptions = {}
): {
  nodes: TreeNode[];
  edges: Edge[];
} {
  if (!tree) {
    return { nodes: [], edges: [] };
  }

  const { idPrefix = "" } = options;
  const nodes: TreeNode[] = [];
  const edges: Edge[] = [];

  // parentBaseId is the base ID without the prefix (to avoid double-prefixing)
  function traverse(tree: Tree, parentBaseId: string | null, index?: number) {
    const path = tree.path ?? "";
    const sibling = index !== null ? `-${index}` : "";
    const baseId = parentBaseId
      ? `${parentBaseId}-${path}${sibling}`
      : path || "root";
    const nodeId = `${idPrefix}${baseId}`;
    const parentNodeId = parentBaseId ? `${idPrefix}${parentBaseId}` : null;
    const hasChildren = tree.children && tree.children.length > 0;

    nodes.push({
      id: nodeId,
      type: "treeNode",
      data: {
        label: tree.name || tree.path || "node",
        expanded: true,
        expandable: hasChildren,
        // Pass through internal data for component nodes
        stStore: tree.st_store,
        effQSize: tree.eff_q_size,
        dec: tree.dec,
        arg: tree.arg,
        handler: tree.handler,
      },
      position: { x: 0, y: 0 },
    });

    if (parentNodeId) {
      edges.push({
        id: `e-${parentNodeId}-${nodeId}`,
        source: parentNodeId,
        target: nodeId,
        type: "smoothstep",
      });
    }

    tree.children.forEach((child, idx) => {
      traverse(child, baseId, idx);
    });
  }

  traverse(tree, null);

  return { nodes, edges };
}

/**
 * Convert a forest (array of trees) into React Flow nodes and edges.
 * Each tree in the forest becomes a separate subtree with no edges between them.
 */
export function forestToFlow(
  forest: Tree[] | null,
  options: TreeToFlowOptions = {}
): {
  nodes: TreeNode[];
  edges: Edge[];
} {
  if (!forest || forest.length === 0) {
    return { nodes: [], edges: [] };
  }

  const allNodes: TreeNode[] = [];
  const allEdges: Edge[] = [];

  forest.forEach((tree, treeIdx) => {
    // Each tree in the forest gets a unique prefix to avoid ID collisions
    const treePrefix = `${options.idPrefix ?? ""}tree${treeIdx}:`;
    const { nodes, edges } = treeToFlow(tree, { idPrefix: treePrefix });
    allNodes.push(...nodes);
    allEdges.push(...edges);
  });

  return { nodes: allNodes, edges: allEdges };
}
