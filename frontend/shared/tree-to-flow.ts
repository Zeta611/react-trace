import { Edge } from "@xyflow/react";
import { Tree } from "@/store/use-app-state";
import type { TreeNode } from "./layout/types";

export function treeToFlow(tree: Tree | null): {
  nodes: TreeNode[];
  edges: Edge[];
} {
  if (!tree) {
    return { nodes: [], edges: [] };
  }

  const nodes: TreeNode[] = [];
  const edges: Edge[] = [];

  function traverse(tree: Tree, parentId: string | null) {
    const nodeId = parentId ? `${parentId}-${tree.path}` : tree.path || "root";
    const hasChildren = tree.children && tree.children.length > 0;

    nodes.push({
      id: nodeId,
      type: "treeNode",
      data: {
        label: tree.name || tree.path || "node",
        expanded: true,
        expandable: hasChildren,
      },
      position: { x: 0, y: 0 },
    });

    if (parentId) {
      edges.push({
        id: `e-${parentId}-${nodeId}`,
        source: parentId,
        target: nodeId,
        type: "smoothstep",
      });
    }

    tree.children.forEach((child) => {
      traverse(child, nodeId);
    });
  }

  traverse(tree, null);

  return { nodes, edges };
}
