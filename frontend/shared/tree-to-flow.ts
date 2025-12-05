import { Node, Edge } from "@xyflow/react";
import { Tree } from "@/store/use-app-state";

export function treeToFlow(tree: Tree | null): {
  nodes: Node[];
  edges: Edge[];
} {
  if (!tree) {
    return { nodes: [], edges: [] };
  }

  const nodes: Node[] = [];
  const edges: Edge[] = [];

  function traverse(tree: Tree, parentId: string | null) {
    const nodeId = parentId ? `${parentId}-${tree.path}` : tree.path || "root";

    nodes.push({
      id: nodeId,
      type: "default",
      data: { label: tree.name || tree.path || "node" },
      position: { x: 0, y: 0 }, // Position will be computed by auto-layout
      style: { opacity: 0 }, // Start invisible, auto-layout will make visible
    });

    if (parentId) {
      edges.push({
        id: `e-${parentId}-${nodeId}`,
        source: parentId,
        target: nodeId,
        type: "smoothstep",
        style: { opacity: 0 },
      });
    }

    tree.children.forEach((child) => {
      traverse(child, nodeId);
    });
  }

  traverse(tree, null);

  return { nodes, edges };
}
