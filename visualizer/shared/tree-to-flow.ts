import { Node, Edge } from "reactflow";
import { Tree } from "@/store/use-app-state";

const NODE_WIDTH = 100;
const NODE_HEIGHT = 40;
const HORIZONTAL_SPACING = 50;
const VERTICAL_SPACING = 80;

type LayoutNode = {
  id: string;
  tree: Tree;
  x: number;
  y: number;
  width: number;
};

function getSubtreeWidth(tree: Tree): number {
  if (tree.children.length === 0) {
    return NODE_WIDTH;
  }

  const childrenWidth = tree.children.reduce(
    (sum, child) => sum + getSubtreeWidth(child) + HORIZONTAL_SPACING,
    -HORIZONTAL_SPACING // Remove extra spacing after last child
  );

  return Math.max(NODE_WIDTH, childrenWidth);
}

function layoutTree(
  tree: Tree,
  x: number,
  y: number,
  idPrefix: string = ""
): LayoutNode[] {
  const result: LayoutNode[] = [];
  const nodeId = idPrefix ? `${idPrefix}-${tree.path}` : tree.path || "root";

  // Calculate total width of all children
  const childWidths = tree.children.map((child) => getSubtreeWidth(child));
  const totalChildrenWidth =
    childWidths.reduce((sum, w) => sum + w + HORIZONTAL_SPACING, 0) -
    (tree.children.length > 0 ? HORIZONTAL_SPACING : 0);

  // Center this node above its children
  const nodeX = x + (getSubtreeWidth(tree) - NODE_WIDTH) / 2;

  result.push({
    id: nodeId,
    tree,
    x: nodeX,
    y,
    width: NODE_WIDTH,
  });

  // Layout children
  let childX = x + (getSubtreeWidth(tree) - totalChildrenWidth) / 2;
  tree.children.forEach((child, index) => {
    const childWidth = childWidths[index];
    const childNodes = layoutTree(
      child,
      childX,
      y + NODE_HEIGHT + VERTICAL_SPACING,
      nodeId
    );
    result.push(...childNodes);
    childX += childWidth + HORIZONTAL_SPACING;
  });

  return result;
}

export function treeToFlow(tree: Tree | null): {
  nodes: Node[];
  edges: Edge[];
} {
  if (!tree) {
    return { nodes: [], edges: [] };
  }

  const layoutNodes = layoutTree(tree, 0, 0);

  const nodes: Node[] = layoutNodes.map((ln) => ({
    id: ln.id,
    type: "default",
    data: { label: ln.tree.name || ln.tree.path || "node" },
    position: { x: ln.x, y: ln.y },
    style: {
      width: NODE_WIDTH,
      height: NODE_HEIGHT,
    },
  }));

  const edges: Edge[] = [];

  // Create edges by traversing the tree again
  function createEdges(tree: Tree, parentId: string) {
    tree.children.forEach((child) => {
      const childId = `${parentId}-${child.path}`;
      edges.push({
        id: `e-${parentId}-${childId}`,
        source: parentId,
        target: childId,
        type: "smoothstep",
      });
      createEdges(child, childId);
    });
  }

  const rootId = tree.path || "root";
  createEdges(tree, rootId);

  return { nodes, edges };
}
