import { Node } from "@xyflow/react";

export type TreeNodeData = {
  label: string;
  expanded: boolean;
  expandable?: boolean;
};

export type TreeNode = Node<TreeNodeData, "treeNode">;
