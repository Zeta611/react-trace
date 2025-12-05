import { Node } from "@xyflow/react";
import type { StateEntry, DecisionInfo } from "@/store/use-app-state";

export type TreeNodeData = {
  label: string;
  expanded: boolean;
  expandable?: boolean;
  // Internal data (only for component nodes)
  stStore?: StateEntry[];
  effQSize?: number;
  dec?: DecisionInfo;
  arg?: string;
};

export type TreeNode = Node<TreeNodeData, "treeNode">;
