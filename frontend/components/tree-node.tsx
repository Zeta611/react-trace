import { Handle, Position, NodeProps } from "@xyflow/react";
import type { TreeNode } from "@/shared/layout/types";

export default function TreeNodeComponent({ data }: NodeProps<TreeNode>) {
  return (
    <div
      className={`relative min-w-22 h-10 px-4 py-2 flex items-center justify-center transition-all duration-75 ease-in-out ${
        data.expandable
          ? "cursor-pointer hover:-translate-x-0.5 hover:-translate-y-0.5"
          : ""
      }`}
    >
      <Handle type="target" position={Position.Top} className="invisible" />

      <div
        className={"absolute inset-0 border-[1.5px] border-border bg-card"}
        style={{ boxShadow: "3px 3px 0 0 var(--border)" }}
      />

      <div className="relative flex gap-1.5">
        <span className="font-mono font-medium text-card-foreground text-center leading-none text-sm">
          {data.label}
        </span>

        {data.expandable && (
          <span
            className={
              "flex items-center justify-center size-3.5 text-xs font-bold leading-none border-[1.5px] border-border bg-primary text-primary-foreground"
            }
          >
            {data.expanded ? "−" : "+"}
          </span>
        )}
      </div>

      <Handle type="source" position={Position.Bottom} className="invisible" />
    </div>
  );
}
