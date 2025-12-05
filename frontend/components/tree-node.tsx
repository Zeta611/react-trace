import { Handle, Position, NodeProps } from "@xyflow/react";
import type { TreeNode } from "@/shared/layout/types";
import { cn } from "@/shared/utils";

export default function TreeNodeComponent({ data }: NodeProps<TreeNode>) {
  // Only component nodes have dec set (decision info)
  const isComponentNode = data.dec != null;
  const hasState = Array.isArray(data.stStore) && data.stStore.length > 0;
  const hasEffects = data.effQSize != null && data.effQSize > 0;

  // Simple node for non-components (constants, closures, lists)
  // Width matches treeWidth (100px) in use-expand-collapse.ts
  if (!isComponentNode) {
    return (
      <div
        className={cn(
          "relative w-[100px] px-4 py-2 flex flex-col items-center justify-center transition-all duration-75 ease-in-out",
          data.expandable &&
            "cursor-pointer hover:-translate-x-0.5 hover:-translate-y-0.5"
        )}
      >
        <Handle type="target" position={Position.Top} className="invisible" />

        <div
          className="absolute inset-0 border-[1.5px] border-border bg-card"
          style={{ boxShadow: "3px 3px 0 0 var(--border)" }}
        />

        <div className="relative flex gap-1.5 h-6 items-center">
          <span className="font-mono font-medium text-card-foreground text-center leading-none text-sm">
            {data.label}
          </span>
        </div>

        {data.expandable && (
          <span
            className="absolute -bottom-3 left-1/2 -translate-x-1/2 translate-y-1/2 flex items-center justify-center size-3.5 text-[6pt] font-bold leading-none border-[1.5px] border-border bg-primary text-primary-foreground"
            aria-label={data.expanded ? "Collapse" : "Expand"}
            aria-expanded={data.expanded}
            role="button"
          >
            {data.expanded ? "−" : "+"}
          </span>
        )}

        <Handle
          type="source"
          position={Position.Bottom}
          className="invisible"
        />
      </div>
    );
  }

  // Rich node for components - shows internal state
  // Width matches component width (280px) in use-expand-collapse.ts
  return (
    <div
      className={cn(
        "relative w-[280px] flex flex-col transition-all duration-75 ease-in-out",
        data.expandable &&
          "cursor-pointer hover:-translate-x-0.5 hover:-translate-y-0.5"
      )}
    >
      <Handle type="target" position={Position.Top} className="invisible" />

      <div
        className="border-[1.5px] border-border bg-card text-xs font-mono"
        style={{ boxShadow: "3px 3px 0 0 var(--border)" }}
      >
        {/* Header with component name */}
        <div className="px-3 py-1.5 border-b border-border bg-muted/50 font-semibold text-card-foreground flex items-center justify-between">
          <span className="text-sm">{data.label}</span>
          <span className="text-muted-foreground text-[10px]">component</span>
        </div>

        {/* Arg */}
        {data.arg && (
          <div className="px-3 py-1.5 border-b border-border">
            <span className="text-muted-foreground">arg: </span>
            <span className="text-foreground">{data.arg}</span>
          </div>
        )}

        {/* Decision flags */}
        <div className="px-3 py-1.5 border-b border-border flex gap-3">
          <span>
            <span className="text-muted-foreground">chk: </span>
            <span
              className={data.dec!.chk ? "text-amber-600" : "text-foreground"}
            >
              {data.dec!.chk ? "true" : "false"}
            </span>
          </span>
          <span>
            <span className="text-muted-foreground">eff: </span>
            <span
              className={data.dec!.eff ? "text-green-600" : "text-foreground"}
            >
              {data.dec!.eff ? "true" : "false"}
            </span>
          </span>
        </div>

        {/* Effect queue */}
        {data.effQSize != null && (
          <div className="px-3 py-1.5 border-b border-border">
            <span className="text-muted-foreground">eff_q: </span>
            <span className={hasEffects ? "text-blue-600" : "text-foreground"}>
              {data.effQSize} pending
            </span>
          </div>
        )}

        {/* State store table */}
        {hasState && (
          <div className="px-3 py-1.5">
            <div className="text-muted-foreground mb-1">st_store:</div>
            <table className="w-full text-left">
              <thead>
                <tr className="text-muted-foreground border-b border-border/50">
                  <th className="pr-2 pb-0.5 font-medium">label</th>
                  <th className="pr-2 pb-0.5 font-medium">value</th>
                  <th className="pb-0.5 font-medium">queue</th>
                </tr>
              </thead>
              <tbody>
                {data.stStore!.map((entry, i) => (
                  <tr key={i} className="text-foreground">
                    <td className="pr-2 py-0.5 text-purple-600">
                      {entry.label}
                    </td>
                    <td className="pr-2 py-0.5">{entry.value}</td>
                    <td className="py-0.5">
                      <span
                        className={entry.queue_size > 0 ? "text-amber-600" : ""}
                      >
                        {entry.queue_size}
                      </span>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      {data.expandable && (
        <span
          className="absolute -bottom-3 left-1/2 -translate-x-1/2 translate-y-1/2 flex items-center justify-center size-3.5 text-[6pt] font-bold leading-none border-[1.5px] border-border bg-primary text-primary-foreground"
          aria-label={data.expanded ? "Collapse" : "Expand"}
          aria-expanded={data.expanded}
          role="button"
        >
          {data.expanded ? "−" : "+"}
        </span>
      )}

      <Handle type="source" position={Position.Bottom} className="invisible" />
    </div>
  );
}
