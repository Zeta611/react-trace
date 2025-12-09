import { Handle, Position, NodeProps } from "@xyflow/react";
import type { TreeNode } from "@/shared/layout/types";
import { cn } from "@/shared/utils";
import { useAppState } from "@/store/use-app-state";

export default function TreeNodeComponent({ data }: NodeProps<TreeNode>) {
  const triggerEvent = useAppState.use.triggerEvent();
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
          className="absolute inset-0 border-[1.5px] border-border bg-card pointer-events-none"
          style={{ boxShadow: "3px 3px 0 0 var(--border)" }}
        />

        <div className="relative flex gap-1.5 h-6 items-center">
          <span className="font-mono font-bold text-card-foreground text-center leading-none text-sm">
            {data.label}
          </span>
        </div>

        {data.handler !== undefined && (
          <button
            type="button"
            className="relative mt-2 px-2 py-1 text-[10px] font-semibold border border-border bg-yellow-300 text-foreground shadow-sm disabled:cursor-not-allowed"
            onClick={(e) => {
              e.stopPropagation();
              if (data.handler !== undefined) {
                triggerEvent(data.handler);
              }
            }}
          >
            handler
          </button>
        )}

        {data.expandable && (
          <span
            className="absolute -bottom-3 left-1/2 -translate-x-1/2 translate-y-1/2 flex items-center justify-center size-3.5 text-[6pt] font-black leading-none border-[1.5px] border-border bg-primary text-primary-foreground"
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
        {/* Header with component name and status indicators */}
        <div className="px-3 py-1.5 border-b border-border bg-blue-400 font-semibold text-card-foreground flex items-center justify-between">
          <span className="text-sm">{data.label}</span>
          <div className="flex items-center gap-1.5">
            {/* Check flag indicator */}
            <span
              className={cn(
                "size-2.5 rounded-full border border-black",
                data.dec!.chk ? "bg-red-500" : "bg-gray-300"
              )}
              title={`chk: ${data.dec!.chk}`}
            />
            {/* Effect flag indicator */}
            <span
              className={cn(
                "size-2.5 rounded-full border border-black",
                data.dec!.eff ? "bg-green-500" : "bg-gray-300"
              )}
              title={`eff: ${data.dec!.eff}`}
            />
          </div>
        </div>

        {/* Arg */}
        {data.arg && (
          <div className="px-3 py-1.5 border-b border-border">
            <span className="text-muted-foreground font-bold">Arg: </span>
            <span className="text-foreground">{data.arg}</span>
          </div>
        )}

        {/* Effect queue */}
        {data.effQSize != null && (
          <div className="px-3 py-1.5 border-b border-border">
            <span className="text-muted-foreground font-bold">Effects: </span>
            <span
              className={
                hasEffects && data.dec!.eff
                  ? "text-green-500"
                  : "text-muted-foreground"
              }
            >
              {data.effQSize} mounted
            </span>
          </div>
        )}

        {/* State store table */}
        {hasState && (
          <div className="px-3 py-1.5">
            <div className="text-muted-foreground mb-1 font-bold">States: </div>
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
                    <td className="pr-2 py-0.5">{entry.label}</td>
                    <td className="pr-2 py-0.5">{entry.value}</td>
                    <td className="py-0.5">
                      <span
                        className={
                          entry.queue_size > 0
                            ? "text-red-500"
                            : "text-muted-foreground"
                        }
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
          className="absolute -bottom-3 left-1/2 -translate-x-1/2 translate-y-1/2 flex items-center justify-center size-3.5 text-[6pt] font-black leading-none border-[1.5px] border-border bg-primary text-primary-foreground"
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
