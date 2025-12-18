import { NodeProps } from "@xyflow/react";
import type { MountingGroupNode } from "@/shared/layout/types";

export default function MountingGroupNodeComponent({
  data,
}: NodeProps<MountingGroupNode>) {
  return (
    <div className="w-full h-full rounded-lg border-2 border-dashed border-amber-400/60 bg-amber-50/30 dark:bg-amber-950/20">
      <div className="absolute -top-6 left-2 px-2 py-0.5 text-xs font-semibold text-amber-600 dark:text-amber-400 bg-amber-100/80 dark:bg-amber-900/60 rounded">
        {data.label}
      </div>
    </div>
  );
}
