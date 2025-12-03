"use client";

import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";

import InfoPane from "@/components/info-pane";
import CodePane from "@/components/code-pane";
import VizPane from "@/components/viz-pane";

export default function WorkSpace() {
  return (
    <ResizablePanelGroup direction="vertical">
      <ResizablePanel defaultSize={75}>
        <ResizablePanelGroup direction="horizontal">
          <ResizablePanel defaultSize={50}>
            <CodePane />
          </ResizablePanel>
          <ResizableHandle withHandle />
          <ResizablePanel defaultSize={50}>
            <VizPane />
          </ResizablePanel>
        </ResizablePanelGroup>
      </ResizablePanel>
      <ResizableHandle withHandle />
      <ResizablePanel
        className="relative"
        defaultSize={25}
        collapsible
        collapsedSize={8}
      >
        <InfoPane />
      </ResizablePanel>
    </ResizablePanelGroup>
  );
}
