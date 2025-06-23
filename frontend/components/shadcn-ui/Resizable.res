module PanelGroup = {
  type resizablePanelGroupProps = {
    children: React.element,
    className?: string,
    direction?: string /* "horizontal" | "vertical" */,
  }

  @module("./resizable") @react.component(: resizablePanelGroupProps)
  external make: resizablePanelGroupProps => React.element = "ResizablePanelGroup"
}

module Panel = {
  type resizablePanelProps = {
    children: React.element,
    className?: string,
    defaultSize?: int,
    minSize?: int,
    maxSize?: int,
  }

  @module("./resizable") @react.component(: resizablePanelProps)
  external make: resizablePanelProps => React.element = "ResizablePanel"
}

module Handle = {
  type resizableHandleProps = {
    className?: string,
    children?: React.element,
    withHandle?: bool,
  }

  @module("./resizable") @react.component(: resizableHandleProps)
  external make: resizableHandleProps => React.element = "ResizableHandle"
}
