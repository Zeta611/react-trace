type skeletonProps = {className?: string}

@module("./skeleton") @react.component(: skeletonProps)
external make: unit => React.element = "Skeleton"
