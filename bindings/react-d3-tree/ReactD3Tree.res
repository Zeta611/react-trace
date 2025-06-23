type rec data = {name?: string, children?: array<data>}

type translation = {
  x: float,
  y: float,
}

@module("react-d3-tree") @react.component
external make: (
  ~data: data,
  ~translate: translation=?,
  ~orientation: string=?,
  ~pathFunc: string=?,
  unit,
) => React.element = "default"
