type sliderProps = {
  className?: string,
  value?: array<int>,
  onValueChange?: array<int> => unit,
  min?: int,
  max?: int,
  step?: int,
}

@module("./slider") @react.component(: sliderProps)
external make: unit => React.element = "Slider"
