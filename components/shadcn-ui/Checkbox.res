type checkboxProps = {
  ...JsxDOM.domProps,
  onCheckedChange?: bool => unit,
}

@module("./checkbox") @react.component(: checkboxProps)
external make: (~onCheckedChange: bool => unit=?) => React.element = "Checkbox"
