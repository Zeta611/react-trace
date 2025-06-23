@module("@uiw/react-codemirror") @react.component
external make: (
  ~value: string,
  ~height: string=?,
  ~onChange: string => unit=?,
  ~extensions: array<'extension>=?,
  ~className: string=?,
) => React.element = "default"
