module Dynamic = {
  type options = {
    ssr?: bool,
    loading?: unit => React.element,
  }

  @module("next/dynamic")
  external dynamic: (unit => promise<'a>, options) => 'a = "default"

  @val external import: string => promise<'a> = "import"
}
