include Formidable.Validations

module Label = Id.MakeComparable({
  type t = [#required | #email | #equals]

  let cmp = Pervasives.compare
})

let required = (
  #name(#required),
  ({Validator.Args.label: label, value}) =>
    switch value {
    | "" => #error(#error(("required", label)))
    | _ => #ok(value)
    },
)

let emailRegEx = %re("/.+@.+/")

let email = (
  #name(#email),
  ({Validator.Args.label: label, value}) =>
    if emailRegEx->Js.Re.test_(value) {
      #ok(value)
    } else {
      #error(#error(("email", label)))
    },
)

let equals = lens => (
  #name(#equals),
  ({Validator.Args.label: label, value, values}) =>
    if lens.Optic.Lens.get(values) == value {
      #ok(value)
    } else {
      #error(#error(("equals", label)))
    },
)
