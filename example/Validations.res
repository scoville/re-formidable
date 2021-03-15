include Formidable.Validations

module Label = {
  type t = [#required | #email | #equals]

  let eq = (label1, label2) => label1 == label2
}

let required = (
  #name(#required),
  ({Validator.Args.label: label, value}) =>
    switch value {
    | "" => Error(#error(("required", label)))
    | _ => Ok(value)
    },
)

let emailRegEx = %re("/.+@.+/")

let email = (
  #name(#email),
  ({Validator.Args.label: label, value}) =>
    if emailRegEx->Js.Re.test_(value) {
      Ok(value)
    } else {
      Error(#error(("email", label)))
    },
)

let equals = lens => (
  #name(#equals),
  ({Validator.Args.label: label, value, values}) =>
    if lens.Optic.Lens.get(values) == value {
      Ok(value)
    } else {
      Error(#error(("equals", label)))
    },
)
