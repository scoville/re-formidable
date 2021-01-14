include Formidable.Validations

let required = {
  Description.names: list{"required"},
  validator: ({label, value}) =>
    switch value {
    | "" => #error(#error(("required", label)))
    | _ => #ok(value)
    },
}

let emailRegEx = %re("/.+@.+/")

let email = {
  Description.names: list{"email"},
  validator: ({label, value}) =>
    if emailRegEx->Js.Re.test_(value) {
      #ok(value)
    } else {
      #error(#error(("email", label)))
    },
}

let equals = lens => {
  Description.names: list{"equals"},
  validator: ({label, value, values}) =>
    if lens.Optic.Lens.get(values) == value {
      #ok(value)
    } else {
      #error(#error(("equals", label)))
    },
}
