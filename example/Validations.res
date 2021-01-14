open Formidable.Validations

module Label = {
  type t = [#required | #email | #equals]
}

module Make = (
  Values: {
    type t
  },
) => {
  module Description = Description.Make(Values, Label)

  let required = Description.make(~name=#required, ({label, value}) =>
    switch value {
    | "" => #error(#error(("required", label)))
    | _ => #ok(value)
    }
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
}
