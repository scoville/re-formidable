@ocaml.doc(`Strategy used during validation, on change, on blur, or on demand.
On demand validation will not be triggered automatically, and you need
to use the validate function manually.`)
module Strategy = {
  type t = [#onBlur | #onChange | #onDemand]
}

@ocaml.doc(`The value returned by a validator, ok or error`)
module Value = {
  type t<'value, 'error> = result<'value, 'error>
}

@ocaml.doc(`The validator is the function that performs the validation`)
module Validator = {
  @ocaml.doc(`The arguments provided to the validator function`)
  module Args = {
    type t<'values, 'value, 'error> = {
      label: option<string>,
      lens: Optic.Lens.t<'values, 'value>,
      name: string,
      value: 'value,
      values: 'values,
    }
  }

  type t<'values, 'value, 'error> = Args.t<'values, 'value, 'error> => Value.t<'value, 'error>
}

module Description = {
  @ocaml.doc(`Only the first "kind" is meant to be used by an application.
The second one is used only internally when composing validations`)
  type kind<'label> = [#name('label) | #names(array<'label>)]

  let resolveKind = (kind: kind<'label>) =>
    switch kind {
    | #name(label) => [label]
    | #names(labels) => labels
    }

  type t<'values, 'value, 'error, 'label> = (kind<'label>, Validator.t<'values, 'value, 'error>)
}

type t<'values, 'value, 'error, 'label> = (
  Strategy.t,
  Description.t<'values, 'value, 'error, 'label>,
)

let compose = ((names, validator), (names', validator')) => (
  names->Description.resolveKind->Js.Array2.concat(names'->Description.resolveKind)->#names,
  field =>
    switch validator(field) {
    | Ok(_) => validator'(field)
    | Error(_) as error => error
    },
)

let getStrategy = ((strategy, _)) => strategy

let getNames = ((_, (names, _))) => names->Description.resolveKind

let getValidator = ((_, (_, validator))) => validator

@ocaml.doc(`Returns true if a validation should be performed in the given context.
If no context is provided, always returns true`)
let shouldValidate = (~context, ~strategy) =>
  switch (context, strategy) {
  | (None, _)
  | (Some(#onChange), #onChange)
  | (Some(#onBlur), #onChange)
  | (Some(#onBlur), #onBlur) => true
  | _ => false
  }
