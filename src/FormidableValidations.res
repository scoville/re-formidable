@ocaml.doc(`Strategy used during validation, on change, on blur, or on demand.
On demand validation will not be triggered automatically, and you need
to use the validate function manually.`)
module Strategy = {
  type t = [#onBlur | #onChange | #onDemand]
}

@ocaml.doc(`The value returned by a validator, ok or error`)
module Value = {
  type t<'value, 'error> = [#ok('value) | #error('error)]
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
  type t<'values, 'value, 'error> = {
    names: list<string>,
    validator: Validator.t<'values, 'value, 'error>,
  }
}

type t<'values, 'value, 'error> = (Strategy.t, Description.t<'values, 'value, 'error>)

let compose = (
  {Description.names: names, validator},
  {Description.names: names', validator: validator'},
) => {
  Description.names: names->List.concat(names'),
  validator: field =>
    switch validator(field) {
    | #ok(_) => validator'(field)
    | #error(_) as error => error
    },
}

let getStrategy = ((strategy, _)) => strategy

let getNames = ((_, {Description.names: names})) => names

let getValidator = ((_, {Description.validator: validator})) => validator

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
