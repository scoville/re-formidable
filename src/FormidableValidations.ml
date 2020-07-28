(** Strategy used during validation, on change, on blur, or on demand.
  * On demand validation will not be triggered automatically, and you need
  * to use the validate function manually.
  *)
module Strategy = struct
  type t = [ `onBlur | `onChange | `onDemand ]
end

(** The value returned by a validator, ok or error *)
module Value = struct
  type ('value, 'error) t = [ `ok of 'value | `error of 'error ]
end

(** The validator is the function that performs the validation *)
module Validator = struct
  (** The arguments provided to the validator function *)
  module Args = struct
    type ('values, 'value, 'error) t = {
      label : string option;
      lens : ('values, 'value) Optic.Lens.t;
      name : string;
      value : 'value;
      values : 'values;
    }
  end

  type ('values, 'value, 'error) t =
    ('values, 'value, 'error) Args.t -> ('value, 'error) Value.t
end

module Pair = struct
  type ('values, 'value, 'error) t =
    string * ('values, 'value, 'error) Validator.t
end

type ('values, 'value, 'error) t = Strategy.t * ('values, 'value, 'error) Pair.t

let compose (name, validator) (name', validator') =
  ( name ^ "<or>" ^ name',
    fun field ->
      match validator field with
      | `ok _ -> validator' field
      | `error _ as error -> error )

let ( >>> ) = compose

let getStrategy (strategy, _) = strategy

let getName (_, (name, _)) = name

let getValidator (_, (_, validator)) = validator

(** Returns true if a validation should be performed in the given context.
    If no context is provided, always returns true *)
let shouldValidate ~context ~strategy =
  match (context, strategy) with
  | None, _
  | Some `onChange, `onChange
  | Some `onBlur, `onChange
  | Some `onBlur, `onBlur ->
      true
  | _ -> false
