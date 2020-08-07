open Relude.Globals
include Formidable.Validations
open Validator.Args
open Optic

let required =
  ( [ "required" ],
    fun { label; value } ->
      if String.isEmpty value then `error (`error ("required", label))
      else `ok value )

let emailRegEx = [%re "/.+@.+/"]

let email =
  ( [ "email" ],
    fun { label; value } ->
      if Js.Re.test_ emailRegEx value then `ok value
      else `error (`error ("email", label)) )

let equals lens =
  ( [ "equals" ],
    fun { label; value; values } ->
      if Lens.view lens values == value then `ok value
      else `error (`error ("equals", label)) )
