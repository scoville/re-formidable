include Formidable.Validations;
open Validator.Args;
open Optic;

let required = (
  ["required"],
  ({label, value}) =>
    if (value == "") {
      `error(`error(("required", label)));
    } else {
      `ok(value);
    },
);

let emailRegEx = [%re "/.+@.+/"];

let email = (
  ["email"],
  ({label, value}) =>
    if (Js.Re.test_(emailRegEx, value)) {
      `ok(value);
    } else {
      `error(`error(("email", label)));
    },
);

let equals = lens => (
  ["equals"],
  ({label, value, values}) =>
    if (Lens.get(lens, values) === value) {
      `ok(value);
    } else {
      `error(`error(("equals", label)));
    },
);
