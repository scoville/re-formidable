open Relude.Globals;

module Test = {
  [@react.component]
  let make = (~id, ~children) =>
    React.cloneElement(children, {"data-testid": id});
};

module Errors = {
  [@react.component]
  let make = (~value: list(I18n.Error.t)) => {
    <div style={ReactDOMStyle.make(~color="red", ())}>
      {value
       |> Array.fromList
       |> Array.map((`error(name, _) as error) =>
            <div key=name>
              {("Field has errors: " ++ I18n.translate(error))->React.string}
            </div>
          )
       |> React.array}
    </div>;
  };
};

module Status = {
  [@react.component]
  let make = (~value) =>
    <div>
      {switch (value) {
       | `pristine => "Field is pristine"->React.string
       | `touched => "Field is touched"->React.string
       | `valid => "Field is valid"->React.string
       | `errors(errors) => <Errors value=errors />
       }}
    </div>;
};

module Label = {
  [@react.component]
  let make = (~required, ~value) =>
    value
    |> Option.fold(React.null, value =>
         <div>
           value->React.string
           {required
              ? <span style={ReactDOMStyle.make(~color="red", ())}>
                  "*"->React.string
                </span>
              : React.null}
         </div>
       );
};

module TextInput = {
  [@react.component]
  let make =
    React.memo(
      (
        ~field as {
          Formidable.Props.Field.isDisabled,
          isFocused,
          label,
          name,
          onBlur,
          onChange,
          onFocus,
          status,
          validationNames,
          value,
        },
      ) =>
      <div>
        <Label
          required={validationNames |> FormidableValidations.has("required")}
          value=label
        />
        <Test id=name>
          <input
            disabled=isDisabled
            name
            onBlur
            onChange={Formidable.Events.handle(onChange)}
            onFocus
            value
          />
        </Test>
        <div> (isFocused ? "Focus" : "Blur")->React.string </div>
        <Status value=status />
      </div>
    );
};
