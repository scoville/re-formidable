module Test = {
  @react.component
  let make = (~id, ~children) => React.cloneElement(children, {"data-testid": id})
}

module Errors = {
  @react.component
  let make = (~value: array<I18n.Error.t>) => {
    <div style={ReactDOMStyle.make(~color="red", ())}>
      {value
      ->Js.Array2.map((#error(name, _) as error) =>
        <div key=name> {`Field has errors: ${I18n.translate(error)}`->React.string} </div>
      )
      ->React.array}
    </div>
  }
}

module Status = {
  @react.component
  let make = (~validating, ~value) =>
    if validating {
      <div> {"Validating..."->React.string} </div>
    } else {
      <div>
        {switch value {
        | #pristine => "Field is pristine"->React.string
        | #touched => "Field is touched"->React.string
        | #valid => "Field is valid"->React.string
        | #errors(errors) => <Errors value=errors />
        }}
      </div>
    }
}

module Label = {
  @react.component
  let make = (~required, ~value) =>
    value->Belt.Option.mapWithDefault(React.null, value =>
      <div>
        {value->React.string}
        {required
          ? <span style={ReactDOMStyle.make(~color="red", ())}> {"*"->React.string} </span>
          : React.null}
      </div>
    )
}

module TextInput = {
  @react.component
  let make = React.memo((
    ~field as {
      isDisabled,
      isFocused,
      label,
      name,
      onBlur,
      onChange,
      onFocus,
      status,
      hasValidation,
      value,
    }: Formidable.Props.Field.t<_, _, Validations.Label.t>,
    ~validating=false,
  ) =>
    <div>
      <Label required={hasValidation(#required)} value=label />
      <Test id=name>
        <input
          disabled={isDisabled || validating}
          name
          onBlur
          onChange={Formidable.Events.handleWithValue(onChange)}
          onFocus
          value
        />
      </Test>
      <div> {(isFocused ? "Focus" : "Blur")->React.string} </div>
      <Status validating value=status />
    </div>
  )
}
