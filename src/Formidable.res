open FormidableExtra

module Events = FormidableEvents
module Validations = FormidableValidations

module type Error = {
  type t

  let eq: (t, t) => bool
}

module type ValidationLabel = {
  type t

  let eq: (t, t) => bool
}

module type Values = {
  type t

  let init: t
}

module States = {
  module Field = {
    module Status = {
      type t<'error> = [#pristine | #valid | #touched | #errors(array<'error>)]
    }

    @deriving(accessors)
    type t<'values, 'error> = {
      status: Status.t<'error>,
      validate: (option<Validations.Strategy.t>, 'values) => Status.t<'error>,
    }

    let isPristine = field =>
      switch field {
      | {status: #pristine} => true
      | _ => false
      }

    let isValid = field =>
      switch field {
      | {status: #valid} => true
      | _ => false
      }

    let isTouched = field =>
      switch field {
      | {status: #touched} => true
      | _ => false
      }

    let hasErrors = field =>
      switch field {
      | {status: #errors(_)} => true
      | _ => false
      }

    let getErrors = field =>
      switch field {
      | {status: #errors(errors)} => Some(errors)
      | _ => None
      }

    let reset = field => {...field, status: #pristine}
  }

  module Form = {
    module Status = {
      type t<'values, 'error> = [
        | #pristine
        | #touched
        | #submitted([#valid('values) | #errors('values, array<'error>)])
      ]
    }

    let isPristine = fields => fields->Belt.Map.String.every(_ => Field.isPristine)

    let isValid = fields => fields->Belt.Map.String.every(_ => Field.isValid)

    let isTouched = fields => fields->Belt.Map.String.some(_ => Field.isTouched)

    let hasErrors = fields => fields->Belt.Map.String.some(_ => Field.hasErrors)

    let getErrors = fields =>
      fields->Belt.Map.String.reduce([], (acc, _, field) =>
        field
        ->Field.getErrors
        ->Belt.Option.mapWithDefault(acc, errors => acc->Js.Array2.concat(errors))
      )

    let reset = fields => fields->Belt.Map.String.map(Field.reset)
  }
}

module Props = {
  module Field = {
    type t<'value, 'error, 'validationLabel> = {
      isDisabled: bool,
      isFocused: bool,
      label: option<string>,
      name: string,
      onBlur: ReactEvent.Focus.t => unit,
      onChange: 'value => unit,
      onFocus: ReactEvent.Focus.t => unit,
      setStatus: States.Field.Status.t<'error> => unit,
      status: States.Field.Status.t<'error>,
      validate: unit => unit,
      hasValidation: 'validationLabel => bool,
      value: 'value,
    }
  }
}

module Context = {
  type state<'values, 'error> = {
    fields: Belt.Map.String.t<States.Field.t<'values, 'error>>,
    isDisabled: bool,
    values: 'values,
  }

  type modifiers<'values, 'error> = {
    removeField: string => unit,
    setIsDisabled: bool => unit,
    setField: (string, States.Field.t<'values, 'error>) => unit,
    setFields: Belt.Map.String.t<States.Field.t<'values, 'error>> => unit,
    setValues: 'values => unit,
    updateField: (
      string,
      States.Field.t<'values, 'error> => States.Field.t<'values, 'error>,
    ) => unit,
  }

  type t<'values, 'error> = (state<'values, 'error>, modifiers<'values, 'error>)
}

module Hook = {
  type computedState<'values, 'error> = {formStatus: States.Form.Status.t<'values, 'error>}

  type t<'values, 'error> = {
    addError: (string, 'error) => unit,
    computedState: computedState<'values, 'error>,
    removeError: (string, 'error) => unit,
    reset: unit => unit,
    setFieldStatus: (string, States.Field.Status.t<'error>) => unit,
    setValues: ('values => 'values) => unit,
    state: Context.state<'values, 'error>,
    submit: ReactEvent.Form.t => unit,
  }
}

module FormMethod = {
  type t = [#get | #post]

  let toString: t => string = method => {
    switch method {
    | #get => "GET"
    | #post => "POST"
    }
  }
}

module type Form = {
  type values

  type error

  type validationLabel

  let use: (~initFormStatus: States.Form.Status.t<values, error>=?, unit) => Hook.t<values, error>

  let useOnSubmitSuccess: (~form: Hook.t<values, error>, (~values: values) => unit) => unit

  let useOnSubmitError: (
    ~form: Hook.t<values, error>,
    (~values: values, ~errors: array<error>) => unit,
  ) => unit

  module Provider: {
    @react.component
    let make: (~children: React.element) => React.element
  }

  module Consumer: {
    type children = Context.state<values, error> => React.element

    @react.component
    let make: (~children: children) => React.element
  }

  module Field: {
    type children<'value> = Props.Field.t<'value, error, validationLabel> => React.element

    @react.component
    let make: (
      ~name: string,
      ~lens: Optic.Lens.t<values, 'value>,
      ~label: string=?,
      ~errorLabel: string=?,
      ~onBlur: ReactEvent.Focus.t => unit=?,
      ~onChange: 'value => unit=?,
      ~onFocus: ReactEvent.Focus.t => unit=?,
      ~validations: array<Validations.t<values, 'value, error, validationLabel>>=?,
      ~disable: bool=?,
      ~children: children<'value>,
    ) => React.element
  }

  @react.component
  let make: (
    ~action: string=?,
    ~method_: FormMethod.t=?,
    ~preventDefault: bool=?,
    ~stopPropagation: bool=?,
    ~className: string=?,
    ~disable: bool=?,
    ~onSubmit: ReactEvent.Form.t => unit=?,
    ~children: React.element,
  ) => React.element
}

module Make = (ValidationLabel: ValidationLabel, Error: Error, Values: Values): (
  Form
    with type values = Values.t
    and type error = Error.t
    and type validationLabel = ValidationLabel.t
) => {
  type error = Error.t

  type values = Values.t

  type validationLabel = ValidationLabel.t

  let context = React.createContext((
    {
      Context.fields: Belt.Map.String.empty,
      isDisabled: false,
      values: Values.init,
    },
    {
      Context.removeField: _ => (),
      setIsDisabled: _ => (),
      setField: (_, _) => (),
      setFields: _ => (),
      setValues: _ => (),
      updateField: (_, _) => (),
    },
  ))

  let use = (~initFormStatus=#pristine, ()) => {
    let (
      {Context.fields: fields, values} as state,
      {Context.setFields: setFields, setValues, updateField},
    ) = React.useContext(context)

    let (formStatus, setFormStatus) = React.useState(() => initFormStatus)

    let reset = () => {
      setFields(States.Form.reset(fields))
      setValues(Values.init)
    }

    let setValues = f => setValues(f(values))

    let updateFieldStatus = (name, f) =>
      updateField(name, field => {...field, status: f(field.status)})

    let setFieldStatus = (name, status) => updateFieldStatus(name, _ => status)

    let addError = (name, error) =>
      updateFieldStatus(name, status => {
        switch status {
        | #touched | #pristine | #valid => #errors([error])
        | #errors(errors) if errors->Js.Array2.some(Error.eq(error)) => status
        | #errors(errors) => errors->Js.Array2.concat([error])->#errors
        }
      })

    let removeError = (name, error) =>
      updateFieldStatus(name, status =>
        switch status {
        | #touched | #pristine | #valid => status
        | #errors(errors) =>
          switch errors->Js.Array2.filter(error' => error != error') {
          | [] => #valid
          | errors => #errors(errors)
          }
        }
      )

    let submit = _event => {
      let fields = fields->Belt.Map.String.map(({States.Field.validate: validate} as field) => {
        ...field,
        status: validate(None, values),
      })

      switch States.Form.getErrors(fields) {
      | [] => setFormStatus(_ => #submitted(#valid(values)))
      | errors => setFormStatus(_ => #submitted(#errors(values, errors)))
      }

      setFields(fields)
    }

    {
      Hook.addError: addError,
      computedState: {formStatus: formStatus},
      removeError: removeError,
      reset: reset,
      setFieldStatus: setFieldStatus,
      setValues: setValues,
      state: state,
      submit: submit,
    }
  }

  let useOnSubmitSuccess = (~form as {Hook.computedState: {formStatus}}, onSuccess) => {
    React.useEffect1(() => {
      switch formStatus {
      | #submitted(#valid(values)) => onSuccess(~values)
      | #pristine | #touched | #submitted(#errors(_, _)) => ignore()
      }

      None
    }, [formStatus])
  }

  let useOnSubmitError = (~form as {Hook.computedState: {formStatus}}, onError) => {
    React.useEffect1(() => {
      switch formStatus {
      | #submitted(#errors(values, errors)) => onError(~values, ~errors)
      | #pristine | #touched | #submitted(#valid(_)) => ignore()
      }

      None
    }, [formStatus])
  }

  module Provider = {
    @react.component
    let make = (~children) => {
      let provider = React.Context.provider(context)
      let (state, _) = React.useContext(context)
      let (state, setState) = React.useState(() => state)

      let modifiers = {
        Context.removeField: name =>
          setState(state => {...state, fields: state.fields->Belt.Map.String.remove(name)}),
        setIsDisabled: isDisabled => setState(state => {...state, isDisabled: isDisabled}),
        setField: (name, field) =>
          setState(state => {...state, fields: state.fields->Belt.Map.String.set(name, field)}),
        setFields: fields => setState(state => {...state, fields: fields}),
        setValues: values => setState(state => {...state, values: values}),
        updateField: (name, f) =>
          setState(state => {
            ...state,
            fields: state.fields->Belt.Map.String.update(name, field =>
              field->Belt.Option.map(field => f(field))
            ),
          }),
      }

      React.createElement(provider, {"children": children, "value": (state, modifiers)})
    }
  }

  module Consumer = {
    type children = Context.state<values, error> => React.element

    @react.component
    let make = (~children) => {
      let (state, _) = React.useContext(context)

      children(state)
    }
  }

  module Field = {
    type children<'value> = Props.Field.t<'value, error, validationLabel> => React.element

    @react.component
    let make = (
      ~name,
      ~lens,
      ~label=?,
      ~errorLabel=?,
      ~onBlur=?,
      ~onChange=?,
      ~onFocus=?,
      ~validations=?,
      ~disable=?,
      ~children,
    ) => {
      let ({Context.isDisabled: isDisabled, fields, values}, modifiers) = React.useContext(context)

      let field = fields->Belt.Map.String.get(name)
      let children = children
      let value = Optic.Lens.get(lens, values)
      let (isFocused, setIsFocused) = React.useState(() => false)

      let validationNames =
        validations->Belt.Option.mapWithDefault([], validations =>
          validations->ArrayExtra.flatMap(Validations.getNames)
        )

      let hasValidation = name => validationNames->Js.Array2.some(ValidationLabel.eq(name))

      let validate = React.useCallback1((validationContext, values) =>
        validations->Belt.Option.mapWithDefault(#valid, validations =>
          switch validations->Belt.Array.keepMap(((strategy, (_, validator))) =>
            if Validations.shouldValidate(~context=validationContext, ~strategy) {
              switch validator({
                Validations.Validator.Args.label: errorLabel->OptionExtra.or(label),
                lens: lens,
                name: name,
                value: Optic.Lens.get(lens, values),
                values: values,
              }) {
              | #ok(_) => None
              | #error(error) => Some(error)
              }
            } else {
              None
            }
          ) {
          | [] => #valid
          | errors => #errors(errors)
          }
        )
      , validationNames)

      let validateAndUpdate = (validationContext, values) =>
        modifiers.updateField(name, field => {
          ...field,
          status: validate(validationContext, values),
        })

      let onBlur = event => {
        setIsFocused(_ => false)

        onBlur->Belt.Option.forEach(onBlur => onBlur(event))

        validateAndUpdate(Some(#onBlur), values)
      }

      let onFocus = event => {
        setIsFocused(_ => true)

        onFocus->Belt.Option.forEach(onFocus => onFocus(event))

        if field->Belt.Option.mapWithDefault(false, States.Field.isPristine) {
          modifiers.updateField(name, field => {...field, status: #touched})
        }
      }

      let onChange = value => {
        let onChange' = onChange

        let values = Optic.Lens.set(lens, value, values)

        onChange'->Belt.Option.forEach(onChange' => onChange'(Optic.Lens.get(lens, values)))

        modifiers.setValues(values)

        validateAndUpdate(Some(#onChange), values)
      }

      let setStatus = status => modifiers.updateField(name, field => {...field, status: status})

      React.useEffect0(() => {
        if Belt.Option.isNone(field) {
          modifiers.setField(name, {States.Field.status: #pristine, validate: validate})
        }

        None
      })

      children({
        Props.Field.isDisabled: isDisabled || disable->Belt.Option.getWithDefault(false),
        isFocused: isFocused,
        label: label,
        name: name,
        onBlur: onBlur,
        onChange: onChange,
        onFocus: onFocus,
        setStatus: setStatus,
        status: field->Belt.Option.mapWithDefault(#pristine, States.Field.status),
        validate: () => validateAndUpdate(None, values),
        hasValidation: hasValidation,
        value: value,
      })
    }
  }

  @react.component
  let make = (
    ~action=?,
    ~method_=?,
    ~preventDefault=?,
    ~stopPropagation=?,
    ~className=?,
    ~disable=?,
    ~onSubmit=?,
    ~children,
  ) => {
    let (_, {Context.setIsDisabled: setIsDisabled}) = React.useContext(context)
    let disable = disable->Belt.Option.getWithDefault(false)

    React.useEffect1(() => {
      setIsDisabled(disable)
      None
    }, [disable])

    <form
      ?className
      ?action
      method=?{method_->Belt.Option.map(FormMethod.toString)}
      onSubmit={Events.handle(~preventDefault?, ~stopPropagation?, event =>
        switch onSubmit {
        | None => ignore()
        | Some(onSubmit) => ignore(onSubmit(event))
        }
      )}>
      children
    </form>
  }
}

type t<'validationLabel, 'error, 'values> = module(Form with
  type validationLabel = 'validationLabel
  and type error = 'error
  and type values = 'values
)

let make = (
  type validationLabel error values,
  ~validationLabel as module(ValidationLabel: ValidationLabel with type t = validationLabel),
  ~error as module(Error: Error with type t = error),
  ~values as module(Values: Values with type t = values),
): t<validationLabel, error, values> => {
  module(Make(ValidationLabel, Error, Values))
}

let use = (
  type validationLabel error values,
  ~validationLabel: module(ValidationLabel with type t = validationLabel),
  ~error: module(Error with type t = error),
  ~values: module(Values with type t = values),
  ~initFormStatus: option<States.Form.Status.t<values, error>>=?,
  (),
) => {
  let module(Form) = make(~validationLabel, ~error, ~values)

  React.useMemo0(() => {
    Form.use(~initFormStatus?, ())
  })
}

let use1 = (
  type validationLabel error values,
  ~validationLabel: module(ValidationLabel with type t = validationLabel),
  ~error: module(Error with type t = error),
  ~values: module(Values with type t = values),
  ~initFormStatus: option<States.Form.Status.t<values, error>>=?,
) => {
  let module(Form) = make(~validationLabel, ~error, ~values)

  React.useMemo1(() => {
    Form.use(~initFormStatus?, ())
  })
}
