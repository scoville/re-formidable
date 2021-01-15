open FormidableExtra

module Events = FormidableEvents
module Validations = FormidableValidations

module type Type = {
  type t
}

module type Values = {
  include Type

  let init: t
}

module States = {
  module Field = {
    module Status = {
      type t<'error> = [#pristine | #valid | #touched | #errors(array<'error>)]
    }

    @bs.deriving(accessors)
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

  let isPristine = fields => fields->Map.String.every(_ => Field.isPristine)

  let isValid = fields => fields->Map.String.every(_ => Field.isValid)

  let isTouched = fields => fields->Map.String.some(_ => Field.isTouched)

  let hasErrors = fields => fields->Map.String.some(_ => Field.hasErrors)

  let getErrors = fields =>
    fields->Map.String.reduce([], (acc, _, field) =>
      field->Field.getErrors->Option.mapWithDefault(acc, errors => acc->Js.Array2.concat(errors))
    )

  let reset = fields => fields->Map.String.map(Field.reset)
}

module Props = {
  module Field = {
    type t<'value, 'error> = {
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
      hasValidation: string => bool,
      value: 'value,
    }
  }
}

module Context = {
  type state<'values, 'error> = {
    fields: Map.String.t<States.Field.t<'values, 'error>>,
    isDisabled: bool,
    values: 'values,
  }

  type modifiers<'values, 'error> = {
    removeField: Map.String.key => unit,
    setIsDisabled: bool => unit,
    setField: (Map.String.key, States.Field.t<'values, 'error>) => unit,
    setFields: Map.String.t<States.Field.t<'values, 'error>> => unit,
    setValues: 'values => unit,
    updateField: (
      Map.String.key,
      States.Field.t<'values, 'error> => States.Field.t<'values, 'error>,
    ) => unit,
  }

  type t<'values, 'error> = (state<'values, 'error>, modifiers<'values, 'error>)
}

module Hook = {
  type t<'values, 'error> = {
    reset: unit => unit,
    setValues: ('values => 'values) => unit,
    state: Context.state<'values, 'error>,
    submit: unit => Js.Promise.t<Map.String.t<States.Field.t<'values, 'error>>>,
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

  let use: (
    ~onSuccess: values => unit=?,
    ~onError: (values, array<error>) => unit=?,
    unit,
  ) => Hook.t<values, error>

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
    type children<'value> = Props.Field.t<'value, error> => React.element

    @react.component
    let make: (
      ~name: string,
      ~lens: Optic.Lens.t<values, 'value>,
      ~label: string=?,
      ~errorLabel: string=?,
      ~onBlur: ReactEvent.Focus.t => unit=?,
      ~onChange: 'value => unit=?,
      ~onFocus: ReactEvent.Focus.t => unit=?,
      ~validations: array<Validations.t<values, 'value, error>>=?,
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
    ~onSubmit: unit => Js.Promise.t<Map.String.t<States.Field.t<values, error>>>=?,
    ~children: React.element,
  ) => React.element
}

module Make = (Values: Values, Error: Type): (
  Form with type values = Values.t and type error = Error.t
) => {
  type error = Error.t

  type values = Values.t

  let context = React.createContext((
    {
      Context.fields: Map.String.empty,
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

  let use = (~onSuccess=?, ~onError=?, ()) => {
    let (
      {Context.fields: fields, values} as state,
      {Context.setFields: setFields, setValues},
    ) = React.useContext(context)

    let reset = () => {
      setFields(States.reset(fields))
      setValues(Values.init)
    }

    let setValues = f => setValues(f(values))

    let submit = () => {
      let fields = fields->Map.String.map(({States.Field.validate: validate} as field) => {
        ...field,
        status: validate(None, values),
      })

      switch (States.getErrors(fields), onSuccess, onError) {
      | ([], Some(onSuccess), _) => onSuccess(values)
      | (errors, _, Some(onError)) => onError(values, errors)
      | _ => ()
      }

      setFields(fields)

      Js.Promise.resolve(fields)
    }

    {Hook.reset: reset, setValues: setValues, state: state, submit: submit}
  }

  module Provider = {
    @react.component
    let make = (~children) => {
      let provider = React.Context.provider(context)
      let (state, _) = React.useContext(context)
      let (state, setState) = React.useState(() => state)

      let modifiers = {
        Context.removeField: name =>
          setState(state => {...state, fields: state.fields->Map.String.remove(name)}),
        setIsDisabled: isDisabled => setState(state => {...state, isDisabled: isDisabled}),
        setField: (name, field) =>
          setState(state => {...state, fields: state.fields->Map.String.set(name, field)}),
        setFields: fields => setState(state => {...state, fields: fields}),
        setValues: values => setState(state => {...state, values: values}),
        updateField: (name, f) =>
          setState(state => {
            ...state,
            fields: state.fields->Map.String.update(name, field =>
              field->Option.map(field => f(field))
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
    type children<'value> = Props.Field.t<'value, error> => React.element

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

      let field = fields->Map.String.get(name)
      let children = children
      let value = Optic.Lens.get(lens, values)
      let (isFocused, setIsFocused) = React.useState(() => false)

      let validationNames =
        validations->Option.mapWithDefault([], validations =>
          validations->ArrayExtra.flatMap(validation =>
            validation->Validations.getNames->List.toArray
          )
        )

      let hasValidation = name => validationNames->Js.Array2.includes(name)

      let validate = React.useCallback1((validationContext, values) =>
        validations->Option.mapWithDefault(#valid, validations =>
          switch validations->Array.keepMap(((strategy, {validator})) =>
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

        onBlur->Option.forEach(onBlur => onBlur(event))

        validateAndUpdate(Some(#onBlur), values)
      }

      let onFocus = event => {
        setIsFocused(_ => true)

        onFocus->Option.forEach(onFocus => onFocus(event))

        if field->Option.mapWithDefault(false, States.Field.isPristine) {
          modifiers.updateField(name, field => {...field, status: #touched})
        }
      }

      let onChange = value => {
        let onChange' = onChange

        let values = Optic.Lens.set(lens, value, values)

        onChange'->Option.forEach(onChange' => onChange'(Optic.Lens.get(lens, values)))

        modifiers.setValues(values)
        validateAndUpdate(Some(#onChange), values)
      }

      let setStatus = status => modifiers.updateField(name, field => {...field, status: status})

      React.useEffect0(() => {
        if Option.isNone(field) {
          modifiers.setField(name, {States.Field.status: #pristine, validate: validate})
        }
        None
      })

      children({
        Props.Field.isDisabled: isDisabled || disable->Option.getWithDefault(false),
        isFocused: isFocused,
        label: label,
        name: name,
        onBlur: onBlur,
        onChange: onChange,
        onFocus: onFocus,
        setStatus: setStatus,
        status: field->Option.mapWithDefault(#pristine, States.Field.status),
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
    let disable = disable->Option.getWithDefault(false)

    React.useEffect1(() => {
      setIsDisabled(disable)
      None
    }, [disable])

    <form
      ?className
      ?action
      method=?{method_->Option.map(FormMethod.toString)}
      onSubmit={Events.handle(~preventDefault?, ~stopPropagation?, () =>
        switch onSubmit {
        | None => ignore()
        | Some(submit) => ignore(submit())
        }
      )}>
      children
    </form>
  }
}

type t<'values, 'error> = module(Form with type error = 'error and type values = 'values)

let make = (
  type values error,
  ~values as module(Values: Values with type t = values),
  ~error as module(Error: Type with type t = error),
): t<values, error> => {
  module(Make(Values, Error))
}

let use = (
  type values error,
  ~values: module(Values with type t = values),
  ~error: module(Type with type t = error),
  ~onSuccess=?,
  ~onError=?,
  (),
) => {
  let module(Form) = make(~values, ~error)

  React.useMemo0(() => {
    Form.use(~onSuccess?, ~onError?, ())
  })
}

let use1 = (
  type values error,
  ~values: module(Values with type t = values),
  ~error: module(Type with type t = error),
  ~onSuccess=?,
  ~onError=?,
) => {
  let module(Form) = make(~values, ~error)

  React.useMemo1(() => {
    Form.use(~onSuccess?, ~onError?, ())
  })
}
