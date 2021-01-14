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

module type Handlers = {
  type values

  type error

  let onSubmit: option<values => unit>

  let onSubmitError: option<(values, array<error>) => unit>
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

  let use: unit => Hook.t<values, error>

  module Provider: {
    @bs.obj
    external makeProps: (
      ~children: React.element,
      ~key: string=?,
      unit,
    ) => {"children": React.element} = ""

    let make: React.component<{"children": React.element}>
  }

  module Consumer: {
    type children = Hook.t<values, error> => React.element

    @bs.obj
    external makeProps: (~children: children, ~key: string=?, unit) => {"children": children} = ""

    let make: React.component<{"children": children}>
  }

  module Field: {
    type children<'value> = Props.Field.t<'value, error> => React.element

    @bs.obj
    external makeProps: (
      ~name: string,
      ~lens: Optic.Lens.t<values, 'value>,
      ~children: children<'value>,
      ~label: string=?,
      ~errorLabel: string=?,
      ~onBlur: ReactEvent.Focus.t => unit=?,
      ~onChange: 'value => unit=?,
      ~onFocus: ReactEvent.Focus.t => unit=?,
      ~validations: array<Validations.t<values, 'value, error>>=?,
      ~disable: bool=?,
      ~key: string=?,
      unit,
    ) => {
      "name": string,
      "lens": Optic.Lens.t<values, 'value>,
      "children": children<'value>,
      "label": option<string>,
      "errorLabel": option<string>,
      "onBlur": option<ReactEvent.Focus.t => unit>,
      "onChange": option<'value => unit>,
      "onFocus": option<ReactEvent.Focus.t => unit>,
      "validations": option<array<Validations.t<values, 'value, error>>>,
      "disable": option<bool>,
    } = ""

    let make: React.component<{
      "name": string,
      "lens": Optic.Lens.t<values, 'value>,
      "children": children<'value>,
      "label": option<string>,
      "errorLabel": option<string>,
      "onBlur": option<ReactEvent.Focus.t => unit>,
      "onChange": option<'value => unit>,
      "onFocus": option<ReactEvent.Focus.t => unit>,
      "validations": option<array<Validations.t<values, 'value, error>>>,
      "disable": option<bool>,
    }>
  }

  @bs.obj
  external makeProps: (
    ~action: string=?,
    ~method_: FormMethod.t=?,
    ~preventDefault: bool=?,
    ~stopPropagation: bool=?,
    ~className: string=?,
    ~disable: bool=?,
    ~children: React.element,
    ~key: string=?,
    unit,
  ) => {
    "action": option<string>,
    "method_": option<FormMethod.t>,
    "preventDefault": option<bool>,
    "stopPropagation": option<bool>,
    "className": option<string>,
    "disable": option<bool>,
    "children": React.element,
  } = ""

  let make: React.component<{
    "action": option<string>,
    "method_": option<FormMethod.t>,
    "preventDefault": option<bool>,
    "stopPropagation": option<bool>,
    "className": option<string>,
    "disable": option<bool>,
    "children": React.element,
  }>
}

module Make = (
  Values: Values,
  Error: Type,
  Handlers: Handlers with type values = Values.t and type error = Error.t,
): (Form with type values = Values.t and type error = Error.t) => {
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

  let use = () => {
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

      switch (Handlers.onSubmit, Handlers.onSubmitError, States.getErrors(fields)) {
      | (Some(onSubmit), _, []) => onSubmit(values)
      | (_, Some(onSubmitError), errors) => onSubmitError(values, errors)
      | _ => ()
      }

      setFields(fields)

      Js.Promise.resolve(fields)
    }

    {Hook.reset: reset, setValues: setValues, state: state, submit: submit}
  }

  module Provider = {
    @bs.obj
    external makeProps: (
      ~children: React.element,
      ~key: string=?,
      unit,
    ) => {"children": React.element} = ""

    let make = props => {
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

      React.createElement(provider, {"children": props["children"], "value": (state, modifiers)})
    }
  }

  module Consumer = {
    type children = Hook.t<values, error> => React.element

    @bs.obj
    external makeProps: (~children: children, ~key: string=?, unit) => {"children": children} = ""

    let make = props => {
      let hook = use()
      let children: children = (props["children"]: children)

      children(hook)
    }
  }

  module Field = {
    type children<'value> = Props.Field.t<'value, error> => React.element

    @bs.obj
    external makeProps: (
      ~name: string,
      ~lens: Optic.Lens.t<values, 'value>,
      ~children: children<'value>,
      ~label: string=?,
      ~errorLabel: string=?,
      ~onBlur: ReactEvent.Focus.t => unit=?,
      ~onChange: 'value => unit=?,
      ~onFocus: ReactEvent.Focus.t => unit=?,
      ~validations: array<Validations.t<values, 'value, error>>=?,
      ~disable: bool=?,
      ~key: string=?,
      unit,
    ) => {
      "name": string,
      "lens": Optic.Lens.t<values, 'value>,
      "children": children<'value>,
      "label": option<string>,
      "errorLabel": option<string>,
      "onBlur": option<ReactEvent.Focus.t => unit>,
      "onChange": option<'value => unit>,
      "onFocus": option<ReactEvent.Focus.t => unit>,
      "validations": option<array<Validations.t<values, 'value, error>>>,
      "disable": option<bool>,
    } = ""

    let make = props => {
      let ({Context.isDisabled: isDisabled, fields, values}, modifiers) = React.useContext(context)

      let field = fields->Map.String.get(props["name"])
      let children = props["children"]
      let value = Optic.Lens.get(props["lens"], values)
      let (isFocused, setIsFocused) = React.useState(() => false)

      let validationNames =
        props["validations"]->Option.mapWithDefault([], validations =>
          validations->ArrayExtra.flatMap(validation =>
            validation->Validations.getNames->List.toArray
          )
        )

      let hasValidation = name => validationNames->Js.Array2.includes(name)

      let validate = React.useCallback1((validationContext, values) =>
        props["validations"]->Option.mapWithDefault(#valid, validations =>
          switch validations->Array.keepMap(((strategy, {validator})) =>
            if Validations.shouldValidate(~context=validationContext, ~strategy) {
              switch validator({
                Validations.Validator.Args.label: props["errorLabel"]->OptionExtra.or(
                  props["label"],
                ),
                lens: props["lens"],
                name: props["name"],
                value: Optic.Lens.get(props["lens"], values),
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
        modifiers.updateField(props["name"], field => {
          ...field,
          status: validate(validationContext, values),
        })

      let onBlur = event => {
        setIsFocused(_ => false)

        props["onBlur"]->Option.forEach(onBlur => onBlur(event))

        validateAndUpdate(Some(#onBlur), values)
      }

      let onFocus = event => {
        setIsFocused(_ => true)

        props["onFocus"]->Option.forEach(onFocus => onFocus(event))

        if field->Option.mapWithDefault(false, States.Field.isPristine) {
          modifiers.updateField(props["name"], field => {...field, status: #touched})
        }
      }

      let onChange = value => {
        let onChange' = props["onChange"]

        let values = Optic.Lens.set(props["lens"], value, values)

        onChange'->Option.forEach(onChange' => onChange'(Optic.Lens.get(props["lens"], values)))

        modifiers.setValues(values)
        validateAndUpdate(Some(#onChange), values)
      }

      let setStatus = status =>
        modifiers.updateField(props["name"], field => {...field, status: status})

      React.useEffect0(() => {
        if Option.isNone(field) {
          modifiers.setField(props["name"], {States.Field.status: #pristine, validate: validate})
        }
        None
      })

      children({
        Props.Field.isDisabled: isDisabled || props["disable"]->Option.getWithDefault(false),
        isFocused: isFocused,
        label: props["label"],
        name: props["name"],
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

  @bs.obj
  external makeProps: (
    ~action: string=?,
    ~method_: FormMethod.t=?,
    ~preventDefault: bool=?,
    ~stopPropagation: bool=?,
    ~className: string=?,
    ~disable: bool=?,
    ~children: React.element,
    ~key: string=?,
    unit,
  ) => {
    "action": option<string>,
    "method_": option<FormMethod.t>,
    "preventDefault": option<bool>,
    "stopPropagation": option<bool>,
    "className": option<string>,
    "disable": option<bool>,
    "children": React.element,
  } = ""

  let make = props => {
    let (_, {Context.setIsDisabled: setIsDisabled}) = React.useContext(context)
    let disable = props["disable"]->Option.getWithDefault(false)
    let {Hook.submit: submit} = use()

    React.useEffect1(() => {
      setIsDisabled(disable)
      None
    }, [disable])

    ReactDOMRe.createElement(
      "form",
      /* The ReactDOM.props function accepts only a `method` argument
       which doesn't compile, hence the following hack */
      ~props=Obj.magic({
        "className": props["className"],
        "action": props["action"],
        "_method": props["method_"]->Option.map(FormMethod.toString),
        "onSubmit": Events.handle'(
          ~preventDefault=?props["preventDefault"],
          ~stopPropagation=?props["stopPropagation"],
          submit,
        ),
      }),
      [props["children"]],
    )
  }
}

type t<'values, 'error> = module(Form with type error = 'error and type values = 'values)

let make = (
  type values error,
  ~values as module(Values: Values with type t = values),
  ~error as module(Error: Type with type t = error),
  ~onSubmit=?,
  ~onSubmitError=?,
  (),
): t<values, error> => {
  module Handlers = {
    type error = Error.t

    type values = Values.t

    let onSubmit = onSubmit

    let onSubmitError = onSubmitError
  }
  module(Make(Values, Error, Handlers))
}

let use = (~values, ~error, ~onSubmit=?, ~onSubmitError=?, ()) =>
  React.useMemo0(() => make(~values, ~error, ~onSubmit?, ~onSubmitError?, ()))

let use1 = (~values, ~error, ~onSubmit=?, ~onSubmitError=?) =>
  React.useMemo1(() => make(~values, ~error, ~onSubmit?, ~onSubmitError?, ()))
