open Relude.Globals
module Events = FormidableEvents
module Validations = FormidableValidations

module type Type = sig
  type t
end

module type Values = sig
  include Type

  val init : t
end

module type Handlers = sig
  type values

  type error

  val onSubmit : values -> unit

  val onSubmitError : (values -> error list -> unit) option
end

module States = struct
  module Field = struct
    module Status = struct
      type 'error t = [ `pristine | `valid | `touched | `errors of 'error list ]
    end

    type ('values, 'error) t = {
      status : 'error Status.t;
      validate : Validations.Strategy.t option -> 'values -> 'error Status.t;
    }
    [@@bs.deriving accessors]

    let isPristine = function { status = `pristine } -> true | _ -> false

    let isValid = function { status = `valid } -> true | _ -> false

    let isTouched = function { status = `touched } -> true | _ -> false

    let hasErrors = function { status = `errors _ } -> true | _ -> false

    let getErrors = function
      | { status = `errors errors } -> Some errors
      | _ -> None

    let reset field = { field with status = `pristine }
  end

  let isPristine fields = StringMap.all (const Field.isPristine) fields

  let isValid fields = StringMap.all (const Field.isValid) fields

  let isTouched fields = not (StringMap.any (const Field.isPristine) fields)

  let hasErrors fields = StringMap.any (const Field.hasErrors) fields

  let getErrors fields =
    StringMap.foldLeft
      (fun acc _ field -> Option.fold acc (( @ ) acc) (Field.getErrors field))
      [] fields

  let reset fields = StringMap.map Field.reset fields
end

module Props = struct
  module Field = struct
    type ('value, 'error) childrenProps = {
      isDisabled : bool;
      isFocused : bool;
      label : string option;
      name : string;
      onBlur : unit -> unit;
      onChange : 'value -> unit;
      onFocus : unit -> unit;
      setStatus : 'error States.Field.Status.t -> unit;
      status : 'error States.Field.Status.t;
      validate : unit -> unit;
      validationNames : string list;
      value : 'value;
    }
  end
end

module Context = struct
  type ('values, 'error) state = {
    fields : ('values, 'error) States.Field.t StringMap.t;
    isDisabled : bool;
    values : 'values;
  }

  type ('values, 'error) modifiers = {
    removeField : StringMap.key -> unit;
    setIsDisabled : bool -> unit;
    setField : StringMap.key -> ('values, 'error) States.Field.t -> unit;
    setFields : ('values, 'error) States.Field.t StringMap.t -> unit;
    setValues : 'values -> unit;
    updateField :
      StringMap.key ->
      (('values, 'error) States.Field.t -> ('values, 'error) States.Field.t) ->
      unit;
  }

  type ('values, 'error) t =
    ('values, 'error) state * ('values, 'error) modifiers
end

module Hook = struct
  type ('values, 'error) t = {
    reset : unit -> unit;
    setValues : ('values -> 'values) -> unit;
    state : ('values, 'error) Context.state;
    submit : unit -> unit;
  }
end

module type Form = sig
  type values

  type error

  module Consumer : sig
    type children = (values, error) Context.state -> React.element

    external makeProps :
      children:children -> ?key:string -> unit -> < children : children > Js.t
      = ""
      [@@bs.obj]

    val make : < children : children > Js.t React.component
  end

  module Provider : sig
    external makeProps :
      children:React.element ->
      ?key:string ->
      unit ->
      < children : React.element > Js.t = ""
      [@@bs.obj]

    val make : < children : React.element > Js.t React.component
  end

  module Field : sig
    type 'value children =
      ('value, error) Props.Field.childrenProps -> React.element

    external makeProps :
      name:string ->
      lens:(values, 'value) Optic.Lens.t ->
      children:'value children ->
      ?label:string ->
      ?errorLabel:string ->
      ?onBlur:(unit -> unit) ->
      ?onChange:('value -> unit) ->
      ?onFocus:(unit -> unit) ->
      ?validations:(values, 'value, error) Validations.t list ->
      ?disable:bool ->
      ?key:string ->
      unit ->
      < name : string
      ; lens : (values, 'value) Optic.Lens.t
      ; children : 'value children
      ; label : string option
      ; errorLabel : string option
      ; onBlur : (unit -> unit) option
      ; onChange : ('value -> unit) option
      ; onFocus : (unit -> unit) option
      ; validations : (values, 'value, error) Validations.t list option
      ; disable : bool option >
      Js.t = ""
      [@@bs.obj]

    val make :
      < name : string
      ; lens : (values, 'value) Optic.Lens.t
      ; children : 'value children
      ; label : string option
      ; errorLabel : string option
      ; onBlur : (unit -> unit) option
      ; onChange : ('value -> unit) option
      ; onFocus : (unit -> unit) option
      ; validations : (values, 'value, error) Validations.t list option
      ; disable : bool option >
      Js.t
      React.component
  end

  val use : unit -> (values, error) Hook.t

  external makeProps :
    ?preventDefault:bool ->
    ?stopPropagation:bool ->
    ?className:string ->
    ?disable:bool ->
    children:React.element ->
    ?key:string ->
    unit ->
    < preventDefault : bool option
    ; stopPropagation : bool option
    ; className : string option
    ; disable : bool option
    ; children : React.element >
    Js.t = ""
    [@@bs.obj]

  val make :
    < preventDefault : bool option
    ; stopPropagation : bool option
    ; className : string option
    ; disable : bool option
    ; children : React.element >
    Js.t
    React.component
end

module Make
    (Values : Values)
    (Error : Type)
    (Handlers : Handlers with type values = Values.t and type error = Error.t) :
  Form with type values = Values.t and type error = Error.t = struct
  type error = Error.t

  type values = Values.t

  let context =
    React.createContext
      ( {
          Context.fields = StringMap.make ();
          isDisabled = false;
          values = Values.init;
        },
        {
          Context.removeField = (fun _ -> ());
          setIsDisabled = (fun _ -> ());
          setField = (fun _ _ -> ());
          setFields = (fun _ -> ());
          setValues = (fun _ -> ());
          updateField = (fun _ _ -> ());
        } )

  module Consumer = struct
    type children = (values, error) Context.state -> React.element

    external makeProps :
      children:children -> ?key:string -> unit -> < children : children > Js.t
      = ""
      [@@bs.obj]

    let make props =
      let state, _ = React.useContext context in
      let children : children = props##children in

      children state
  end

  module Provider = struct
    external makeProps :
      children:React.element ->
      ?key:string ->
      unit ->
      < children : React.element > Js.t = ""
      [@@bs.obj]

    let make props =
      let provider = React.Context.provider context in
      let state, _ = React.useContext context in
      let state, setState = React.useState (fun () -> state) in
      let modifiers =
        {
          Context.removeField =
            (fun name ->
              setState (fun state ->
                  { state with fields = StringMap.remove name state.fields }));
          setIsDisabled =
            (fun isDisabled ->
              setState (fun state -> { state with isDisabled }));
          setField =
            (fun name field ->
              setState (fun state ->
                  { state with fields = StringMap.set name field state.fields }));
          setFields =
            (fun fields -> setState (fun state -> { state with fields }));
          setValues =
            (fun values -> setState (fun state -> { state with values }));
          updateField =
            (fun name f ->
              setState (fun state ->
                  {
                    state with
                    fields = StringMap.update name (Option.map f) state.fields;
                  }));
        }
      in

      React.createElement provider
        [%bs.obj { children = props##children; value = (state, modifiers) }]
  end

  module Field = struct
    type 'value children =
      ('value, error) Props.Field.childrenProps -> React.element

    external makeProps :
      name:string ->
      lens:(values, 'value) Optic.Lens.t ->
      children:'value children ->
      ?label:string ->
      ?errorLabel:string ->
      ?onBlur:(unit -> unit) ->
      ?onChange:('value -> unit) ->
      ?onFocus:(unit -> unit) ->
      ?validations:(values, 'value, error) Validations.t list ->
      ?disable:bool ->
      ?key:string ->
      unit ->
      < name : string
      ; lens : (values, 'value) Optic.Lens.t
      ; children : 'value children
      ; label : string option
      ; errorLabel : string option
      ; onBlur : (unit -> unit) option
      ; onChange : ('value -> unit) option
      ; onFocus : (unit -> unit) option
      ; validations : (values, 'value, error) Validations.t list option
      ; disable : bool option >
      Js.t = ""
      [@@bs.obj]

    let make props =
      let { Context.isDisabled; fields; values }, modifiers =
        React.useContext context
      in
      let field = StringMap.get props##name fields in
      let children = props##children in
      let value = Optic.Lens.view props##lens values in
      let isFocused, setIsFocused = React.useState (fun () -> false) in
      let validationNames =
        Option.fold [] (List.map Validations.getName) props##validations
      in

      let validate =
        React.useCallback1
          (fun validationContext values ->
            props##validations
            |> Option.fold `valid
                 (List.mapOption (fun (strategy, (_, validation)) ->
                      if
                        Validations.shouldValidate ~context:validationContext
                          ~strategy
                      then
                        match
                          validation
                            {
                              Validations.Validator.Args.label =
                                Option.alt props##errorLabel props##label;
                              lens = props##lens;
                              name = props##name;
                              value = Optic.Lens.view props##lens values;
                              values;
                            }
                        with
                        | `ok _ -> None
                        | `error error -> Some error
                      else None)
                  >> function
                  | [] -> `valid
                  | errors -> `errors errors))
          (Array.fromList validationNames)
      in

      let validateAndUpdate validationContext values =
        modifiers.updateField props##name (fun field ->
            { field with status = validate validationContext values })
      in

      let onBlur () =
        let onBlur' = props##onBlur in

        setIsFocused (const false);

        Option.forEach (( |> ) ()) onBlur';

        validateAndUpdate (Some `onBlur) values
      in

      let onFocus () =
        let onFocus' = props##onFocus in

        setIsFocused (const true);

        Option.forEach (( |> ) ()) onFocus';

        if Option.fold false States.Field.isPristine field then
          modifiers.updateField props##name (fun field ->
              { field with status = `touched })
      in

      let onChange value =
        let onChange' = props##onChange in

        let values = Optic.Lens.set props##lens value values in

        Option.forEach (( |> ) (Optic.Lens.view props##lens values)) onChange';

        modifiers.setValues values;
        validateAndUpdate (Some `onChange) values
      in

      let setStatus status =
        modifiers.updateField props##name (fun field -> { field with status })
      in

      React.useEffect0 (fun () ->
          if Option.isNone field then
            modifiers.setField props##name
              { States.Field.status = `pristine; validate };
          None);

      children
        {
          Props.Field.isDisabled =
            isDisabled || Option.getOrElse false props##disable;
          isFocused;
          label = props##label;
          name = props##name;
          onBlur;
          onChange;
          onFocus;
          setStatus;
          status = Option.fold `pristine States.Field.status field;
          validate = (fun () -> validateAndUpdate None values);
          validationNames;
          value;
        }
  end

  let use () =
    let ({ Context.fields; values } as state), { Context.setFields; setValues }
        =
      React.useContext context
    in

    let reset () =
      setFields (States.reset fields);
      setValues Values.init
    in

    let setValues f = setValues (f values) in

    let submit () =
      let fields =
        StringMap.map
          (fun ({ States.Field.validate } as field) ->
            { field with status = validate None values })
          fields
      in

      ( match (Handlers.onSubmitError, States.getErrors fields) with
      | _, [] -> Handlers.onSubmit values
      | Some onSubmitError, errors -> onSubmitError values errors
      | None, _ -> () );

      setFields fields
    in

    { Hook.reset; setValues; state; submit }

  external makeProps :
    ?preventDefault:bool ->
    ?stopPropagation:bool ->
    ?className:string ->
    ?disable:bool ->
    children:React.element ->
    ?key:string ->
    unit ->
    < preventDefault : bool option
    ; stopPropagation : bool option
    ; className : string option
    ; disable : bool option
    ; children : React.element >
    Js.t = ""
    [@@bs.obj]

  let make props =
    let _, { Context.setIsDisabled } = React.useContext context in
    let disable = Option.getOrElse false props##disable in
    let { Hook.submit } = use () in

    React.useEffect1
      (fun () ->
        setIsDisabled disable;
        None)
      [| disable |];

    ReactDOMRe.createElement "form"
      ~props:
        (ReactDOMRe.props ?className:props##className
           ~onSubmit:
             (Events.handle' ?preventDefault:props##preventDefault
                ?stopPropagation:props##stopPropagation submit)
           ())
      [| props##children |]
end

let make (type values error) (module Values : Values with type t = values)
    (module Error : Type with type t = error) ~onSubmit ?onSubmitError () :
    (module Form with type values = Values.t and type error = Error.t) =
  let module Handlers = struct
    type error = Error.t

    type values = Values.t

    let onSubmit = onSubmit

    let onSubmitError = onSubmitError
  end in
  (module Make (Values) (Error) (Handlers))
