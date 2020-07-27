open Relude.Globals

let%private handle_ ~preventDefault ~stopPropagation event =
  if preventDefault then ReactEvent.Synthetic.preventDefault event;
  if stopPropagation then ReactEvent.Synthetic.stopPropagation event

let eventTargetValue =
  ReactEvent.Form.target
  >> (fun target -> target##value)
  >> Js.Nullable.toOption >> Option.getOrElse ""

let handle ?(preventDefault = false) ?(stopPropagation = false) f event =
  handle_ ~preventDefault ~stopPropagation event;
  f @@ eventTargetValue event

let handle' ?(preventDefault = false) ?(stopPropagation = false) f event =
  handle_ ~preventDefault ~stopPropagation event;
  f ()
