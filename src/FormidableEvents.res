%%private(
  let handle_ = (~preventDefault, ~stopPropagation, event) => {
    if preventDefault {
      ReactEvent.Synthetic.preventDefault(event)
    }

    if stopPropagation {
      ReactEvent.Synthetic.stopPropagation(event)
    }
  }
)

let eventTargetValue = event =>
  event
  ->ReactEvent.Form.target
  ->(target => target["value"])
  ->Js.Nullable.toOption
  ->Belt.Option.getWithDefault("")

let handleWithValue = (~preventDefault=false, ~stopPropagation=false, f, event) => {
  event->handle_(~preventDefault, ~stopPropagation)
  event->eventTargetValue->f
}

let handleAndIgnore = (~preventDefault=false, ~stopPropagation=false, f, event) => {
  handle_(~preventDefault, ~stopPropagation, event)
  f()
}

let handle = (~preventDefault=false, ~stopPropagation=false, f, event) => {
  handle_(~preventDefault, ~stopPropagation, event)
  f(event)
}
