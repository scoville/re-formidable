// Simulates a fetch hook

type t<'data, 'error> = Init | Loading | Data('data) | Error('error)

let useFetch = (~path) => {
  let (response, setResponse) = React.useState(() => Init)

  // Prevents memory leaks
  let (timeoutId, setTimeoutId) = React.useState(() => None)

  React.useEffect1(() =>
    switch timeoutId {
    | None => None
    | Some(timeoutId) => Some(() => Js.Global.clearTimeout(timeoutId))
    }
  , [timeoutId])

  (
    response,
    (~body) => {
      setResponse(_ => Loading)

      let data = switch path->Js.String2.split("/")->Js.Array2.filter(pathPart => pathPart != "") {
      | ["email", "exists"] if body->Js.Dict.get("email") == Some("already@exists.com") =>
        Error(#alreadyExists)
      | ["email", "exists"] => Data("Ok")
      | _ => Error(#notFound)
      }

      setTimeoutId(_ => Some(Js.Global.setTimeout(() => setResponse(_ => data), 200)))
    },
  )
}
