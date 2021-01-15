// Simulates a fetch hook

type t<'data, 'error> = Init | Loading | Data('data) | Error('error)

let useFetch = (~path) => {
  let (response, setResponse) = React.useState(() => Init)

  (
    response,
    (~body) => {
      setResponse(_ => Loading)

      let data = switch path->Js.String2.split("/")->Js.Array2.filter(pathPart => pathPart != "") {
      | ["email", "exists"] when body->Js.Dict.get("email") == Some("already@exists.com") =>
        Error(#alreadyExists)
      | ["email", "exists"] => Data("Ok")
      | _ => Error(#notFound)
      }

      Js.Global.setTimeout(() => setResponse(_ => data), 1000)->ignore
    },
  )
}
