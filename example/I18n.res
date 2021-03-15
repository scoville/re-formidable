module Error = {
  type t = [#error(string, option<string>)]

  let eq = (error1, error2) => error1 == error2
}

let translate = (#error(name, label)) => `${label->Belt.Option.getWithDefault("")}/${name}`
