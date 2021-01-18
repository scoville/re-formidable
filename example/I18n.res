module Error = {
  type t = [#error(string, option<string>)]
}

let translate = (#error(name, label)) => `${label->Option.getWithDefault("")}/${name}`
