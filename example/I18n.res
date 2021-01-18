module Error = Id.MakeComparable({
  type t = [#error(string, option<string>)]

  let cmp = Pervasives.compare
})

let translate = (#error(name, label)) => `${label->Option.getWithDefault("")}/${name}`
