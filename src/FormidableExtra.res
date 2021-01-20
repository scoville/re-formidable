module ArrayExtra = {
  let flatMap = (xs, f) => xs->Js.Array2.reduce((acc, x) => acc->Js.Array2.concat(f(x)), [])
}

module OptionExtra = {
  let or = (option1, option2) =>
    switch (option1, option2) {
    | (Some(_), _) => option1
    | _ => option2
    }
}
