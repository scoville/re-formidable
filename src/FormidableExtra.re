module ListExtra = {
  let flatMap = (xs, f) =>
    xs->List.reduce([], (acc, x) => acc->List.concat(f(x)));

  let rec includes = (xs, y) =>
    switch (xs) {
    | [] => false
    | [x, ...xs] => x === y ? true : includes(xs, y)
    };
};

module OptionExtra = {
  let alt = (option1, option2) =>
    switch (option1, option2) {
    | (Some(_), _) => option1
    | _ => option2
    };
};
