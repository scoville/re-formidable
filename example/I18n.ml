open Relude.Globals

module Error = struct
  type t = [ `error of string * string option ]
end

let translate (`error (name, label)) = Option.getOrElse "" label ^ "/" ^ name
