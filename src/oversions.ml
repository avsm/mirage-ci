type version = (string * string)

let primary = ("4.05.0", "4.05")
let recents = [("4.03.0","4.03");("4.04.2","4.04");primary;("4.06.0","4.06")]

let to_string (_, v) = v

let docker_opam1 (v, _) =
  "ocaml-"^v

let docker_opam2 v =
  if v == primary then
    "ocaml"
  else
    "ocaml-"^snd v

let to_string_with_minor (v, _) = v
