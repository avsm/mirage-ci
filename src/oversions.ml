type version = string

let default = "default" (* default OCaml version in the default docker images *)
let primary = default
let recents = [
  "4.03";
  "4.04";
  "4.05";
  "4.06";
  "4.07";
  "4.08";
]

let to_string v = v

let docker v =
  if v == default
  then ""
  else "-ocaml-"^v
