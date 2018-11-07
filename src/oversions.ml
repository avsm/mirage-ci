type version = string

let latest = "4.07"
let primary = latest
let recents = [
  "4.03";
  "4.04";
  "4.05";
  "4.06";
  "4.07";
]

let to_string v = v

let docker v =
  if v == latest
  then ""
  else "-ocaml-"^v
