type version = string

let latest = "4.07"

let primary ~opam_version = match opam_version with
| `V1 -> "4.05.0"
| `V2 -> latest

let recents ~opam_version = match opam_version with
| `V1 -> [
    "4.03.0";
    "4.04.2";
    "4.05.0";
    "4.06.0";
  ]
| `V2 -> [
    "4.03";
    "4.04";
    "4.05";
    "4.06";
    "4.07";
  ]

let to_string v = v

let docker ~opam_version v = match opam_version with
| `V1 -> "_ocaml-"^v
| `V2 when v == latest -> ""
| `V2 -> "-ocaml-"^v
