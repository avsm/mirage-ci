type version = (string option * string)

let primary = (Some "4.05.0", "4.05")
let recents = [
  (Some "4.03.0", "4.03");
  (Some "4.04.2", "4.04");
  (Some "4.05.0", "4.05");
  (Some "4.06.0", "4.06");
  (None,          "4.07");
]

let to_string (_, v) = v

let docker_opam1 = function
| (Some v, _) -> "_ocaml-"^v
| (None, _) -> assert false

let docker_opam2 v =
  if v == primary then
    ""
  else
    "-ocaml-"^snd v

let exists ~opam_version v = match opam_version, v with
| `V2, _ -> true
| `V1, (Some _, _) -> true
| `V1, (None, _) -> false
