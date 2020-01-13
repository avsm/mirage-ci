type version = Ocaml_version.t

let primary = Ocaml_version.Releases.latest
let recents = Ocaml_version.Releases.recent

let older_than_4_07 v =
  Ocaml_version.compare v Ocaml_version.Releases.v4_07_0 < 0

let to_string v =
  Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor v)
