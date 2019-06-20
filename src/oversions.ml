type version = Ocaml_version.t

let primary = Ocaml_version.Releases.latest
let recents = Ocaml_version.Releases.recent

let to_string v =
  Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor v)
