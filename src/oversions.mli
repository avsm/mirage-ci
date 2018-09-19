type version

val primary : opam_version:[`V1 | `V2] -> version
val recents : opam_version:[`V1 | `V2] -> version list

val to_string : version -> string
val docker : opam_version:[`V1 | `V2] -> version -> string
