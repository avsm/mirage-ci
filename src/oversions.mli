type version

val primary : version
val recents : version list

val to_string : version -> string
val docker_opam1 : version -> string
val docker_opam2 : version -> string

val exists : opam_version:[`V1 | `V2] -> version -> bool
