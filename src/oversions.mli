type version

val primary : version
val recents : version list

val older_than_4_06 : version -> bool

val to_string : version -> string
