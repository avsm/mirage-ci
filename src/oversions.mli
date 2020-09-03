type version

val primary : version
val recents : version list

val has_native_dune_support : version -> bool

val to_string : version -> string
