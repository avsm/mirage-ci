type t
val sexp_of_t : t -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t
val v : ?patch:int -> ?extra:string -> int -> int -> t
val to_string : t -> string
val of_string : string -> t
val compare : t -> t -> int
val t : t
val pp : Format.formatter -> t -> unit
module Since : sig
  val bytes: t
end
module Has : sig
  val bytes : t -> bool
end
