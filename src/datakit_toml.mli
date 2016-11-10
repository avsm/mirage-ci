(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Parse OPAM configuration keys from a TOML spec *)

(** {1 Type definitions and pretty printers} *)

type t = {
  ref: string list;     (** Tag or branch (e.g. [["heads";"master"]] or [["tags";"v2.0.0"]] *)
  ocamlv: string list;  (** List of OCaml compiler versions to be tested *)
  flambda: bool;        (** Whether flambda should be tried for compilers that support it. *)
  distros: string list; (** List of distributions to compile on. *)
}
(** Record describing an individual test *)
type l = t list

val sexp_of_t : t -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_l : l -> Sexplib.Sexp.t
val l_of_sexp : Sexplib.Sexp.t -> l

val pp : t Fmt.t
(** [pp t] is a human-readable pretty printer for {!t} *)

val compare : t -> t -> int
(** [compare t t] will do a structural stable comparison of {!t} values. *)

val equal : t -> t -> bool
(** [equal t t] will do a structural equality test of {!t} values. *)

(** {1 Parser and conversion functions} *)

val of_string_exn : string -> l
(** [of_string_exn behaves as {!of_string} except it raises an exception on error.
    @raises Failure with the error message on failure. *)

val of_string : string -> (l, [> Rresult.R.msg ]) Result.result
(** [of_string s] will parse the TOML string and return the tests found within as a list. *)

(** {1 Accessor and search functions} *)

val assoc : string list -> l -> t option
(** [assoc ref l] will search the list of {!t} values and return the
  first one that matches [ref]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
