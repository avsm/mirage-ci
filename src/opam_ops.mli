(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_ci.Term


val packages_from_diff : ?default:string list -> Docker_ops.t -> Datakit_ci.Target.t -> string list t

val run_phases :
  ?volume: Fpath.t ->
  ?build_filter:bool Datakit_ci.Term.t ->
  revdeps: bool ->
  packages:string list Datakit_ci.Term.t ->
  remotes:(Datakit_github.Repo.t * string) list ->
  typ:[ `Package | `Repo | `Full_repo ] ->
  opam_repo:Datakit_github.Repo.t * string ->
  Opam_build.t ->
  Docker_ops.t ->
  Datakit_ci.Target.t -> (string * string Datakit_ci.Term.t) list

module Cmds : sig
  val build_archive : ?volume:Fpath.t -> Docker_ops.t -> string -> (Docker_build.image * string) t
  val run_package : ?volume:Fpath.t -> ?with_tests:bool -> Docker_run.t -> Docker_build.image -> string -> string t * string
  val run_packages : ?volume:Fpath.t -> ?with_tests:bool -> Docker_run.t -> Docker_build.image -> string list -> (string * string Datakit_ci.Term.t * string) list t
  val list_all_packages : Docker_ops.t -> Docker_build.image -> string list t
  val list_revdeps : Docker_ops.t -> Docker_build.image -> string -> string list t
  val run_revdeps: ?volume:Fpath.t -> Docker_ops.t -> string -> Docker_build.image -> unit t
end

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
