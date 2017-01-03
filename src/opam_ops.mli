(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_ci.Term


val packages_from_diff : Docker_ops.t -> Datakit_ci.Target.t -> string list t

val distro_build :
  ?extra_remotes:(Datakit_github.Repo.t * string) list ->
  ?packages:string list ->
  ?target:Datakit_ci.Target.t ->
  opam_repo:Datakit_github.Repo.t * string ->
  distro:string ->
  ocaml_version:string ->
  typ:[ `Package | `Repo ] ->
  opam_t:Opam_build.t -> docker_t:Docker_ops.t -> unit -> Docker_build.image t

val run_phases :
  packages:string list Datakit_ci.Term.t ->
  build:(target:Datakit_ci.Target.t ->
    packages:string list ->
    distro:string ->
    ocaml_version:string ->
    unit -> Docker_build.image Datakit_ci.Term.t) ->
  build_revdeps:(Docker_ops.t ->
    string list ->
    Docker_build.image -> 'a Datakit_ci.Term.t) ->
  Docker_ops.t ->
  Datakit_ci.Target.t -> (string * string Datakit_ci.Term.t) list

module type V = sig
  open Datakit_github
  val build_archive : ?volume:Fpath.t -> Docker_build.t -> Docker_run.t -> string -> string t
  val run_package : ?volume:Fpath.t -> Docker_run.t -> Docker_build.image -> string -> string t * string
  val run_packages : ?volume:Fpath.t -> Docker_run.t -> Docker_build.image -> string list -> (string * string Datakit_ci.Term.t * string) list t
  val list_all_packages : Docker_run.t -> Docker_build.image -> string list t
  val list_revdeps : Docker_ops.t -> Docker_build.image -> string -> string list t
  val run_revdeps: ?volume:Fpath.t -> Docker_ops.t -> string list -> Docker_build.image -> unit t
end

module V1 : V
module V2 : V

(* TODO dead code*
val build_package : Docker_ops.t -> Docker_build.image -> string -> Docker_build.image t

val build_packages : Docker_ops.t -> Docker_build.image -> string list -> unit t

val build_revdeps : Docker_ops.t -> string list -> Docker_build.image -> unit t
*)

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

