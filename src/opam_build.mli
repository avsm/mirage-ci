(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Datakit_ci module to generate a Docker from an OPAM PR *)
open Datakit_ci

type t
(* [t] is the state of an Opam_build instance *)

val v : logs:Live_log.manager -> label:string -> t
(** [v ~logs ~label t] will configure an [Opam_build] instance to generate
  Dockerfiles from {!key} parameters. *)

val run : ?packages:string list -> ?target:Target.v -> distro:string ->
  ocaml_version:string -> remotes:Opam_docker.Remote.t list -> typ:[`Package|`Repo|`Full_repo]
  -> opam_version:[`V1|`V2] -> t -> Dockerfile.t Term.t
(** [run t key] will result in a Datakit_ci {!Term.t} that will generate
  a {!Dockerfile.t} that can be built using a {!Docker_build.t} instance
  into a concrete image. *)

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
