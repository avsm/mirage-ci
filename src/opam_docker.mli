(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_github

module Remote : sig
  type t = {
    repo: Repo.t;
    commit: Commit.t;
    full_remote: bool;
  }
  val pp_for_compare : t Fmt.t
end

val repo : user:string -> repo:string -> branch:string -> Repo.t * string
val ocaml_opam_repository : Repo.t * string
val mirage_opam_repository : Repo.t * string
val js_opam_repository : Repo.t * string

module type V = sig
  val add_cache_dir : Dockerfile.t
  val add_remotes : Remote.t list -> Dockerfile.t
  val set_opam_repo_rev : ?remote:Remote.t -> ?branch:string -> ?dst_branch:string -> string -> Dockerfile.t
  val base : ocaml_version:string -> distro:string -> Dockerfile.t
  val clone_src : user:string -> repo:string -> branch:string -> commit:string -> Dockerfile.t
  val merge_src : user:string -> repo:string -> branch:string -> commit:string -> Dockerfile.t
  val add_local_pins : string list -> Dockerfile.t
  val switch_local_remote : Dockerfile.t
  val add_local_remote : Dockerfile.t
  val add_ci_script : Dockerfile.t
  val add_archive_script : Dockerfile.t
end

module V1 : V
module V2 : V

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

