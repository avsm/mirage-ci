(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** DataKitCI module to build a Dockerfile image *)

open Datakit_ci

type t
(** [t] is the state of a [Docker_build] instance *)

type image = {
  tag: string;     (* textual tag tied to image *)
  sha256: string;  (* SHA256 tag that uniquely identifies this image *)
  hum: string;     (* human-readable description of Dockerfile *)
}
(** [image] has the metadata for a locally built Docker image *)

val config : logs:Live_log.manager -> label:string -> pool:Monitored_pool.t -> timeout:float -> t
(** [config ~logs ~label ~pool ~timeout] will configure a Docker builder to build
    images that are tagged with [label:digest] of the build, where [digest]
    is calculated using {!digest_of_dockerfile}.  [pool] controls the level
    of parallel builds allowed, and [timeout] is the length in seconds that
    a build can run for before being terminated. *)

val run : t -> hum:string -> Dockerfile.t -> image Term.t
(** [run t ~hum d] will build the [d] Dockerfile using the [t] builder, and return
    the SHA256 build hash of the resulting image.  The image will also be
    tagged as [label:digest] where [label] is configured as part of [t] and
    [digest] is calculated via {!digest_of_dockerfile}. [hum] is a human-readable
    description of the Dockerfile for showing in the UI or status logs. *)

val digest_of_dockerfile : Dockerfile.t -> string
(** [digest_of_dockerfile d] will calculate a hex digest of the input Dockerfile. *)

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
