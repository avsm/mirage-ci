(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** DataKitCI module to run a Docker image and capture output *)

open DataKitCI

type t
(** [t] is the state of a [Docker_run] instance *)

val config : logs:Live_log.manager -> label:string -> pool:Monitored_pool.t -> timeout:float -> t
(** [config ~logs ~label ~pool ~timeout] will configure a Docker runner to execute
    images. [pool] controls the level of parallel builds allowed, and [timeout]
    is the length in seconds that a run can execute for before being terminated. *)

val run : tag:string -> cmd:string list -> t -> string Term.t
(** [run t d] will run the [cmd] inside [tag] image using the [t] builder, and
    return the string output of the command. *)

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
