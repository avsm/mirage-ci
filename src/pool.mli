(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Resource pools of worker instances for DataKitCI jobs *)

open Astring

type t
(** [t] is the state of the worker pool *)

val create : unit -> t
(** [create  ()] will costruct a new worker pool that starts empty. *)

val add_worker : t -> Worker.t -> unit
(** [add_worker t worker] add new [worker] to the free pool. *)

val get_worker : t -> Worker.t Lwt.t
(** [get_worker t] waits for a worker to become free and returns it.
    The worker is marked as busy and must be later freed with [release_worker]. *)

val release_worker : t -> Worker.t -> unit
(** [release_worker t worker] returns [worker] to the free pool. *)

val free_workers : t -> Worker.t list
(** [free_workers t] is a snapshot of the list of free workers. *)

val busy_workers : t -> Worker.t String.Map.t
(** [busy_workers t] is a snapshot of the map of busy workers, indexed by worker ID. *)

val with_worker : t -> (Worker.t -> 'a Lwt.t) -> 'a Lwt.t
(** [with_worker fn] is [fn worker], where [worker] is the next free worker.
    The worker is returned to the free pool when [fn] finishes. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy
   Copyright (c) 2016 Thomas Leonard

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

