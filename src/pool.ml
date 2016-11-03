(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open! Astring
open Lwt.Infix

type t = {
  free_workers : Worker.t Lwt_queue.t;
  mutable busy_workers : Worker.t String.Map.t;
}

let create () =
  {
    free_workers = Lwt_queue.create ();
    busy_workers = String.Map.empty;
  }

let list_of_queue q = Queue.fold (fun acc i -> i :: acc) [] q

let free_workers t = list_of_queue t.free_workers.Lwt_queue.items
let busy_workers t = t.busy_workers

let add_worker t w =
  Lwt_queue.add t.free_workers w

let get_worker t =
  Lwt_queue.next t.free_workers >|= fun w ->
  t.busy_workers <- String.Map.add w.Worker.id w t.busy_workers;
  w

let release_worker t w =
  assert (String.Map.mem w.Worker.id t.busy_workers);
  t.busy_workers <- String.Map.remove w.Worker.id t.busy_workers;
  Lwt_queue.add t.free_workers w

let with_worker t fn =
  get_worker t >>= fun worker ->
  Lwt.finalize
    (fun () -> fn worker)
    (fun () -> release_worker t worker; Lwt.return ())

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

