(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Term.Infix

let rec term_map_s fn l =
  match l with
  | [] -> Term.return []
  | x :: l ->
      fn x >>= fun x ->
      term_map_s fn l >|= fun l ->
      x :: l

let after t =
    Term.wait_for ~while_pending:"waiting" ~on_failure:"depfail" t |>
    Term.without_logs

let ignore_failure ~on_fail t =
  Term.state t >>= function
  | Error (`Pending m) -> Term.pending "%s" m
  | Error (`Failure m) -> Term.return (on_fail m)
  | Ok m -> Term.return m

let report ~order ~label t =
  let l = Fmt.strf "%d %s" order label in
  let t = t >>= fun _ -> Term.return label in
  l, t

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
