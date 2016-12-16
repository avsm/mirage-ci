(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Term.Infix
open Term_utils

(* revdep handling *) 
let build_revdep t image pkg =
  let open !Dockerfile in
  let dfile =
    from image.Docker_build.sha256 @@
    run "opam depext -uiyv -j 2 %s" pkg in
  let hum = Fmt.strf "opam install %s" pkg in
  Docker_build.run t ~hum dfile

let build_revdeps t image pkgs =
  String.cuts ~empty:false ~sep:"\n" pkgs |>
    List.map (fun pkg ->
      let t = build_revdep t image pkg in
      pkg, t
    ) |>
  Term.wait_for_all |>
  ignore_failure ~on_fail:(fun _ -> ())

let calculate_revdeps t image pkg =
  let cmd = ["opam";"list";"-s";"--depends-on";pkg] in
  Docker_run.run ~tag:image.Docker_build.sha256 ~cmd t

let revdeps t run_t packages image =
  List.map (fun pkg ->
    let terms =
      calculate_revdeps run_t image pkg >>=
      build_revdeps t image in
    let l = Fmt.strf "revdeps:%s" pkg in
    l, terms) packages |>
  Term.wait_for_all

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

