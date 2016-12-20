(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github
open Term.Infix

let build_package t image pkg =
  let open !Dockerfile in
  let dfile =
    from image.Docker_build.sha256 @@
    run "opam depext -y %s" pkg @@
    run "opam install -j 2 -yv %s" pkg in
  let hum = Fmt.strf "opam install %s" pkg in
  Docker_build.run t ~hum dfile

let build_packages t image pkgs =
  List.map (fun pkg ->
    let t = build_package t image pkg in
    pkg, t
  ) pkgs |>
  Term.wait_for_all |>
  Term_utils.ignore_failure ~on_fail:(fun _ -> ())

let list_all_packages t image =
  let cmd = ["opam";"list";"-a";"-s"] in
  Docker_run.run ~tag:image.Docker_build.sha256 ~cmd t >|=
  String.cuts ~empty:false ~sep:"\n"

let list_revdeps t image pkg =
  let cmd = ["opam";"list";"-s";"--depends-on";pkg] in
  Docker_run.run ~tag:image.Docker_build.sha256 ~cmd t >|=
  String.cuts ~empty:false ~sep:"\n"

let build_revdeps t run_t packages image =
  List.map (fun pkg ->
    let terms =
      list_revdeps run_t image pkg >>=
      build_packages t image in
    let l = Fmt.strf "revdeps:%s" pkg in
    l, terms) packages |>
  Term.wait_for_all

module V1 = struct
  open !Dockerfile

  let add_remotes rs =
    let remotes_ref = ref 0 in
    List.map (fun (repo, commit) ->
     incr remotes_ref;
     run "opam remote add e%d https://github.com/%s.git#%s"
       !remotes_ref (Fmt.strf "%a" Repo.pp repo) (Commit.hash commit)
    ) rs |> fun remotes ->
    empty @@@ remotes

  let add_pins packages =
    List.map (run "opam pin add -n %s /home/opam/src") packages |> fun pins ->
    empty @@@ pins
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

