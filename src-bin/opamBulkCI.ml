(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Datakit_github
module DO = Docker_ops

module Builder = struct

  open Term.Infix

  let label = "opamRepo"
  let docker_t = DO.v ~logs ~label ~jobs:24 ()
  let opam_t = Opam_build.v ~logs ~label
  let opam_bulk_t = Opam_bulk_build.v ~label ~logs
  let opam_bulk_diff_t = Opam_bulk_build_diff.v ~label ~logs
  let volume_v1 = Fpath.v "opam-archive"
  let volume_v2 = Fpath.v "opam2-archive"

  let run_bulk typ target =
    match Target.id target with
    |`Ref ["heads";"bulk"] ->
       let distro = "ubuntu-16.04" in
       let ocaml_version_402 = "4.02.3" in
       let ocaml_version_403 = "4.03.0" in
       let ocaml_version_404 = "4.04.0" in
       let ocaml_version_405 = "4.05.0" in
       let b402_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[] ~ocaml_version:ocaml_version_402 ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
       let b403_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[] ~ocaml_version:ocaml_version_403 ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
       let b404_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[] ~ocaml_version:ocaml_version_404 ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
       let b405_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[] ~ocaml_version:ocaml_version_405 ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
       let v402 = b402_t >>= Opam_bulk_build.run opam_bulk_t in
       let v403  = b403_t  >>= Opam_bulk_build.run opam_bulk_t in
       let v404 = b404_t >>= Opam_bulk_build.run opam_bulk_t in
       let v405 = b405_t >>= Opam_bulk_build.run opam_bulk_t in
       let diff_ocaml_402_403 =
         Term.without_logs b402_t >>= fun v402 ->
         Term.without_logs b403_t >>= fun v403 ->
         Opam_bulk_build_diff.run_ocaml_version_diff (ocaml_version_402, ocaml_version_403) ~distro v402 v403 opam_bulk_diff_t
       in
       let diff_ocaml_403_404 =
         Term.without_logs b403_t >>= fun v403 ->
         Term.without_logs b404_t >>= fun v404 ->
         Opam_bulk_build_diff.run_ocaml_version_diff (ocaml_version_403, ocaml_version_404) ~distro v403 v404 opam_bulk_diff_t
       in
       let diff_ocaml_404_405 =
         Term.without_logs b404_t >>= fun v404 ->
         Term.without_logs b405_t >>= fun v405 ->
         Opam_bulk_build_diff.run_ocaml_version_diff (ocaml_version_404, ocaml_version_405) ~distro v404 v405 opam_bulk_diff_t
       in
       ["V2 Bulk 4.02", v402; "V2 Bulk 4.03", v403; "V2 Bulk 4.04", v404; "V2 Bulk 4.05", v405; "Results (4.02->4.03)", diff_ocaml_402_403; "Results (4.03->4.04)", diff_ocaml_403_404; "Results (4.04->4.05)", diff_ocaml_404_405]
    |_ -> []
 
  let tests = [
    Config.project ~id:"mirage/opam-repository" (run_bulk `Full_repo);
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"opam-repo-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(everyone)
    ~state_repo:(Uri.of_string "https://github.com/ocaml/ocaml-ci.logs")
    ()

let () =
  run (Cmdliner.Term.pure (Config.v ~web_config ~projects:Builder.tests))

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

