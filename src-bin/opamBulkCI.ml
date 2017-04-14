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
       let ocaml_version = "4.04.0" in
       let main_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[] ~ocaml_version ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
(*
       let mirage_t = 
         let opam_repo = Opam_docker.repo ~user:"mirage" ~repo:"opam-repository" ~branch:"bulk" in
         let mirage_dev_repo = Opam_docker.repo ~user:"mirage" ~repo:"mirage-dev" ~branch:"master" in
         Opam_ops.bulk_build ~volume:volume_v2 ~remotes:[mirage_dev_repo] ~ocaml_version ~distro ~opam_version:`V2 ~opam_repo opam_t docker_t target in
       let diff =
         Term.without_logs main_t >>= fun main ->
         Term.without_logs mirage_t >>= fun mirage ->
         Opam_bulk_build_diff.run ~ocaml_version ~distro main mirage opam_bulk_diff_t
       in
*)
       let main = main_t >>= Opam_bulk_build.run opam_bulk_t in
(*
       let mirage = mirage_t >>= Opam_bulk_build.run opam_bulk_t in
       ["V2 Bulk", main; "V2 Bulk-Mirage-Dev", mirage; "Results", diff]
*)
       ["V2 Bulk", main ]
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

