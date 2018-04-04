(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Datakit_github

module Builder = struct

  open Term.Infix

  let one_hour = 60. *. 60.
  let eight_hours = one_hour *. 8.
  let opam_repo = Repo.v ~user:"ocaml" ~repo:"opam-repository"

  let pool = Monitored_pool.create "docker" 24
  let label = "ocaml"
  let docker_t = Docker_build.v ~logs ~label ~pool ~timeout:eight_hours ()
  let docker_run_t = Docker_run.v ~logs ~label ~pool ~timeout:eight_hours ()
  let opam_t = Opam_build.v ~logs ~label ~version:`V2
  let opam_bulk_t = Opam_bulk_build.v ~label ~logs

  let opam_build_all target =
    let base_dfile ~distro ~ocaml_version ~git_rev =
      let open Dockerfile in
      from ~tag:(distro^"_ocaml-"^ocaml_version) "ocaml/opam-dev" @@
      Opam_docker.V2.set_opam_repo_rev git_rev @@
      run "opam depext -uivy ocamlfind ocamlbuild camlp4" @@
      run "echo 'archive-mirrors: [ \"file:///home/opam/opam-repository/cache\" ]' >> /home/opam/.opam/config"
    in
    let archive_build =
      Term.head target >>= fun h ->
      let git_rev = Commit.hash h in
      Opam_ops.V1.build_archive ~volume:(Fpath.v "opam-archive") docker_t docker_run_t git_rev
    in
    let archive_build_v2 =
      Term.head target >>= fun h ->
      let git_rev = Commit.hash h in
      Opam_ops.V2.build_archive ~volume:(Fpath.v "opam2-archive") docker_t docker_run_t git_rev
    in
    let bulk_build ~distro ~ocaml_version =
      Term_utils.after archive_build_v2 >>= fun () ->
      Term.head target >>= fun h ->
      let git_rev = Commit.hash h in
      Docker_build.run docker_t ~hum:(Fmt.strf "Base for %s (%s)" ocaml_version git_rev) (base_dfile ~distro ~ocaml_version ~git_rev)
      >>= fun img -> Opam_ops.V2.list_all_packages docker_run_t img
      >>= Opam_ops.V2.run_packages ~volume:(Fpath.v "opam2-archive") docker_run_t img
      >>= fun results ->
      let rec fn rs acc =
        match rs with
        |(package,t,log_branch)::tl ->
           (Term.state t >>= fun t ->
            let success = match t with Ok _ -> true |_ -> false in
            let open Opam_bulk_build in
            let r = {ocaml_version;distro;package;success;log_branch} in
            fn tl (r::acc))
        |[] -> Term.return (List.rev acc)
      in
      fn results []
    in
    let order = ref 1 in
    let tests, results =
      let ocaml_versions = ["4.03.0";"4.04.0";"4.02.3"] in
      let distros = ["ubuntu-16.04"; "alpine-3.4" ] in
      let results = ref [] in
      let ts = List.flatten (
        List.map (fun distro ->
          List.map (fun ocaml_version ->
            incr order;
            let t = bulk_build ~distro ~ocaml_version in
            let label = Fmt.strf "%s-%s" distro ocaml_version in
            results := t :: !results;
            Term_utils.report ~order:!order ~label t
          ) ocaml_versions
        ) distros)
      in
      let results =
        let rec fn l acc =
          match l with
          | hd::tl -> begin
              Term.state hd >>= function
              | Error (`Pending _) -> Term.pending "Waiting for jobs to complete (%d done)" (List.length acc)
              | _ -> Term.without_logs hd >>= fun t -> fn tl (t @ acc)
          end
          | [] -> Term.return (List.rev acc) in
        let t = Term.without_logs (fn !results []) in
        Term.state t >>= function
        | Error (`Pending _) -> Term.pending "Waiting for all builds to complete"
        | _ -> t >>= Opam_bulk_build.run opam_bulk_t
      in
      ts, results in
    let all_tests =
      (Term_utils.report ~order:0 ~label:"Summary" results) ::
      (Term_utils.report ~order:1 ~label:"OPAM2 archive" archive_build_v2) ::
      tests
    in
    match Target.id target with
    |`Ref ["heads";"bulk"] -> all_tests
    | _ -> []

  let tests = [
    Config.project ~id:"mirage/opam-repository" opam_build_all;
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"opam-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(username "admin")
    ~state_repo:(Uri.of_string "https://github.com/avsm/ocaml-ci.logs")
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

