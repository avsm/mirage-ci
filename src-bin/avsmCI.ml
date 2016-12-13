(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** An experimental "avsm-CI" for my personal projects, to see if I can automate
    some of the topkg publishing workflow. *)

open !Astring

open Datakit_ci
open Datakit_github

module Builder = struct

  open Term.Infix

  let one_hour = 60. *. 60.
  let opam_repo = Repo.v ~user:"mirage" ~repo:"opam-repository"
  let primary_ocaml_version = "4.03.0"

  let pool = Monitored_pool.create "docker" 2

  (* XXX TODO temporary until we can query package list automatically *)
  let packages_of_repo target =
    let repo = Target.repo target in
    match Fmt.strf "%a" Repo.pp repo with
    | "avsm/ocaml-dockerfile" -> ["dockerfile"]
    | _ -> failwith "TODO package_of_repo"

  let label = "mirage1" 
  let docker_t = Docker_build.v ~logs ~label ~pool ~timeout:one_hour ()
  let docker_run_t = Docker_run.config ~logs ~label ~pool ~timeout:one_hour
  let opam_t = Opam_build.config ~logs ~label
  let git_mk user repo = Git.v ~logs ~dir:("/home/avsm/mirage-ci/_checkouts"^"/"^user^"/"^repo)
  let git_t = git_mk "avsm" "ocaml-dockerfile"
  let toml_t = Toml_reader.config ~logs ~label ~git_t ~toml_filename:".datakit.toml"

  let rec term_map_s fn l =
    match l with
    | [] -> Term.return []
    | x :: l ->
        fn x >>= fun x ->
        term_map_s fn l >|= fun l ->
        x :: l

  (* base building *)
  let build ?(extra_remotes=[]) target distro ocaml_version = 
    let packages = packages_of_repo target in
    Term.branch_head opam_repo "master" >>= fun opam_repo_commit ->
    term_map_s (fun (repo,branch) ->
      Term.branch_head repo branch >>= fun commit ->
      Term.return (repo,branch,commit)
    ) extra_remotes >>= fun extra_remotes ->
    let remote_git_rev = Commit.hash opam_repo_commit in
    Term.target target >>= fun target -> 
    let hum = Fmt.(strf "opam install %a" (list ~sep:Format.pp_print_space string) packages) in
    Opam_build.(run opam_t {packages;target;distro;ocaml_version;remote_git_rev;extra_remotes}) >>=
    Docker_build.run ~hum docker_t

  let report ?(allow_fail=false) label img =
    let term =
      Term.state img >>= function
      | Error (`Pending p) -> Term.pending "%s" p
      | Error (`Failure f) when allow_fail -> Term.return (Fmt.strf "(Allowed failure) %s" f)
      | Error (`Failure f) -> Term.fail "%s" f
      | Ok _img            -> Term.return label
    in
    label, term

  let run_tests ?extra_remotes target test =
    let open Datakit_toml in
    let tests =
      List.map (fun ov ->
        List.map (fun distro ->
          build target distro ov
        ) test.distros
      ) test.ocamlv |> List.flatten
    in
    let rec proc (ok,fail) = function
      |hd::tl -> begin
        Term.state hd >>= function
        | Error (`Pending p) -> Term.pending "%s" p
        | Error (`Failure m) -> proc (ok, m::fail) tl
        | Ok r -> proc (r::ok,fail) tl
      end
      |[] ->
        let ok_s = match List.length ok with |0 -> "" |n -> Fmt.strf "OK(%d)" n in
        let err_s = match List.length fail with |0 -> "" |n -> Fmt.strf " ERR(%d)" n in
        Term.return (Fmt.strf "%s%s" ok_s err_s)
    in
    proc ([],[]) tests

  let run_toml () target =
    let t = match Target.id target with
    |`Ref ref -> begin
       Git.fetch_head git_t target >>= fun local_head ->
       Toml_reader.run toml_t local_head >>= fun tests ->
       match Datakit_toml.assoc ref tests with
       | None -> Term.return "skipped"
       | Some t -> run_tests target t
    end
    | _ -> Term.return "skipped" in
    ["Build", t]

  let tests = [
    Config.project ~id:"avsm/ocaml-dockerfile" (run_toml ())
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"avsm-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(username "admin")
    ~state_repo: (Uri.of_string "https://github.com/avsm/avsm-ci.logs")
    ()

open Cmdliner
let () =
  let info =
    let doc = "Experimental CI to build the $(i,topkg) workflow for personal projects" in
    Term.info ~version:"0.0" ~doc "avsmCI" in
  run ~info (Cmdliner.Term.pure (Config.v ~web_config ~projects:Builder.tests))

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

