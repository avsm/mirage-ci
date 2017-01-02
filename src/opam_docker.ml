(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github

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

  let set_opam_repo_rev rev =
    workdir "/home/opam/opam-repository" @@
    run "git pull origin master" @@
    run "git checkout %s" rev @@
    run "opam update -uy"

  let base ~ocaml_version ~distro =
    from ~tag:(distro^"_ocaml-"^ocaml_version) "ocaml/opam"

  let clone_src ~user ~repo ~branch ~commit () =
    run "git clone git://github.com/%s/%s /home/opam/src" user repo @@
    workdir "/home/opam/src" @@
    run "git fetch origin %s:cibranch" branch @@
    run "git checkout %s" commit
end

module V2 = struct
  open !Dockerfile

  let add_remotes = V1.add_remotes
  let add_pins = V1.add_pins

  let set_opam_repo_rev rev =
    workdir "/home/opam/opam-repository" @@
    run "git checkout master" @@
    run "git pull origin master" @@
    run "git branch -D v2" @@
    run "git checkout -b v2 %s" rev @@
    run "opam admin upgrade-format" @@
    run "git add ." @@
    run "git commit -a -m 'upgrade format to opam2'" @@
    run "opam update -uy"
end

let dfile_v1 ?(pins=[]) ?(remotes=[]) ~ocaml_version ~distro ~opam_repo_git_rev
  ~user ~repo ~branch ~commit () =
  let open V1 in
  let (@@) = Dockerfile.(@@) in
  base ~ocaml_version ~distro @@
  set_opam_repo_rev opam_repo_git_rev @@
  add_remotes remotes @@
  add_pins pins @@
  clone_src ~user ~repo ~branch ~commit ()

let dfile ?(pins=[]) ?(remotes=[]) ~ocaml_version ~distro ~opam_repo_git_rev (target:Target.t) =
  let open Term.Infix in
  let {Repo.user; repo} = Target.repo target in
  let branch =
    match Target.id target with
    | `PR pr -> Printf.sprintf "pull/%d/head" pr
    | `Ref r -> Fmt.strf "%a" Ref.pp_name r
  in
  Term.target target >>= fun target ->
  let commit = Commit.hash (Target.head target) in
  let dfile = dfile_v1 ~pins ~remotes ~ocaml_version ~distro ~opam_repo_git_rev ~user ~repo ~branch ~commit () in
  Term.return dfile

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

