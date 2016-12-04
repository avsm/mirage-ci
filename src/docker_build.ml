(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open! Astring
open Lwt.Infix
open DataKitCI

let ( / ) = Datakit_path.Infix.( / )

let src = Logs.Src.create "datakit-ci.docker-build" ~doc:"Docker plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

let digest_of_dockerfile d =
  let dockerfile_str = Dockerfile.string_of_t d in
  Digest.string dockerfile_str |> Digest.to_hex

module Dockerfile_key = struct
  type t = Dockerfile.t * string
  let compare = Pervasives.compare
end

type image = {
  tag: string;
  sha256: string;
  hum: string;
}

module Docker_builder = struct
  type t = {
    label: string;
    pool: Monitored_pool.t;
    timeout: float;
  }

  module Key = Dockerfile_key

  type context = job_id
  type value = image

  let name t = "docker-build:" ^ t.label

  let title _t (dockerfile,hum) =
    digest_of_dockerfile dockerfile |> fun digest ->
    Fmt.strf "Building %s (%s)" hum (String.with_range ~len:6 digest)

  let run_long_cmd ~switch t job_id fn =
    Monitored_pool.use ~label:"docker build" t.pool job_id (fun () ->
      Utils.with_timeout ~switch t.timeout fn
    )

  let check_docker_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Utils.failf "Build exit %d" n
  | Unix.WSIGNALED x -> Utils.failf "Signal %d" x
  | Unix.WSTOPPED x -> Utils.failf "Stopped %d" x

  let generate t ~switch ~log trans job_id (dockerfile,hum) =
    let dockerfile_str = Dockerfile.string_of_t dockerfile in
    let digest = Digest.string dockerfile_str |> Digest.to_hex in
    let output = Live_log.write log in
    let tag = t.label ^ ":" ^ digest in
    Utils.with_tmpdir (fun tmp_dir ->
      Dockerfile_distro.generate_dockerfile ~crunch:true tmp_dir dockerfile;
      let fname = tmp_dir ^ "/Dockerfile" in
      let builton = string_of_float (Unix.gettimeofday ()) in
      let label = Printf.sprintf "--label com.docker.datakit.digest=%s --label com.docker.datakit.builton=%s" digest builton in
      run_long_cmd ~switch t job_id (fun switch ->
        let cmd = Printf.sprintf "docker build %s --network mirageci_opam_build -t %s --no-cache --rm --force-rm - < %s" label tag fname in
        Process.run_with_exit_status ~switch ~output ("", [|"sh";"-c";cmd|]) >>= fun exit_status ->
        check_docker_status exit_status;
        let cmd = Printf.sprintf "docker images -q --digests --no-trunc --filter \"label=com.docker.datakit.digest=%s\" --filter \"label=com.docker.datakit.builton=%s\"" digest builton in 
        let images_output = Buffer.create 1024 in
        Process.run ~switch ~output:(Buffer.add_string images_output) ("",[|"sh";"-c";cmd|]) >|= fun () ->
        Buffer.contents images_output |> fun sha256 -> String.trim sha256
      ) >>= fun sha256 ->
      let open Utils in
      DK.Transaction.create_or_replace_file trans (Cache.Path.value / "sha256") (Cstruct.of_string sha256) >>*= fun () ->
      DK.Transaction.create_or_replace_file trans (Cache.Path.value / "tag") (Cstruct.of_string tag) >>*= fun () ->
      DK.Transaction.create_or_replace_file trans (Cache.Path.value / "label") (Cstruct.of_string hum) >>*= fun () ->
      output (Fmt.strf "Recorded container with tag %s (%s)" tag sha256);
      Lwt.return (Ok {sha256; tag; hum} )
    )

  let branch _t (dockerfile,_) =
    digest_of_dockerfile dockerfile |>
    Fmt.strf "docker-build-%s"

  let load _t tr _key =
    let open Utils in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/sha256") >>*= fun sha256 ->
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/tag") >>*= fun tag ->
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/label") >>*= fun hum ->
    Lwt.return {
      sha256=String.trim (Cstruct.to_string sha256);
      tag=String.trim (Cstruct.to_string tag);
      hum=String.trim (Cstruct.to_string hum);
    }
end
 
module Docker_build_cache = Cache.Make(Docker_builder)

type t = Docker_build_cache.t 
let config ~logs ~label ~pool ~timeout =
  Docker_build_cache.create ~logs { Docker_builder.label; pool; timeout }

let run config ~hum dfile =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Docker_build_cache.term config job_id (dfile,hum)


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
