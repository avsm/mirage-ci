(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open! Astring
open Lwt.Infix
open Datakit_ci

let ( / ) = Datakit_path.Infix.( / )

let src = Logs.Src.create "datakit-ci.docker-build" ~doc:"Docker plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

let digest_of_dockerfile d =
  let dockerfile_str = Dockerfile.string_of_t d in
  Digest.string dockerfile_str |> Digest.to_hex

type key = {
  dockerfile: Dockerfile.t;
  hum: string;
  tag: string option;
  pull: bool;
}

module Dockerfile_key = struct
  type t = key

  let ( ++ ) x fn =
    match x with
    | 0 -> fn ()
    | r -> r

  let compare {dockerfile;hum;tag} b =
    Pervasives.compare dockerfile b.dockerfile ++ fun () ->
    String.compare hum b.hum ++ fun () ->
    Pervasives.compare tag b.tag
end

type image = {
  tag: string option;
  sha256: string;
  hum: string;
}

module Docker_builder = struct
  type t = {
    label: string;
    pool: Monitored_pool.t;
    timeout: float;
    network: string option;
  }

  module Key = Dockerfile_key

  type context = job_id
  type value = image

  let name t = "docker-build:" ^ t.label

  let title _t {dockerfile;hum;_} =
    digest_of_dockerfile dockerfile |> fun digest ->
    Fmt.strf "Building %s (%s)" hum (String.with_range ~len:6 digest)

  let run_long_cmd ~switch t job_id fn =
    Monitored_pool.use ~label:"docker build" t.pool job_id (fun () ->
      Utils.with_timeout ~switch t.timeout fn
    )

  let generate t ~switch ~log trans job_id {dockerfile;hum;tag;pull} =
    let digest = digest_of_dockerfile dockerfile in
    let output = Live_log.write log in
    let builton = string_of_float (Unix.gettimeofday ()) in
    let label = Printf.sprintf "--label com.docker.datakit.digest=%s --label com.docker.datakit.builton=%s" digest builton in
    let network_cli = match t.network with None -> "" | Some n -> " --network " ^ n in
    let tag_cli = match tag with None -> "" | Some t -> " -t " ^ t in
    let pull_cli = match pull with false -> "" | true -> " --pull" in
    let images_output = Buffer.create 1024 in
    Utils.with_tmpdir (fun tmp_dir ->
      Dockerfile_distro.generate_dockerfile ~crunch:true tmp_dir dockerfile;
      Monitored_pool.use ~log ~label:"docker build" t.pool job_id (fun () ->
        Utils.with_timeout ~switch t.timeout (fun switch ->
          let cmd = Printf.sprintf "docker build%s %s%s%s --rm --force-rm - < %s/Dockerfile" network_cli label tag_cli pull_cli tmp_dir in
          Process.run ~switch ~output ("", [|"sh";"-c";cmd|]) >>= fun () ->
          let cmd = Printf.sprintf "docker images -q --digests --no-trunc --filter \"label=com.docker.datakit.digest=%s\" --filter \"label=com.docker.datakit.builton=%s\"" digest builton in 
          Process.run ~switch ~output:(Buffer.add_string images_output) ("",[|"sh";"-c";cmd|]))
      )) >>= fun () ->
    let sha256 = Buffer.contents images_output |> String.trim in
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "sha256") (Cstruct.of_string sha256) >>*= fun () ->
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "tag") (Cstruct.of_string (match tag with None -> "" | Some t -> t)) >>*= fun () ->
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "label") (Cstruct.of_string hum) >>*= fun () ->
    output (Fmt.strf "Recorded container%s (%s)" (match tag with None -> "" | Some t -> Fmt.strf " with tag %s " t) sha256);
    Lwt.return (Ok {sha256; tag; hum} )

  let branch _t {dockerfile;_} =
    digest_of_dockerfile dockerfile |>
    Fmt.strf "docker-build-%s"

  let load _t tr _key =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/sha256") >>*= fun sha256 ->
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/tag") >>*= fun tag ->
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/label") >>*= fun hum ->
    let tag = match Cstruct.to_string tag with | "" -> None | x -> Some (String.trim x) in
    let sha256 = String.trim (Cstruct.to_string sha256) in
    let cmd = Printf.sprintf "docker inspect %s" sha256 in
    Process.run ~output:(fun _ -> ()) ("",[|"sh";"-c";cmd|]) >|= fun () ->
    { tag; sha256; hum=String.trim (Cstruct.to_string hum) }
end
 
module Docker_build_cache = Cache.Make(Docker_builder)

type t = Docker_build_cache.t 
let v ?network ~logs ~label ~pool ~timeout () =
  Docker_build_cache.create ~logs { Docker_builder.label; pool; timeout; network }

let run config ?(pull=false) ?tag ~hum dockerfile =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Docker_build_cache.find config job_id {dockerfile; hum; tag; pull}


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
