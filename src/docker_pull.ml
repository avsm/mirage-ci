(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open! Astring
open Lwt.Infix
open Datakit_ci
module DB = Docker_build

let ( / ) = Datakit_client.Path.Infix.( / )

let src = Logs.Src.create "datakit-ci.docker-pull" ~doc:"Docker pull plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

type key = {
  slug: string;
  tag: string option;
  time: Ptime.t;
}

module Docker_pull_key = struct
  type t = key

  let ( ++ ) x fn =
    match x with
    | 0 -> fn ()
    | r -> r

  let _compare {slug; tag; time} b =
    Pervasives.compare slug b.slug ++ fun () ->
    Pervasives.compare tag b.tag ++ fun () ->
    Ptime.compare time b.time
end

module Docker_puller = struct
  type t = {
    label: string;
  }

  module Key = Docker_pull_key

  type context = job_id
  type value = DB.image

  let name t = "docker-pull:" ^ t.label

  let pull_frag_of_t slug tag =
    let tag' = match tag with None -> "" | Some t -> ":"^t in
    slug ^ tag'

  let imgs_of_sha256s slug tag =
    let tag = pull_frag_of_t slug tag in
    List.map (fun sha256 -> { DB.sha256; tag=Some tag; hum=tag })

  let title _t {slug; tag; time} =
    Fmt.strf "Pulling %s (%a)" (pull_frag_of_t slug tag) Ptime.pp time

  let generate _t ~switch ~log trans _job_id {slug; tag; _} =
    let output = Live_log.write log in
    let cmd = Printf.sprintf "docker pull %s" (pull_frag_of_t slug tag) in
    Process.run ~switch ~output ("", [|"sh";"-c"; cmd|]) >>= fun () ->
    let images_output = Buffer.create 1024 in
    let cmd = Printf.sprintf "docker images -q --digests --no-trunc %s" slug in
    Process.run ~switch ~output:(Buffer.add_string images_output) ("",[|"sh";"-c";cmd|]) >>= fun () ->
    let sha256s = Buffer.contents images_output |> String.trim |> String.cuts ~empty:false ~sep:"\n" |> List.map String.trim in
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "sha256s") (Cstruct.of_string (String.concat ~sep:"\n" sha256s)) >>*= fun () ->
    match imgs_of_sha256s slug tag sha256s with
    | img :: _ -> Lwt.return (Ok img)
    | [] -> Lwt.return (Error (`Failure "Pull failed"))

  let branch _t {slug; tag; time} =
    let tag = match tag with None -> "latest" | Some t -> t in
    Fmt.strf "docker-pull-%s-%s-%Ld" slug tag (Ptime.to_float_s time |> Int64.of_float) 

  let load _t tr {slug; tag; _ }  =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_client.Path.of_string_exn "value/sha256s") >>*= fun sha256s ->
    let sha256s = Cstruct.to_string sha256s |> String.cuts ~empty:false ~sep:"\n" in
    match imgs_of_sha256s slug tag sha256s with
    | img :: _ -> Lwt.return img
    | [] -> Lwt.fail_with "Pull failed"
end
 
module Docker_pull_cache = Cache.Make(Docker_puller)

type t = Docker_pull_cache.t 
let v ~logs ~label () =
  Docker_pull_cache.create ~logs { Docker_puller.label }

let run ?tag ~slug ~time config =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Docker_pull_cache.find config job_id { slug; tag; time }


(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy

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
