(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_ci
open! Astring
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.docker-run" ~doc:"Docker_run plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Datakit_path.Infix.( / )

type key = {
  img: string;
  cmd: string list;
  volumes: (Fpath.t * Fpath.t) list;
  hum: string;
}

module Docker_run_key = struct
 type t = key
 let compare = Pervasives.compare
end

let branch {img;cmd;volumes;_} =
  Fmt.strf "%s:%s:%s" img (String.concat ~sep:" " cmd)
    (String.concat ~sep:" " (List.map (fun (a,b) -> Fmt.strf "%a:%a" Fpath.pp a Fpath.pp b) volumes)) |>
  Digest.string |> Digest.to_hex |> Fmt.strf "docker-run-%s"

module Docker_runner = struct
  type t = {
    label: string;
    pool: Monitored_pool.t;
    timeout: float;
  }

  module Key = Docker_run_key

  type context = job_id
  type value = string

  let name t = "docker-run:" ^ t.label
  let title _t {hum;_} = hum

  let generate t ~switch ~log trans job_id {img;cmd;volumes} =
    let tee outputs s = List.iter (fun o -> o s) outputs in
    let vols = List.flatten (List.map (fun (h,c) -> ["-v";(Fmt.strf "%a:%a" Fpath.pp h Fpath.pp c)]) volumes) in
    let cmd = Array.of_list ("docker"::"run"::"--rm"::"--tmpfs"::"/tmp"::vols@img::cmd) in
    let cmd_output = Buffer.create 1024 in
    let output = tee [ Buffer.add_string cmd_output; Live_log.write log ] in
    Monitored_pool.use ~log ~label:"docker run" t.pool job_id (fun () ->
      Utils.with_timeout ~switch t.timeout (fun switch ->
        Process.run ~switch ~output ("",cmd) >|= fun () -> 
        Buffer.contents cmd_output
      )
    ) >>= fun cmd_output ->
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "output") (Cstruct.of_string cmd_output) >>*= fun () ->
    Lwt.return (Ok cmd_output)

  let branch _t = branch

  let load _t tr _key =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/output") >>*= fun output ->
    Lwt.return (Cstruct.to_string output)
end
 
module Docker_run_cache = Cache.Make(Docker_runner)

type t = Docker_run_cache.t 
let v ~logs ~label ~pool ~timeout () =
  Docker_run_cache.create ~logs { Docker_runner.label; pool; timeout }

let run ?(volumes=[]) ?hum ~tag ~cmd config =
  let open! Term.Infix in
  let hum = match hum with None -> String.concat ~sep:" " cmd | Some h -> h in
  Term.job_id >>= fun job_id ->
  Docker_run_cache.find config job_id {img=tag;hum;cmd;volumes}

let branch ?(volumes=[]) ~tag ~cmd () =
  branch {img=tag;hum="";cmd;volumes}

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
