(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Astring

(*
module Store = Irmin_unix.Irmin_git.Memory(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module Sync = Irmin.Sync(Store)

let bulk_branch = "opam-bulk-6c6f8c7147291bceb9fc0d2986dce4b5"
let upstream = Irmin.remote_uri "git://github.com/avsm/ocaml-ci.logs"

let src = Logs.Src.create "canopy-store" ~doc:"Canopy store logger"
module Log = (val Logs.src_log src : Logs.LOG)

let store_config = Irmin_mem.config ()
let task s = Irmin.Task.create ~date:0L ~owner:"Server" s
let repo _ = Store.Repo.create store_config

let main root =
  let open Irmin_unix in
  Irmin_git.config ~root ~bare:true () |> fun config ->
  Store.Repo.create config >>= fun cfg ->
  Store.of_branch_id task bulk_branch cfg >>= fun t ->
  Sync.pull_exn ~depth:1 (t "pulling upstream") upstream `Update >>= fun () -> 
  Store.read_exn (t "read") ["value";"results.sexp"] >>= fun s ->
  print_endline s;
  Lwt.return ()
*)

let parse_raw (a,b) =
  let parse f =
    match Bos.OS.File.read (Fpath.v f) with
    | Ok f -> Astring.String.trim f |> Sexplib.Sexp.of_string |> Opam_bulk_build.keys_of_sexp
    | Error (`Msg m) -> failwith m
  in
  let open Opam_bulk_build in
  let a = parse a in let b = parse b in
  Opam_bulk_build.diff ~ocaml_version:"4.03.0" ~distro:"ubuntu-16.04" a b Format.std_formatter;
  Lwt.return_unit

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

(* let run_lwt db_dir () = Lwt_main.run (main db_dir) *)
let run_lwt raw () = Lwt_main.run (parse_raw raw)
open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let db_dir =
  let doc = "Directory to store the db in" in
  Arg.(required & opt (some dir) None & info ["d"; "db-directory"] ~docv:"DB_DIRECTORY" ~doc)

let raw =
  let doc = "Raw before/after sexp results to compare" in
  Arg.(required & opt (some (pair string string)) None & info ["r";"results"] ~docv:"RESULTS" ~doc)

let main () =
  match Term.(eval (const run_lwt $ raw $ setup_log, Term.info "opam-bulk-scry")) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)

let () = main ()
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
