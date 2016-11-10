(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner
open Rresult

let term_res = function
  | Error (`Msg m) -> `Error (false, m)
  | Ok r -> `Ok r

let setup style_renderer log_level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ())

let setup =
  let style_renderer = Fmt_cli.style_renderer () in
  let log_level = Logs_cli.level () in
  Term.(const setup $ style_renderer $ log_level)

let lint file () = begin
  let open R.Infix in
  Fpath.v file |>
  Bos.OS.File.read >>=
  Datakit_toml.of_string >>= fun tests ->
  Fmt.(pr "Processing %a and found these tests:@.@.%!" (styled `Underline string) file);
  Fmt.(pr "%a@.%!" (list ~sep:Format.pp_force_newline Datakit_toml.pp) tests);
  Ok ()
  end |> term_res

let file =
  Arg.(value & (pos 0 non_dir_file ".datakit.toml") & info [] ~docv:"FILE")

let cmd =
  let doc = "lint a DataKit-CI Toml configuration" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) prints the configuration represented in a $(t,.datakit.toml)
        file to standard output."; ]
  in
  Term.(ret (const lint $ file $ setup)),
  Term.info "datakitToml" ~version:"1.0.1" ~doc ~man

let () = match Term.eval cmd with | `Error _ -> exit 1 | _ -> exit 0

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

