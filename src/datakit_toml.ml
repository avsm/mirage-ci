(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Parse configuration keys from a TOML spec *)
open Rresult
open Astring

let error_to_string (msg,l) =
  let module T = Toml.Parser in
  Fmt.strf "[%s:line %d/col %d/pos %d] %s"
    l.T.source l.T.line l.T.column l.T.position msg

let toml_of_string s =
  Toml.Parser.from_string s |> function
  | `Ok r -> R.ok r
  | `Error e -> R.error_msg (error_to_string e)

let result_to_exn = function
  | Ok r -> r
  | Error (`Msg m) -> raise (Failure m)

open Sexplib.Conv
type t = {
  ref: string list;
  ocamlv: string list;
  flambda: bool;
  distros: string list;
} [@@deriving sexp]

type l = t list [@@deriving sexp]

let pp ppf {ref; ocamlv; flambda; distros} =
  let open Fmt in
  let slist t = list ~sep:(unit "/") t |> styled `Bold |> styled `Green in
  let clist = list ~sep:(unit ", ") in
  let bold s = styled_unit `Bold s in
  pf ppf "%a: %a@.%a: %a@.%a: %a@ flambda: %a@."
    (bold "References") () (slist string) ref
    (bold "Compilers") () (clist string) ocamlv
    (bold "Distributions") () (clist string) distros
    bool flambda

let compare = Pervasives.compare

let equal = (=)

let parse tbl =
  let get_ents mode =
    let open TomlTypes in
    let open TomlLenses in
    get tbl (key mode |-- table) |> function
    | None -> []
    | Some tbl ->
        List.map (fun (k,v) ->
            Table.Key.to_string k |> fun tag ->
            get tbl (key tag |-- table) |> function
            | None -> raise (Failure tag)
            | Some tbl ->
                let strings k =
                  match get tbl (key k |-- array |-- strings) with
                  | None -> []
                  | Some l -> l in
                let bool k =
                  match get tbl (key k |-- bool) with 
                  | None -> false
                  | Some b -> b in
                let ref =
                  match get tbl (key "name" |-- string) with 
                  | None -> [mode;tag]
                  | Some v -> [mode;v] in
                let ocamlv = strings "compilers" in
                let flambda = bool "flambda" in
                let distros = strings "distros" in
                { ref; ocamlv; flambda; distros }
          ) (Table.bindings tbl) in
  try R.ok ((get_ents "heads") @ (get_ents "tags"))
  with Failure f -> R.error_msg f

let assoc ref t =
  try Some (List.find (fun k -> k.ref = ref) t) with
  Not_found -> None

open R.Infix
let of_string s = toml_of_string s >>= parse
let of_string_exn s = of_string s |> result_to_exn

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
