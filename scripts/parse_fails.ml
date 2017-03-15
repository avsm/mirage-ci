#!/usr/bin/env ocamlscript
(* Parse the DataKit CI JSON logs for revdep build failures *)
Ocaml.packs := ["ezjsonm"; "astring"]
--

open Astring
open Printf
let rec find_title h = function
  | `Null | `Bool _ | `Float _ | `String _ -> ()
  | `A vl -> List.iter (find_title h) vl
  | `O kl ->
      match (List.assoc "title" kl), (List.assoc "failed" kl), (List.assoc "branch" kl) with
      | (`String t), (`Bool true), (`String b) ->
          String.span ~rev:true ~sat:((!=) ' ') t |> snd |> fun t ->
          Hashtbl.replace h t b
      | _ -> List.iter (fun (_,v) -> find_title h v) kl
      | exception Not_found -> List.iter (fun (_,v) -> find_title h v) kl

let _ =
  Hashtbl.create 100 |> fun h ->
  open_in "output" |> Ezjsonm.from_channel |> find_title h;
  eprintf "Found %d failures\n%!" (Hashtbl.length h);
  Hashtbl.iter (fun k v ->
    printf "git checkout %s\n" v;
    printf "cp log $ODIR/%s\n" k;
  ) h

