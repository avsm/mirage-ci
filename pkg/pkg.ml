#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-ci" @@ fun c ->
  Ok [
    Pkg.lib "src/mirage-ci.mllib";
    Pkg.bin "src-bin/mirageCI";
    Pkg.bin "src-bin/opam-bulk-scry";
(*
    Pkg.bin "src-bin/opamCI"; *)
    Pkg.bin "src-bin/opamRepoCI";
    Pkg.bin "src-bin/opamBulkCI";
(*
    Pkg.bin "src-bin/datakitToml";
    Pkg.bin "src-bin/opam-bulk-scry"; *)
  ]
