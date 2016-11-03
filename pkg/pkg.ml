#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage_ci" @@ fun c ->
  Ok [ Pkg.bin "src/mirage_ci" ]
