type t = { major: int; minor: int; patch: int option; extra: string option }

let v ?patch ?extra major minor = { major; minor; patch; extra }

let to_string =
  function
  | {major;minor;patch=None;extra=None} -> Printf.sprintf "%d.%02d" major minor
  | {major;minor;patch=Some patch;extra=None} -> Printf.sprintf "%d.%02d.%d" major minor patch
  | {major;minor;patch=Some patch;extra=Some extra} -> Printf.sprintf "%d.%02d.%d+%s" major minor patch extra
  | {major;minor;patch=None;extra=Some extra} -> Printf.sprintf "%d.%02d+%s" major minor extra

let parse s =
  try Scanf.sscanf s "%d.%d.%d+%s" (fun major minor patch extra -> v ~patch ~extra major minor)
  with End_of_file | Scanf.Scan_failure _ -> begin
      try Scanf.sscanf s "%d.%d+%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ -> begin
          try Scanf.sscanf s "%d.%d.%d" (fun major minor patch -> v ~patch major minor)
          with End_of_file | Scanf.Scan_failure _ -> begin
              Scanf.sscanf s "%d.%d" (fun major minor -> v major minor)
            end
        end
    end

let of_string s =
  try parse s with
  | exn ->
      raise (Invalid_argument (Printf.sprintf "Unable to parse OCaml version '%s'" s))

let pp ppf v = Fmt.pf ppf "%s" (to_string v)

let sexp_of_t t = Sexplib.Sexp.Atom (to_string t)
let t_of_sexp = function
  | Sexplib.Sexp.Atom s -> of_string s
  | _ -> raise (Failure "unexpected sexp element")

let ( ++ ) x fn =
  match x with
  | 0 -> fn ()
  | r -> r

let compare {major; minor; patch; extra} a =
  compare major a.major ++ fun () ->
    compare minor a.minor ++ fun () ->
      compare patch a.patch ++ fun () ->
        compare extra a.extra

let t = of_string Sys.ocaml_version

module Since = struct
  let bytes = of_string "4.03.0"
end

module Has = struct
  let bytes v =
    match compare Since.bytes v with
    |(-1) | 0 -> true
    |n -> false
end
