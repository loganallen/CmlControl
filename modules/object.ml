open Unix
open Universal
open Tree

(******************************* Object Module ********************************)
(******************************************************************************)

(* creates a blob object for the given file. Returns the hash. *)
let create_blob (file_name: string) : string =
  let hsh = Crypto.hash file_name in
  let (d1,path) = split_hash hsh in
  let _ =  if Sys.file_exists (".cml/objects/"^d1) then () else mkdir (".cml/objects/"^d1) 0o777 in
  path |> open_out |> close_out; Crypto.compress file_name path; hsh

(* creates a commit object for the given commit. Returns the hash. *)
let create_commit (ptr : string) (user : string) (date : string) (msg: string) (parents : string list) : string =
  let rec write_commit oc = function
    | [] -> close_out oc
    | h::t -> Printf.fprintf oc "%s\n" h; write_commit oc t
  in
  let temp_name = ".cml/temp_commit"^ptr in
  let oc = open_out temp_name in
  let parents' = parents |> List.fold_left (fun acc p -> acc^" "^p) "" |> String.trim in
  let lines = [ptr; user; date; msg; parents'] in
  let _ = write_commit oc lines in
  let hsh = Crypto.hash temp_name in
  let (d1,path) = split_hash hsh in
    if not (Sys.file_exists (".cml/objects/"^d1))
    then mkdir (".cml/objects/"^d1) 0o777;
    Sys.rename temp_name path; hsh

(* returns a commit record for the given commit ptr *)
let parse_commit (ptr : string) : commit =
  try
    let (_,path) = split_hash ptr in
    let ic = open_in path in
    let tr = input_line ic in
    let usr = input_line ic in
    let dt = input_line ic in
    let msg = input_line ic in
    let ps = ic |> input_line |> Str.split (Str.regexp " ") in close_in ic;
      {tree=tr; author=usr; date=dt; message=msg; parents=ps}
  with
    | Sys_error _ -> raise (Fatal ("commit - "^ptr^": not found"))
    | Invalid_argument _ -> raise (Fatal ("commit - "^ptr^": not valid"))
    | End_of_file -> raise Corrupt

(* takes a commit hash and returns the index of the commit *)
let get_commit_index (ptr : string) : index =
  let cmt = parse_commit ptr in
  Tree.(read_tree "" cmt.tree |> tree_to_index)
