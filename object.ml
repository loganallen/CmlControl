open Unix
open Common
open Tree

(******************************* Object Module ********************************)
(******************************************************************************)

(* creates a blob object for the given file. Returns the hash. *)
let create_blob (file_name: string) : string =
  let hsh = Crypto.hash file_name in
  let (d1,path) = split_hash hsh in
  let _ =  if Sys.file_exists (".cml/objects/"^d1) then () else mkdir (".cml/objects/"^d1) 0o777 in
  open_out path |> close_out; Crypto.compress file_name path; hsh

(* creates a commit object for the given commit. Returns the hash. *)
let create_commit (ptr : string) (user : string) (date : string) (msg: string) (parents : string list) : string =
  let rec write_commit oc = function
    | [] -> close_out oc
    | h::t -> Printf.fprintf oc "%s\n" h; write_commit oc t
  in
  let temp_name = ".cml/temp_commit"^ptr in
  let oc = open_out temp_name in
  let parents' = List.fold_left (fun acc p -> acc^" "^p) "" parents |> String.trim in
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
    let ch = open_in path in
    let tree = input_line ch in
    let user = input_line ch in
    let time = input_line ch in
    let msg = input_line ch in
    let parents = input_line ch |> Str.split (Str.regexp " ") in close_in ch;
      {tree=tree; author=user; date=time; message=msg; parents=parents}
  with
    | Sys_error _ -> raise (Fatal ("commit - "^ptr^": not found"))
    | Invalid_argument _ -> raise (Fatal ("commit - "^ptr^": not valid"))
    | End_of_file -> raise (Fatal ("commit - "^ptr^": corrupted"))

(* takes a commit hash and returns the index of the commit *)
let get_commit_index (ptr : string) : index =
  try
    let commit = parse_commit ptr in
    Tree.read_tree "" commit.tree |> Tree.tree_to_index
  with
    | Tree_ex _ -> raise (Fatal ("commit - " ^ ptr ^ " is corrupted"))