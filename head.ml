open Unix
open Common

(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

(* returns the current HEAD of the cml repository *)
let get_head () : string =
  try
    let ch = open_in ".cml/HEAD" in
    let path = ".cml/" ^ input_line ch in close_in ch;
    let ch' = open_in path in
    let head = input_line ch' in close_in ch'; head
  with
    | Sys_error _ -> raise (Fatal "HEAD not found")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* sets the HEAD of the cml repository *)
let set_head (commit_hash : string) : unit =
  try
    let ch = open_in ".cml/HEAD" in
    let path = ".cml/" ^ input_line ch in close_in ch;
    let ch' = open_out path in
      output_string ch' commit_hash; close_out ch'
  with
    | Sys_error _ -> raise (Fatal "could not set HEAD")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* sets the HEAD of the cml repository to a commit hash *)
let set_detached_head (commit_hash : string) : unit =
  try
    let ch = open_out ".cml/HEAD" in
    output_string ch commit_hash; close_out ch
  with
    | Sys_error _ -> raise (Fatal "could not set detached HEAD")

(* returns the commit hash the head was detached at *)
let get_detached_head () : string =
  try
    let ch = open_in ".cml/HEAD" in
    let raw = input_line ch in close_in ch; raw
  with
  | Sys_error _ -> raise (Fatal "could not get detached HEAD")

(* returns true if repo currently is in detached head mode, else false *)
let detached_head () : bool =
  let raw = get_detached_head () in
  String.sub raw 0 4 <> "head"

(* returns correct head depending on detached_head mode *)
let get_head_safe () =
  if detached_head () then get_detached_head () else get_head ()
