open Unix
open Universal

(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

(* returns the current HEAD of the cml repository *)
let get_head () : string =
  try
    let ic = open_in ".cml/HEAD" in
    let path = ".cml/" ^ input_line ic in close_in ic;
    let oc = open_in path in
    let head = input_line oc in
    close_in oc; head
  with
    | Sys_error _ -> raise (Fatal "HEAD not found")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* sets the HEAD of the cml repository *)
let set_head (cmt_hash : string) : unit =
  try
    let ic = open_in ".cml/HEAD" in
    let path = ".cml/" ^ input_line ic in close_in ic;
    let oc = open_out path in
    output_string oc cmt_hash; close_out oc
  with
    | Sys_error _ -> raise (Fatal "could not set HEAD")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* sets the HEAD of the cml repository to a commit hash *)
let set_detached_head (cmt_hash : string) : unit =
  try
    let ic = open_out ".cml/HEAD" in
    output_string ic cmt_hash; close_out ic
  with
    | Sys_error _ -> raise (Fatal "could not set detached HEAD")

(* returns the commit hash the head was detached at *)
let get_detached_head () : string =
  try
    let ic = open_in ".cml/HEAD" in
    let raw = input_line ic in
    close_in ic; raw
  with
  | Sys_error _ -> raise (Fatal "could not get detached HEAD")

(* returns true if repo currently is in detached head mode, else false *)
let detached_head () : bool =
  let raw = get_detached_head () in
  String.sub raw 0 4 <> "head"

(* returns correct head depending on detached_head mode *)
let get_head_safe () =
  if detached_head () then get_detached_head () else get_head ()
