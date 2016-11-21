open Unix
open Cryptokit
open Print

type commit = {
  author: string;
  message: string;
  date: string;
  tree_ptr: string;
  prev_commit_ptr: string;
}

type blob = string
type tree = string list
type mapping = string * string
type index = mapping list

type obj = Blob of blob | Tree of tree | Commit of commit


(* hash returns a SHA-1 hash of a given input *)
let hash (file_name : string) : string =
	try
		let fd = openfile file_name [O_RDONLY] 0o777 in
		let channel = in_channel_of_descr fd in
		let hash = hash_channel (Hash.sha1 ()) channel in
		close_in channel; transform_string (Hexa.encode ()) hash
	with
		Unix_error (Unix.ENOENT,_,_) -> failwith ("Could not find file: " ^ file_name)

(* compress compresses a file/directory
 * takes initial path and final path as arguments.
 *)
let compress (file_name : string) (dest_path : string) : unit =
  failwith "Unimplemented"

(* decompress decompresses a file/directory
 * takes initial and final path as arguments.
 *)
let decompress (file_name : string) (dest_path : string) : unit =
  failwith "Unimplemented"

(* creates and object in the object directory and returns its name (hashed) *)
let create_obj (obj : obj) : string =
  failwith "Unimplemented"

(* takes hash and returns an object type *)
let parse_obj (file_name : string) : obj =
  failwith "Unimplemented"

(* returns the current HEAD of the cml repository *)
let get_head () : string =
	try
		let st_ch = open_in ".cml/HEAD" in
		let path = ".cml/" ^ input_line st_ch in
		close_in st_ch;
		let ed_ch = open_in path in
		let head = input_line ed_ch in
		close_in ed_ch; head
	with
		| Sys_error _ -> failwith "HEAD not found"
		| End_of_file -> failwith "HEAD not initialized"

(* sets the HEAD of the cml repository *)
let set_head (commit_hash : string) : unit =
	try
		let st_ch = open_in ".cml/HEAD" in
		let path = ".cml/" ^ input_line st_ch in
		close_in st_ch;
		let out_ch = open_out path in
		output_string out_ch commit_hash; close_out out_ch
	with
		| Sys_error _ -> failwith "could not set HEAD"
		| End_of_file -> failwith "HEAD not initialized"

(* returns the HASH of a head of the given branch *)
let get_branch_ptr (branch_name : string) : string =
	try
		let in_ch = open_in (".cml/heads/" ^ branch_name) in
		let ptr = input_line in_ch in
		close_in in_ch; ptr
	with
		| Sys_error _ -> failwith ("branch " ^ branch_name ^ " not found")
		| End_of_file -> failwith (branch_name ^ " ptr not set")

(* initializes a given commit to a given branch name *)
let set_branch_ptr (branch_name : string) (commit_hash : string) : unit =
	try
		let out_ch = open_out (".cml/heads/" ^ branch_name) in
		output_string out_ch commit_hash; close_out out_ch
	with
		| Sys_error _ -> failwith ("write error")

(* updates the index by adding a mapping type *)
let update_index (idx : index) (map : mapping) : index =
  let (file_path, _) = map in
  map::(List.remove_assoc file_path idx)

(* returns the index *)
let get_index () : index =
  try
    let in_ch = open_in ".cml/index" in
    let rec parse_index ch acc =
      try
        let raw = input_line ch in let split = String.index raw ' ' in
        let file_path = String.sub raw 0 split in
        let hash = String.sub raw split (String.length raw - split) in
        (file_path,hash)::acc
      with
        End_of_file ->   close_in ch; acc
    in parse_index in_ch []
  with
    | Sys_error _ -> failwith ("index not found")

(* initializes an index in the cml directory *)
let set_index (idx : index) : unit =
  let out_ch = open_out ".cml/index" in
  let rec write_index to_ch = function
    | [] -> close_out to_ch
    | (fp, h)::t -> output_string to_ch (fp ^ " " ^ h); write_index to_ch t
  in write_index out_ch idx
