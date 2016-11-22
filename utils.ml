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
type index = (string * string) list

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

(* updates the index by adding a new mapping *)
let update_index (idx : index) (map : string * string) : index =
  let (file_path, _) = map in
  map::(List.remove_assoc file_path idx)

(* returns the index which is a list that maps tracked filenames
 * to their most recent hash string value *)
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
    | Sys_error _ -> []

(* initializes an index in the cml directory *)
let set_index (idx : index) : unit =
  let out_ch = open_out ".cml/index" in
  let rec write_index to_ch = function
    | [] -> close_out to_ch
    | (fp, h)::t -> output_string to_ch (fp ^ " " ^ h); write_index to_ch t
  in write_index out_ch idx

(* returns true if dir is known link, or if is .cml *)
let is_bad_dir name =
  let temp =
    try
      let st = String.rindex name '/' in
      String.sub name (st + 1) (String.length name - st - 1)
    with
      Not_found -> name
  in match temp with
    | "." | ".." | ".cml" -> true
    | _ -> false

(* returns a list of all files in working repo by absolute path *)
let rec get_all_files (dirs : string list) (acc : string list) : string list =
  let rec loop dir_h path files directories =
    try
      let temp = readdir dir_h in
      let f_name = if path = "" || path = "./" then temp else (path ^ "/" ^ temp) in
      if Sys.is_directory f_name then loop dir_h path files (f_name::directories)
      else loop dir_h path (f_name::files) directories
    with
      End_of_file -> closedir dir_h; (files, directories)
  in
  match dirs with
    | [] -> acc
    | dir_name::t ->
      begin
        if is_bad_dir dir_name then
          (*let _ = print_endline "skipped" in*)
          get_all_files t acc
        else
          (*let _ = print_endline dir_name in*)
          let dir_h = opendir dir_name in
          let (files, directories) = loop dir_h dir_name acc [] in
          get_all_files (t@directories) files
      end

(* returns a list of all files staged (added) for commit *)
let rec get_staged (idx : index) : string list = []
  (* TODO: compare [idx] to the index of the HEAD to see which file
   * hashes are added/staged for a commit *)

(* returns a list of changed files *)
let get_changed (cwd : string list) (idx : index) : string list =
  let find_changed acc fn =
    try
      let old_hash = List.assoc fn idx in
      let new_hash = hash fn in
      if old_hash = new_hash then acc else fn::acc
    with
    | Not_found -> acc
  in
  List.fold_left find_changed [] cwd

(* returns a list of untracked files *)
let get_untracked (cwd : string list) (idx : index) : string list =
  let find_untracked acc fn =
    try
      let _ = List.assoc fn idx in acc
    with
    | Not_found -> if fn.[0] = '.'then acc else fn::acc
  in
  List.fold_left find_untracked [] cwd

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise raises an exception *)
let cml_initialized (path : string) : bool =
  Sys.file_exists ".cml"

