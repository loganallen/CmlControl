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
type tree = Blob of string * string | Tree of string * tree list
type index = (string * string) list

exception Fatal of string

let perm = 0o777

(***************************** Generic Helpers ********************************)
(******************************************************************************)

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise raises an exception *)
let cml_initialized (path : string) : bool =
  Sys.file_exists ".cml"

(* ($) is an infix operator for appending a char to a string *)
let ($) (str : string) (c : char) : string =  str ^ Char.escaped c

(* returns (dir_name, file_name) for any string with the format "dir_name/file_name" *)
let split_path fn =
  let split = String.index fn '/' in
  let dir_name = String.sub fn 0 split in
  let file_name = String.sub fn (split + 1) (String.length fn - split - 1) in
  (dir_name, file_name)

(************************* File Compression & Hashing *************************)
(******************************************************************************)

(* hash returns a SHA-1 hash of a given input *)
let hash (file_name : string) : string =
	try
		let fd = openfile file_name [O_RDONLY] 0o777 in
		let channel = in_channel_of_descr fd in
		let hash = hash_channel (Hash.sha1 ()) channel in
		close_in channel; transform_string (Hexa.encode ()) hash
	with
		Unix_error (Unix.ENOENT,_,_) -> raise (Fatal ("Could not find file: "^file_name))

(* [copy filename destination] creates exact copy of filename at destination *)
let copy (file_name : string) (dest_path : string) : unit =
  let rec loop ic oc =
    try Printf.fprintf oc ("%s\n") (input_line ic); loop ic oc with End_of_file -> close_in ic; close_out oc
  in try
    let ic = open_in file_name in
    let oc = open_out dest_path in
    loop ic oc
  with
    Sys_error _ -> raise (Fatal "utils.copy, file not found")

(* compress compresses a file/directory
 * takes initial path and final path as arguments.
 *)
let compress (file_name : string) (dest_path : string) : unit =
  let oc = Gzip.open_out dest_path in
  let ic = open_in file_name in
  let rec loop ic oc =
    try
      Gzip.output_char oc (input_char ic); loop ic oc
    with
      | Sys_error _ -> raise (Fatal (file_name^" not found"))
      | End_of_file -> close_in ic; Gzip.close_out oc
  in loop ic oc

(* decompress decompresses a file/directory
 * takes initial and final path as arguments.
 *)
let decompress (file_name : string) (dest_path : string) : unit =
  let rec loop ic acc =
    try loop ic (acc $ (Gzip.input_char ic)) with End_of_file -> Gzip.close_in ic; acc
  in try
    let ic = Gzip.open_in file_name in
    let oc = open_out dest_path in
    Printf.fprintf oc "%s" (loop ic ""); close_out oc
  with
    | Sys_error _ -> failwith (file_name ^ " not found")
    | _ -> raise (Fatal "Gzip error - file empty or not Gzip")

(* creates a blob object for the given file. Returns the hash. *)
let create_blob (file_name: string) : string =
  let hsh = hash file_name in
  let d1 = String.sub hsh 0 2 in
  if not (Sys.file_exists (".cml/objects/"^d1)) then
    mkdir (".cml/objects/"^d1) perm;
  let f1 = String.sub hsh 2 (String.length hsh -2) in
  let path = ".cml/objects/"^d1^"/"^f1 in
    open_out path |> close_out; copy file_name path; d1^f1

(************************** Tree Creation & Helpers ***************************)
(******************************************************************************)
let rec tree_insert tree (fn, hn) =
  let rec loop acc (fn, hn) = function
    | [] ->
      begin
        try
          let (dir_name, file_name) = split_path fn in
          (Tree (dir_name, [Blob(file_name, hn)]))::acc
        with Not_found -> Blob (fn, hn)::acc
      end
    | (Tree (n, lst))::t ->
      begin
        try
          let (dir_name, file_name) = split_path fn in
          if dir_name = n then
            let _ = print_endline "here" in
            (tree_insert (Tree (n, lst)) (file_name, hn))::(acc @ t)
          else raise Not_found
        with Not_found -> loop ((Tree (n, lst))::acc) (fn, hn) t
      end
    | (Blob (fx, hx))::t -> loop ((Blob (fx, hx))::acc) (fn, hn) t
  in match tree with
    | Tree (n, lst) -> Tree (n, loop [] (fn, hn) lst)
    | _ -> failwith "error"

let make_tree (idx : index) =
  let tree = Tree (".", []) in
  List.fold_left tree_insert tree idx

let rec write_tree tree =
  let rec tree_data acc = function
    | [] -> acc
    | (Blob (fn,hn))::t -> tree_data ((fn ^ " " ^ hn)::acc) t
    | (Tree (n, lst))::t -> tree_data ((write_tree (Tree (n, lst)))::acc) t
  in let rec write_lines oc = function
    | [] -> close_out oc;
    | h::t -> Printf.fprintf oc "%s\n" h; write_lines oc t
  in match tree with
    | Tree (n, lst) ->
      begin
        let temp_name = ".cml/temp_"^n in
        let oc = open_out temp_name in
        let _ = write_lines oc (tree_data [] lst) in
        let hsh = hash temp_name in
        let d1 = String.sub hsh 0 2 in
        if not (Sys.file_exists (".cml/objects/"^d1)) then
        mkdir (".cml/objects/"^d1) perm;
        let f1 = String.sub hsh 2 (String.length hsh -2) in
        let path = ".cml/objects/"^d1^"/"^f1 in
        Sys.rename temp_name path; path
      end
    | _ -> failwith "write-tree error"

(* creates a tree object for the given directory. Returns the hash.*)
let create_tree (idx : index) : string =
  let tree = make_tree idx in
  write_tree tree

(* creates a commit object for the given commit. Returns the hash. *)
let create_commit (msg: string) (idx : index) (last_commit : string) : string =
  let rec write_commit oc = function
    | [] -> close_out oc
    | h::t -> Printf.fprintf oc "%s\n" h; write_commit oc t
  in
  let commit_tree = create_tree idx in
  let temp_name = ".cml/temp_commit"^commit_tree in
  let oc = open_out temp_name in
  let _ = write_commit oc [("tree " ^ commit_tree);("mesg " ^ msg);("lstc " ^ last_commit)] in
  let hsh = hash temp_name in
  let d1 = String.sub hsh 0 2 in
  if not (Sys.file_exists (".cml/objects/"^d1)) then
  mkdir (".cml/objects/"^d1) perm;
  let f1 = String.sub hsh 2 (String.length hsh -2) in
  let path = ".cml/objects/"^d1^"/"^f1 in
  Sys.rename temp_name path; path


(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

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
		| Sys_error _ -> raise (Fatal "HEAD not found")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* sets the HEAD of the cml repository *)
let set_head (commit_hash : string) : unit =
	try
		let st_ch = open_in ".cml/HEAD" in
		let path = ".cml/" ^ input_line st_ch in
		close_in st_ch;
		let out_ch = open_out path in
		output_string out_ch commit_hash; close_out out_ch
	with
		| Sys_error _ -> raise (Fatal "could not set HEAD")
		| End_of_file -> raise (Fatal "HEAD not initialized")

(* returns the HASH of a head of the given branch *)
let get_branch_ptr (branch_name : string) : string =
	try
		let in_ch = open_in (".cml/heads/" ^ branch_name) in
		let ptr = input_line in_ch in
		close_in in_ch; ptr
	with
		| Sys_error _ -> raise (Fatal ("branch "^branch_name^" not found"))
		| End_of_file -> raise (Fatal (branch_name^" ptr not set"))

(* initializes a given commit to a given branch name *)
let set_branch_ptr (branch_name : string) (commit_hash : string) : unit =
	try
		let out_ch = open_out (".cml/heads/" ^ branch_name) in
		output_string out_ch commit_hash; close_out out_ch
	with
		| Sys_error _ -> raise (Fatal "write error")


(* returns a list of all versions of HEAD *)
let get_versions () : string list =
  []

(* go to an old version of HEAD *)
(* precondition: [version] of HEAD exists *)
let switch_version (version : string) : unit =
  ()

(***************************** Index Manipulation *****************************)
(******************************************************************************)

(* returns the index which is a list that maps tracked filenames
* to their most recent hash string value *)
let get_index () : index =
  try
    let rec parse_index acc ch =
      try
        let raw = input_line ch in let split = String.index raw ' ' in
        let file_path = String.sub raw 0 split in
        let hash = String.sub raw (split+1) (String.length raw - (split+1)) in
          parse_index ((file_path,hash)::acc) ch
      with
        End_of_file -> close_in ch; acc
    in parse_index [] (open_in ".cml/index")
  with
    | Sys_error _ -> []

(* updates the index by adding a new mapping *)
let update_index (map : string * string) (idx : index) : index =
  map :: (List.remove_assoc (fst map) idx)


(* initializes an index in the cml directory *)
let set_index (idx : index) : unit =
  let rec write_index ch = function
    | [] -> close_out ch
    | (f,h)::t -> output_string ch (f^" "^h^"\n"); write_index ch t
  in
  write_index (open_out ".cml/index") idx

(****************************** File Fetching *********************************)
(******************************************************************************)

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
      let f_name = (if path = "" || path = "./" then temp else (path^"/"^temp)) in
      if Sys.is_directory f_name then
        loop dir_h path files (f_name::directories)
      else
        loop dir_h path (f_name::files) directories
    with
      End_of_file -> closedir dir_h; (files, directories)
  in match dirs with
    | [] -> List.sort (Pervasives.compare) acc
    | dir_name::t -> begin
      if is_bad_dir dir_name then
        get_all_files t acc
      else
        let dir_h = opendir dir_name in
        let (files, directories) = loop dir_h dir_name acc [] in
        get_all_files (t@directories) files
    end

(* returns a list of all files staged (added) for commit *)
(* precondition: all files have objects in [.cml/objects/] *)
let rec get_staged (idx : index) : string list =
  let find_staged acc (f,h) =
    let d1 = String.sub h 0 2 in
    let f1 = String.sub h 2 (String.length h - 2) in
      if (Sys.file_exists (".cml/objects/"^d1^"/"^f1)) then f::acc else acc
  in
  List.fold_left find_staged [] idx |> List.sort (Pervasives.compare)

(* returns a list of changed files (different from the working index) *)
let get_changed (cwd : string list) (idx : index) : string list =
  let find_changed acc fn =
    try
      let old_hash = List.assoc fn idx in
      let new_hash = hash fn in
      if old_hash = new_hash then acc else fn::acc
    with
    | Not_found -> acc
  in
  List.fold_left find_changed [] cwd |> List.sort (Pervasives.compare)

(* returns a list of untracked files *)
let get_untracked (cwd : string list) (idx : index) : string list =
  let find_untracked acc fn =
    try
      let _ = List.assoc fn idx in acc
    with
    | Not_found -> if fn.[0] = '.' then acc else fn::acc
  in
  List.fold_left find_untracked [] cwd |> List.sort (Pervasives.compare)

(******************************** Branching ***********************************)
(******************************************************************************)

(* validate the branch name *)
let validate_branch (branch : string) : unit =
  if (String.sub branch 0 1) = "." || (String.sub branch 0 1) = "-" then
    raise (Fatal "invalid branch name")
  else ()

(* returns a list of all branches in alphabetical order*)
let get_branches () : string list =
  let rec branch_loop acc q =
    match q with
    | []   -> acc
    | h::t -> begin
      let temp = ".cml/heads/"^h in
      if Sys.is_directory temp then
        let subs = Sys.readdir temp |> Array.to_list
        |> List.map (fun f -> h^"/"^f) in branch_loop acc t@subs
      else
        branch_loop (h::acc) t
    end
  in
  Sys.readdir ".cml/heads" |> Array.to_list |> branch_loop [] |>
  List.sort (Pervasives.compare)

(* returns string of name of the current branch *)
let get_current_branch () : string =
  try
    let ch = open_in ".cml/HEAD" in
    let raw = input_line ch in
    let _ = close_in ch in
    let split = (String.index raw '/') + 1 in
    String.sub raw split (String.length raw - split)
  with
    | Sys_error _ -> raise (Fatal "HEAD not found")
    | End_of_file -> raise (Fatal "HEAD not initialized")

(* recursively creates branch sub-directories as needed *)
let rec branch_help (path : string) (branch : string) : unit =
  try
    let slash = String.index branch '/' in
    let dir = String.sub branch 0 slash in
    let prefix = path^dir in
    if not (Sys.file_exists prefix) then mkdir prefix perm;
    String.sub branch (slash+1) ((String.length branch) - (slash+1)) |>
      branch_help (prefix^"/")
  with
  | Not_found -> open_out (path^"/"^branch) |> close_out

(* create a new branch if it doesn't exist *)
let create_branch (branch : string) : unit =
  if (get_branches () |> List.mem branch) then
    raise (Fatal ("a branch named "^branch^" already exists"))
  else
    let _ = validate_branch branch in
    if String.contains branch '/' then
      branch_help ".cml/heads/" branch
    else
      open_out (".cml/heads/"^branch) |> close_out

(* delete a branch if it exists *)
let delete_branch (branch : string) : unit =
  if branch = get_current_branch () then
    raise (Fatal ("cannot delete branch '"^branch^"'"))
  else
    try
      if (get_branches () |> List.mem branch) then
        let _ = Sys.remove (".cml/heads/"^branch) in
        print ("Deleted branch "^branch^"")
      else
        raise (Fatal ("branch '"^branch^"' not found"))
    with
    | Sys_error _ -> raise (Fatal "cannot perform such an operation")

(* switch current working branch *)
(* precondition: [branch] exists *)
let switch_branch (branch : string) : unit =
  if (get_current_branch () = branch) then
    print ("Already on branch '"^branch^"'")
  else
    let ch = open_out ".cml/HEAD" in
    output_string ch ("heads/"^branch); close_out ch;
    let _ = print ("Switched to branch '"^branch^"'") in
    get_branch_ptr branch |> switch_version

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
let get_user_info () : string =
  try
    let ch = open_in ".cml/config" in
    let raw = input_line ch in
    let split = (String.index raw ' ') + 1 in
    let len = (String.length raw) - split in
    close_in ch; String.sub raw split len
  with
  | Sys_error _ -> raise (Fatal "username not set, set with [--user <name>]")

(* set the user info (username) *)
let set_user_info (name : string) : unit =
  let ch = open_out ".cml/config" in
  output_string ch ("user: "^name^"\n"); close_out ch
