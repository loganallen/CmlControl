open Unix
open Print
open Common
open Crypto
open Tree

type commit = {
  tree: string;
  author: string;
  date: string;
  message: string;
  parents: string list;
}

type blob = string

type index = (string * string) list

let perm = 0o777

(***************************** Generic Helpers ********************************)
(******************************************************************************)

(* returns the option path to the nearest .cml directory from the cwd
 * (current working directory). *)
let cml_path path =
  let rec cml_path_helper i acc =
    if i = 0 then
      raise (Fatal "Not a Cml repository (or any of the parent directories)")
    else
      if Sys.file_exists (acc^".cml") then
        Some acc
      else
        cml_path_helper (i-1) (acc^"../")
  in
  let cwd = Sys.getcwd () in
  let i = ref 0 in
  String.iter (fun c -> if c = '/' then incr i else ()) cwd;
  cml_path_helper !i path

(* returns the absolute path to the nearest cml repository *)
let abs_cml_path () =
  let cwd = Sys.getcwd () in
  let path = match cml_path (cwd^"/") with
    | None -> raise (Fatal "Not a Cml repository (or any of the parent directories)")
    | Some s -> s
  in
  Sys.chdir path;
  let abs_path = Sys.getcwd () in
  Sys.chdir cwd;
  abs_path

(* returns true if the path contains an initialized Cml repo,
 * otherwise returns false *)
let cml_initialized (path : string) : bool =
  Sys.file_exists (path^".cml")

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise returns false *)
let cml_initialized_r path =
  match cml_path path with
  | Some _ -> true
  | None -> false

(* sets the cwd (current working directory) to the nearest .cml directory.
 * Raises Fatal exception if directory is not a Cml repository
 * (or any of the parent directories) *)
let chdir_to_cml () =
  match cml_path ((Sys.getcwd ())^"/") with
  | Some path -> Sys.chdir path
  | None -> raise (Fatal "Not a Cml repository (or any of the parent directories)")

(* returns [str] without [sub] *)
let remove_from_string str sub =
  Str.replace_first (Str.regexp sub) "" str

let current_dir () =
  Filename.basename (Sys.getcwd ())

(* returns the absolute path from the repository givent the relative path
 * from the cwd *)
let abs_path_from_cml rel_path =
  let path_to_cml = abs_cml_path () in
  let cwd = Sys.getcwd () in
  let rel_path_dirname =
    if Sys.is_directory rel_path then
      rel_path
    else
      Filename.dirname rel_path
  in
  let rel_path_filename = Filename.basename rel_path in
  Sys.chdir rel_path_dirname;
  let abs_path = Sys.getcwd () in
  Sys.chdir cwd;
  let final_path = remove_from_string abs_path path_to_cml in
  if Sys.is_directory rel_path then
    final_path
  else
    (final_path^"/"^rel_path_filename)

(* returns the relative path from the cwd to the given path relative to the
 * cml repo (essentially the input is the path in idx) *)
let get_rel_path idx_path =
  let rec add_back_string acc dir_path file_path =
    let path_regexp = Str.regexp ("^"^dir_path) in
    if Str.string_match path_regexp file_path 0 then
      (acc^(Str.replace_first path_regexp "" file_path))
    else begin
      try begin
        let i = Str.search_backward (Str.regexp "/.*/$") dir_path (String.length dir_path) in
        let dir_path' = Str.string_before dir_path (i+1) in
        add_back_string ("../"^acc) dir_path' file_path
      end with
        | Not_found -> ("../"^acc^file_path)
    end
  in
  let path_to_cml = match cml_path "" with
    | None -> raise (Fatal "Not a Cml repository (or any of the parent directories)")
    | Some s -> s
  in
  if path_to_cml = "" then
    idx_path
  else begin
    let dir_path_from_cml = (abs_path_from_cml "./")^"/"
      |> Str.replace_first (Str.regexp "^/") "" in
    idx_path |> add_back_string "" dir_path_from_cml
  end

(* returns whether the given argument is a flag (if arg is of the form
 * dash [-] followed by any number of characters > 0) *)
let arg_is_flag arg =
  let r = Str.regexp "^-.*" in
  Str.string_match r arg 0

(* precondition: [arg] is a flag.
 * postcondition: returns the list of flags from the argument.
 * example: [get_flags_from_arg "-hi" ~ ["h"; "i"]]
 * example: [get_flags_from_arg "--hi" ~ ["hi"]] *)
let get_flags_from_arg arg =
  let r_double_dash = Str.regexp "^--" in
  let r_single_dash = Str.regexp "^-" in
  if Str.string_match r_double_dash arg 0 then
    [Str.replace_first r_double_dash "" arg]
  else
    Str.replace_first r_single_dash "" arg |> Str.split (Str.regexp "")

(* recursively delete the empty directories in the [path] *)
let rec remove_empty_dirs path =
  let check_remove_empty files =
    match files with
    | [||] -> Unix.rmdir path
    | [|".DS_Store"|] -> Sys.remove (path^"/"^".DS_Store"); Unix.rmdir path
    | _ -> ()
  in
  let remove_empty_dirs_helper file =
    let file_path = path^"/"^file in
    if try Sys.is_directory file_path with _ -> false then
      remove_empty_dirs file_path
    else ()
  in
  let files = Sys.readdir path in
  Array.iter remove_empty_dirs_helper files;
  let files' = Sys.readdir path in
  check_remove_empty files'

(************************ Object Creation & Parsing  **************************)
(******************************************************************************)

(* creates a blob object for the given file. Returns the hash. *)
let create_blob (file_name: string) : string =
  let hsh = hash file_name in
  let (d1,path) = split_hash hsh in
  let _ =  if Sys.file_exists (".cml/objects/"^d1) then () else mkdir (".cml/objects/"^d1) perm in
  open_out path |> close_out; compress file_name path; hsh

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
  let hsh = hash temp_name in
  let (d1,path) = split_hash hsh in
  let _ = if Sys.file_exists (".cml/objects/"^d1) then () else mkdir (".cml/objects/"^d1) perm in
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

(* sets the HEAD of the cml repository to a commit hash *)
let set_detached_head (commit_hash : string) : unit =
  try
    let oc = open_out ".cml/HEAD" in
    output_string oc commit_hash; close_out oc
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
  if detached_head () then get_detached_head ()
  else get_head ()

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

(* overwrites file with version added to supplied index
 * if the file is not in the index, do nothing *)
let checkout_file (file_name : string) (idx : index) : unit =
  try
    let obj_path = get_object_path (List.assoc file_name idx) in
    decompress obj_path file_name
  with
    | Not_found -> ()

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

(* removes [rm_files] list from the index *)
let rm_files_from_idx rm_files =
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  let idx = get_index () in
  let idx' = List.filter (fun (s,_) -> not (List.mem s rm_files)) idx in
  set_index idx';
  Sys.chdir cwd

(* removes [rm_files] list from the repository (deletes physical files).
 * the files given must be the path from .cml repo *)
let rm_files_from_repo rm_files =
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  List.iter Sys.remove rm_files;
  Sys.chdir cwd

(* adds [add_files] list from the index *)
let add_files_to_idx add_files =
  let acc_idx acc file =
    let hsh = create_blob file in
    if List.mem (file,hsh) acc then
      acc
    else
      update_index (file,hsh) acc
  in
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  let idx = get_index () in
  let idx' = List.fold_left acc_idx idx add_files in
  set_index idx';
  Sys.chdir cwd

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

(* returns true if the file is an ignored file *)
let is_ignored_file ignored_files file =
  List.mem file ignored_files

(* returns a list of all files in working repo by absolute path *)
let rec get_all_files (dirs : string list) (acc : string list) : string list =
  let rec loop ignored dir_h path files directories =
    try
      let temp = readdir dir_h in
      if is_ignored_file ignored temp then
        loop ignored dir_h path files directories
      else begin
        let fn = (if path = "" || path = "./" then temp else (path^"/"^temp)) in
        if Sys.is_directory fn then
          loop ignored dir_h path files (fn::directories)
        else
          loop ignored dir_h path (fn::files) directories
      end
    with
      End_of_file -> closedir dir_h; (files, directories)
  in match dirs with
    | [] -> List.sort (Pervasives.compare) acc
    | dir_name::t ->
      begin
        if is_bad_dir dir_name then
          get_all_files t acc
        else
          let dir_h = opendir dir_name in
          let (files, directories) = loop [".DS_Store"] dir_h dir_name acc [] in
          get_all_files (t@directories) files
      end

(* returns a list of all files staged (added) for commit *)
(* precondition: all files have objects in [.cml/objects/] *)
let rec get_staged (idx : index) (commit_idx : index) : string list =
  let find_staged acc (f,h) =
    let hash = try List.assoc f commit_idx with Not_found -> "nil" in
    if hash = h then acc else f::acc
  in
  List.fold_left find_staged [] idx |> List.sort (Pervasives.compare)

(* returns a mapping of changed files to their old obj hashes *)
let get_changed_as_index (cwd : string list) (idx : index) : index =
  let find_changed acc fn =
    try
      let old_hash = List.assoc fn idx in
      let new_hash = hash fn in
      if old_hash = new_hash then acc else (fn,old_hash)::acc
    with
    | Not_found -> acc
  in
  List.fold_left find_changed [] cwd |> List.sort (Pervasives.compare)

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

(* returns a list of untracked files (i.e. not in the index) *)
let get_untracked (cwd : string list) (idx : index) : string list =
  let find_untracked acc fn =
    try
      let _ = List.assoc fn idx in acc
    with
    | Not_found -> fn::acc
  in
  List.fold_left find_untracked [] cwd |> List.sort (Pervasives.compare)

(******************************** Branching ***********************************)
(******************************************************************************)

(* validate the branch name *)
let validate_branch (branch : string) : unit =
  if branch.[0] = '.' || branch.[0] = '-' then
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

(* returns the head pointer of the branch *)
let get_branch branch =
  let branch_path = ".cml/heads/"^branch in
  if Sys.file_exists branch_path then
    try
      let ic = open_in branch_path in
      let head = input_line ic in
      close_in ic;
      head
    with
      | _ -> raise (Fatal ("Could not find branch '"^branch^"'"))
  else
    raise (Fatal ("Branch '"^branch^"' does not exist"))

(* returns the index of the branch *)
let get_branch_index branch =
  let commit = parse_commit (get_branch branch) in
  Tree.read_tree "" commit.tree |> Tree.tree_to_index

(* recursively creates branch sub-directories as needed *)
let rec branch_help (path : string) (branch : string) : out_channel =
  try
    let slash = String.index branch '/' in
    let dir = String.sub branch 0 slash in
    let prefix = path^dir in
    if not (Sys.file_exists prefix) then mkdir prefix perm;
    String.sub branch (slash+1) ((String.length branch) - (slash+1)) |>
      branch_help (prefix^"/")
  with
  | Not_found -> open_out (path^"/"^branch)

(* create a new branch if it doesn't exist *)
let create_branch (branch : string) (ptr : string) : unit =
  if (get_branches () |> List.mem branch) then
    raise (Fatal ("a branch named "^branch^" already exists"))
  else
    let _ = validate_branch branch in
    let ch = begin
      if String.contains branch '/' then
        branch_help ".cml/heads/" branch
      else
        open_out (".cml/heads/"^branch)
    end in
    let _ = output_string ch ptr in close_out ch

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
let switch_branch (branch : string) (isdetached : bool) : unit =
  let cur = if isdetached then "" else get_current_branch () in
  if cur = branch then
    print ("Already on branch '"^branch^"'")
  else
    let ch = open_out ".cml/HEAD" in
    output_string ch ("heads/"^branch); close_out ch;
    print ("Switched to branch '"^branch^"'")

(* switches state of repo to state of given commit_hash *)
let switch_version (commit_hash : string) : unit =
  let ohead = parse_commit (get_head ()) in
  let oindex = Tree.read_tree "" ohead.tree |> Tree.tree_to_index in
  let nhead = parse_commit commit_hash in
  let ntree = Tree.read_tree "" nhead.tree in
  let nindex = Tree.tree_to_index ntree in
  List.iter (fun (fn, hn) -> Sys.remove fn ) oindex;
  Tree.recreate_tree "" ntree;
  remove_empty_dirs "./";
  set_index nindex

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
let get_user_info () : string =
  try
    let ch = open_in ".cml/config" in
    let raw = input_line ch in
    let split = (String.index raw ' ') + 1 in
    close_in ch; String.sub raw split (String.length raw - split)
  with
  | Sys_error _ -> raise (Fatal "username not set, set with [--user <name>]")

(* set the user info (username) *)
let set_user_info (name : string) : unit =
  let ch = open_out ".cml/config" in
  output_string ch ("user: "^name^"\n"); close_out ch
