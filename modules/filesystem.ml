open Unix
open Universal
open Tree

(***************************** Filesystem Module ******************************)
(******************************************************************************)

(******************************* Path Functions *******************************)

(* returns the option path to the nearest .cml directory from the cwd
 * (current working directory). *)
let cml_path (path : string) : string option =
  let rec cml_path_helper i acc =
    if i = 0 then
      raise (Fatal "Not a Cml repository (or any of the parent directories)")
    else
      if Sys.file_exists (acc^".cml") then Some acc
      else cml_path_helper (i-1) (acc^"../")
  in
  let cwd = Sys.getcwd () in
  let i = ref 0 in
  String.iter (fun c -> if c = '/' then incr i else ()) cwd;
  cml_path_helper !i path

(* returns the absolute path to the nearest cml repository *)
let abs_cml_path () : string =
  let cwd = Sys.getcwd () in
  let path = match cml_path (cwd^"/") with
    | None -> raise (Fatal "Not a Cml repository (or any of the parent directories)")
    | Some s -> s
  in
  Sys.chdir path;
  let abs_path = Sys.getcwd () in Sys.chdir cwd; abs_path

(* returns the absolute path from the repository givent the relative path
 * from the cwd *)
let abs_path_from_cml (rel_path : string) : string =
  let path_to_cml = abs_cml_path () in
  let cwd = Sys.getcwd () in
  let rel_path_dirname =
    if Sys.is_directory rel_path then rel_path else Filename.dirname rel_path
  in
  let rel_path_filename = Filename.basename rel_path in
    Sys.chdir rel_path_dirname;
  let abs_path = Sys.getcwd () in Sys.chdir cwd;
  let final_path = abs_path |> Str.replace_first (Str.regexp path_to_cml) "" in
    if Sys.is_directory rel_path then final_path
    else (final_path^"/"^rel_path_filename)

(* returns the relative path from the cwd to the given path relative to the
 * cml repo (essentially the input is the path in idx) *)
let get_rel_path (idx_path : string) : string =
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
  if path_to_cml = "" then idx_path
  else begin
    let dir_path_from_cml = (abs_path_from_cml "./")^"/"
      |> Str.replace_first (Str.regexp "^/") "" in
    idx_path |> add_back_string "" dir_path_from_cml
  end

(* sets the cwd (current working directory) to the nearest .cml directory.
 * Raises Fatal exception if directory is not a Cml repository
 * (or any of the parent directories) *)
let chdir_to_cml () : unit =
  match cml_path ((Sys.getcwd ())^"/") with
  | Some path -> Sys.chdir path
  | None -> raise (Fatal "Not a Cml repository (or any of the parent directories)")

(*************************** Fetching and Comparing ***************************)

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
let is_ignored_file (ignored_files : string list) (file : string) : bool =
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
        if is_bad_dir dir_name then get_all_files t acc
        else
          let dir_h = opendir dir_name in
          let (files, directories) = loop [".DS_Store"] dir_h dir_name acc [] in
          get_all_files (t@directories) files
      end

(* returns a list of all files in the current repo *)
let verify_files_in_repo (files : string list) : string list =
  let filter file =
    if not (Sys.file_exists file) then
      raise (Fatal ("pathspec '"^file^"' is outside the repository"))
    else true
  in
  let cwd = Sys.getcwd () in chdir_to_cml ();
  let filtered_files = List.filter filter files in
  Sys.chdir cwd; filtered_files

(* returns a list of the file names in [rel_path] to cwd, (the returned
 * filenames are relative to cml repo) *)
let get_files_from_rel_path (rel_path : string) : string list =
  let path_from_cml = abs_path_from_cml rel_path in
  begin
    if Sys.is_directory rel_path then
      let rel_path' = begin
        if Str.string_match (Str.regexp ".*/$") rel_path 0 then
          rel_path
        else
          rel_path^"/"
      end in
      get_all_files [rel_path'] []
      |> List.map (fun s ->
        let name = Str.replace_first (Str.regexp "^/") "" (rel_path')
          |> Str.global_replace (Str.regexp "\\.") "\\." in
        Str.replace_first (Str.regexp name) "" s)
      |> List.map (fun s -> (path_from_cml^"/"^s))
      |> List.map (fun s -> Str.global_replace (Str.regexp "\\.\\./") "" s)
      |> List.map (fun s -> Str.global_replace (Str.regexp "\\./") "" s)
    else
      [path_from_cml]
  end |> List.map (fun s -> Str.replace_first (Str.regexp "//") "/" s)
      |> List.map (fun s -> Str.replace_first (Str.regexp "^/") "" s)
      |> verify_files_in_repo

(* returns a list of all files staged (added) for commit *)
(* precondition: all files have objects in [.cml/objects/] *)
let get_staged_help (idx : index) (commit_idx : index) : string list =
  let find_staged acc (f,h) =
    let hash = try List.assoc f commit_idx with Not_found -> "nil" in
    if hash = h then acc else f::acc
  in
  idx |> List.fold_left find_staged [] |> List.sort (Pervasives.compare)

(* helper function that returns a list of files staged for commit *)
let get_staged (idx: index) : string list =
  try
    let cmt = Head.get_head_safe () |> Object.parse_commit in
    cmt.tree |> Tree.read_tree "" |> Tree.tree_to_index |> get_staged_help idx
  with
  | Fatal _ -> get_staged_help idx []

(* returns a mapping of changed files to their old obj hashes *)
let get_changed_as_index (cwd : string list) (idx : index) : index =
  let find_changed acc fn =
    try
      let old_hash = List.assoc fn idx in
      let new_hash = Crypto.hash fn in
      if old_hash = new_hash then acc else (fn,old_hash)::acc
    with
    | Not_found -> acc
  in
  cwd |> List.fold_left find_changed [] |> List.sort (Pervasives.compare)

(* returns a list of changed files (different from the working index) *)
let get_changed (cwd : string list) (idx : index) : string list =
  let find_changed acc fn =
    try
      let old_hash = List.assoc fn idx in
      let new_hash = Crypto.hash fn in
      if old_hash = new_hash then acc else fn::acc
    with
    | Not_found -> acc
  in
  cwd |> List.fold_left find_changed [] |> List.sort (Pervasives.compare)

(* returns a list of untracked files (i.e. not in the index) *)
let get_untracked (cwd : string list) (idx : index) : string list =
  let find_untracked acc fn =
    try let _ = List.assoc fn idx in acc with Not_found -> fn::acc
  in
  cwd |> List.fold_left find_untracked [] |> List.sort (Pervasives.compare)

(* returns a list of all files in the index *)
let files_in_index (idx : index) : string list =
  List.map (fun (file,_) -> file) idx

(* returns a list of files that were deleted since last commit *)
let get_deleted (cwd_files : string list) (idx : index) : string list =
  let idx_files = files_in_index idx in
  try
    let cmt = Head.get_head_safe () |> Object.parse_commit in
    cmt.tree |> Tree.read_tree "" |> Tree.tree_to_index |> files_in_index
    |> List.filter (fun file -> (not (List.mem file idx_files)) || (not (List.mem file cwd_files)))
    |> List.sort Pervasives.compare
  with
  | Fatal _ -> []

(******************************** Manipulation ********************************)

(* removes [rm_files] list from the repository (deletes physical files).
 * the files given must be the path from .cml repo *)
let rm_files_from_repo rm_files =
  let cwd = Sys.getcwd () in
  chdir_to_cml (); List.iter Sys.remove rm_files; Sys.chdir cwd

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

(* overwrites file with version added to supplied index
 * if the file is not in the index, do nothing *)
let checkout_file (file_name : string) (idx : index) : unit =
  try
    let obj_path = get_object_path (List.assoc file_name idx) in
    Crypto.decompress obj_path file_name
  with
    | Not_found -> ()
