open Unix
open Common

(******************************* Files Module *********************************)
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
let get_staged (idx : index) (commit_idx : index) : string list =
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
      let new_hash = Crypto.hash fn in
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
      let new_hash = Crypto.hash fn in
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
