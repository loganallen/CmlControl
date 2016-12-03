open Unix
open Common
open Tree

(******************************** Index Module ********************************)
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
let rm_files_from_idx (rm_files : string list) : unit =
  let cwd = Sys.getcwd () in Filesystem.chdir_to_cml ();
  let idx = get_index () in
  let idx' = List.filter (fun (s,_) -> not (List.mem s rm_files)) idx in
  set_index idx'; Sys.chdir cwd

(* adds [add_files] list from the index *)
let add_files_to_idx (add_files : string list) : unit =
  let acc_idx acc file =
    let hsh = Object.create_blob file in
    if List.mem (file,hsh) acc then acc else update_index (file,hsh) acc
  in
  let cwd = Sys.getcwd () in Filesystem.chdir_to_cml ();
  let idx = get_index () in
  let idx' = List.fold_left acc_idx idx add_files in
  set_index idx'; Sys.chdir cwd

(* switches state of repo to state of given commit_hash *)
let switch_version (commit_hash : string) : unit =
  let ohead = Object.parse_commit (Head.get_head ()) in
  let oindex = Tree.read_tree "" ohead.tree |> Tree.tree_to_index in
  let nhead = Object.parse_commit commit_hash in
  let ntree = Tree.read_tree "" nhead.tree in
  let nindex = Tree.tree_to_index ntree in
  List.iter (fun (fn, hn) -> Sys.remove fn ) oindex;
  Tree.recreate_tree "" ntree;
  Filesystem.remove_empty_dirs "./";
  set_index nindex
