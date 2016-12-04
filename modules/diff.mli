type file_data = {
  file_path : string;
  compressed : bool;
}

type diff_input = {
  name : string;
  old_file_data : file_data option;
  new_file_data : file_data option;
}

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
val diff : file_data -> file_data -> Odiff.diffs

(* prints diffs for all pairs of files in a list [lst] *)
val diff_mult : diff_input list -> unit

(* Convert an index into an associate list with file_data *)
val index_to_diff_index : bool -> Universal.index -> (string * file_data) list

(* Print the diff of two diff input lists *)
val diff_indexes : (string * file_data) list -> (string * file_data) list -> unit

(* precondition: cwd is cml repo root *)
val get_diff_current_index: unit -> (string * file_data) list

(* precondition: [abs_path_lst] holds the absolute paths from cml.
 * Also, all the files are uncompressed *)
val diff_idx_current_files: string list -> (string * file_data) list

(* return the diff index of the cmt_idx for each file in [files] *)
val diff_idx_commit: Universal.index -> string list -> (string * file_data) list
