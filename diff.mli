type file_data = {
  file_path : string;
  compressed : bool;
}

type diff_input = {
  name : string;
  old_file_data : file_data option;
  new_file_data : file_data option;
}

(* Convert an index into an associate list with file_data *)
val index_to_diff_index : bool -> Utils.index -> (string * file_data) list

(* Print the diff of two diff input lists *)
val diff_indexes : (string * file_data) list -> (string * file_data) list -> unit

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
val diff : file_data -> file_data -> Odiff.diffs

(* prints diffs for all pairs of files in a list [lst] *)
val diff_mult : diff_input list -> unit
