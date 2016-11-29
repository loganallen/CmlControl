(* prints a diff between two files to console *)
val print_diff_files : string -> string -> unit

(* prints diffs all pairs of files in a list *)
val print_diff_files_mult : (string * string) list -> unit
