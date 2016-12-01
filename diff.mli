(* prints a diff between two files to console *)
val diff_file : string -> string -> unit

(* prints diffs all pairs of files in a list *)
val diff_mult : ((string * bool) * (string * bool)) list -> unit

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
val diff : (string *  bool) -> (string * bool) -> unit
