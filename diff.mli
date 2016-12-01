(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
val diff : (string *  bool) -> (string * bool) -> unit

(* prints diffs for all pairs of files in a list [lst] *)
val diff_mult : ((string * bool) * (string * bool)) list -> unit