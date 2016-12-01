(* prints the diffs made from two files *)
val diff_file : string -> string -> unit

<<<<<<< HEAD
(* prints diffs all pairs of files in a list *)
val diff_mult : ((string * bool) * (string * bool)) list -> unit

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
val diff : (string *  bool) -> (string * bool) -> unit
=======
(* prints diffs for all pairs of files in a list [lst] *)
val diff_mult : (string * string) list -> unit
>>>>>>> 22050bb... spec changes
