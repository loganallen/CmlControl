(* prints a diff between two files to console *)
val diff_file : string -> string -> unit

(* prints diffs all pairs of files in a list *)
val diff_mult : (string * string) list -> unit
