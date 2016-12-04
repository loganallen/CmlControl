(******************************* Object Module ********************************)
(******************************************************************************)

(* creates a blob object for the given file. Returns the hash. *)
val create_blob: string -> string

(* creates a commit object for the given commit. Returns the hash. *)
val create_commit: string -> string -> string -> string -> string list -> string

(* returns a commit record for the given commit ptr *)
val parse_commit: string -> Universal.commit

(* takes a commit hash and returns  the index of the commit *)
val get_commit_index: string -> Universal.index
