(******************************* Files Module *********************************)
(******************************************************************************)

(* returns true if dir is known link, or if is .cml *)
val is_bad_dir: string -> bool

(* returns a list of all files in working repo by absolute path *)
val get_all_files: string list -> string list -> string list

(* returns a list of all files staged (added) for commit *)
val get_staged: Common.index -> Common.index -> string list

(* returns a mapping of changed files to their old obj hashes *)
val get_changed_as_index: string list -> Common.index -> Common.index

(* returns a list of changed files *)
val get_changed: string list -> Common.index -> string list

(* returns a list of untracked files *)
val get_untracked: string list -> Common.index -> string list
