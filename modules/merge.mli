(******************************** Merge Module ********************************)
(******************************************************************************)

(* returns the commit history for the given commit hash pointer *)
val get_commit_history: string -> string list

(* perform a true merge if there are no merge conflicts by creating
 * a new commit that combines the states of the two branches *)
val true_merge: string -> string -> string -> unit

(* perform fast-forward merge by updating the head to the branch head *)
val fast_forward_merge: string -> string -> unit
