(******************************** Merge Module ********************************)
(******************************************************************************)

(* returns a commit history that is the merge of two histories *)
val merge_histories: string list -> string list -> string list

(* recursivley builds the commit history starting from a specified hash ptr *)
val get_commit_history: string list -> string list -> string -> string list

(* returns the commit ptr of the common ancestor between two branches
 * and the next commit for the branch *)
val get_common_ancestor: string -> string -> string

(* perform a true merge if there are no merge conflicts by creating
 * a new commit that combines the states of the two branches *)
val true_merge: string -> string -> string -> unit

(* perform fast-forward merge by updating the head to the branch head *)
val fast_forward_merge: string -> string -> unit
