(******************************** Index Module ********************************)
(******************************************************************************)

(* returns the index which is a list that maps tracked filenames
 * to their most recent hash string value *)
val get_index: unit -> Universal.index

(* returns the index of the branch *)
val get_branch_index: string -> Universal.index

(* updates the index by adding a new mapping *)
val update_index: string * string -> Universal.index -> Universal.index

(* initializes an index in the cml directory *)
val set_index: Universal.index -> unit

(* removes [rm_files] list from the index *)
val rm_files_from_idx : string list -> unit

(* adds [add_files] list from the index *)
val add_files_to_idx : string list -> unit

(* switches state of repo to state of given commit_hash *)
val switch_version: bool -> string -> unit
