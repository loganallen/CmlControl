(******************************** Index Module ********************************)
(******************************************************************************)

(* returns the index which is a list that maps tracked filenames
 * to their most recent hash string value *)
val get_index: unit -> Common.index

(* updates the index by adding a new mapping *)
val update_index: string * string -> Common.index -> Common.index

(* initializes an index in the cml directory *)
val set_index: Common.index -> unit

(* removes [rm_files] list from the index *)
val rm_files_from_idx : string list -> unit

(* adds [add_files] list from the index *)
val add_files_to_idx : string list -> unit

(* switches state of repo to state of given commit_hash *)
val switch_version: string -> unit
