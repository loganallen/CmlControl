(****************************** Branch Module *********************************)
(******************************************************************************)

(* returns a list of all branches *)
val get_branches: unit -> string list

(* returns string representation of repo's current branch *)
val get_current_branch: unit -> string

(* returns the HASH of a head of the given branch *)
val get_branch_ptr: string -> string

(* returns the head pointer of the branch *)
val get_branch: string -> string

(* initializes a given commit to a given branch name *)
val set_branch_ptr: string -> string -> unit

(* create a new branch at the specified ptr if it doesn't exist *)
val create_branch: string -> string -> unit

(* delete a branch if it exists *)
val delete_branch: string -> unit

(* switch current working branch *)
(* precondition: [branch] exists *)
val switch_branch: string -> bool -> unit
