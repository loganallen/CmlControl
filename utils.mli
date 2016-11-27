(* Utils Module --- helper functions for directory data structure *)


(* a record representing all the information about a commit type.
 * note: pointers in this case are string file names.
 *)
type commit = {
  author: string;
  message: string;
  tree: string;
  parent: string;
}

(* type blob represents a compressed file object *)
type blob = string

(* type index is a list of mappings from filename to its hash,
 * referred to as the index in git *)
type index = (string * string) list

(* Fatal exception for internal cml execution errors *)
exception Fatal of string

(***************************** Generic Helpers ********************************)
(******************************************************************************)

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise raises an exception *)
val cml_initialized: string -> bool

(************************* File Compression & Hashing *************************)
(******************************************************************************)

(* hash returns a SHA-1 hash of a given input *)
val hash : string -> string

(* copy creates copy of a file in a new destination *)
val copy: string -> string -> unit

(* compress compresses a file/directory
 * takes initial path and final path as arguments.
 *)
val compress: string -> string -> unit

(* decompress decompresses a file/directory
 * takes initial and final path as arguments.
 *)
val decompress: string -> string -> unit

(* creates a blob object for the given file. Returns the hash. *)
val create_blob: string -> string

(* creates a commit object for the given commit. Returns the hash. *)
val create_commit: string -> string -> string -> string -> string

(* returns a commit record for the given commit ptr *)
val parse_commit: string -> commit

(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

(* returns the current HEAD of the cml repository *)
val get_head: unit -> string

(* sets the HEAD of the cml repository *)
val set_head: string -> unit

(* returns the HASH of a head of the given branch *)
val get_branch_ptr: string -> string

(* initializes a given commit to a given branch name *)
val set_branch_ptr: string -> string -> unit

(* returns a list of all versions of HEAD *)
val get_versions: unit -> string list

(* go to an old version of HEAD *)
(* precondition: [version] of HEAD exists *)
val switch_version: string -> unit

(***************************** Index Manipulation *****************************)
(******************************************************************************)

(* returns the index which is a list that maps tracked filenames
 * to their most recent hash string value *)
val get_index: unit -> index

(* updates the index by adding a new mapping *)
val update_index: string * string -> index -> index

(* initializes an index in the cml directory *)
val set_index: index -> unit

(****************************** File Fetching *********************************)
(******************************************************************************)

(* returns true if dir is known link, or if is .cml *)
val is_bad_dir: string -> bool

(* returns a list of all files in working repo by absolute path *)
val get_all_files: string list -> string list -> string list

(* returns a list of all files staged (added) for commit *)
val get_staged: index -> string list

(* returns a list of changed files *)
val get_changed: string list -> index -> string list

(* returns a list of untracked files *)
val get_untracked: string list -> index -> string list

(******************************** Branching ***********************************)
(******************************************************************************)

(* validate the branch name *)
val validate_branch: string -> unit

(* returns a list of all branches *)
val get_branches: unit -> string list

(* returns string representation of repo's current branch *)
val get_current_branch: unit -> string

(* recursively creates branch sub-directories as needed *)
val branch_help: string -> string -> unit

(* create a new branch if it doesn't exist *)
val create_branch: string -> unit

(* delete a branch if it exists *)
val delete_branch: string -> unit

(* switch current working branch *)
(* precondition: [branch] exists *)
val switch_branch: string -> unit

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
val get_user_info: unit -> string

(* set the user info (username) *)
val set_user_info: string -> unit
