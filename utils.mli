(* Utils Module --- helper functions for directory data structure *)


(* a record representing all the information about a commit type.
 * note: pointers in this case are string file names.
 *)
type commit = {
  tree: string;
  author: string;
  date: string;
  message: string;
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

(* returns the option path to the nearest .cml directory from the cwd
 * (current working directory). *)
 val cml_path : string -> string option

(* returns the absolute path to the nearest cml repository *)
val abs_cml_path : unit -> string

(* returns true if the path contains an initialized Cml repo,
 * otherwise returns false *)
val cml_initialized: string -> bool

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise returns false *)
val cml_initialized_r : string -> bool

(* ($) is an infix operator for appending a char to a string *)
val ($): string -> char -> string

(* sets the cwd (current working directory) to the nearest .cml directory.
 * Raises Fatal exception if directory is not a Cml repository
 * (or any of the parent directories) *)
val chdir_to_cml : unit -> unit

(* returns the absolute path from the repository givent the relative path
 * from the cwd *)
val abs_path_from_cml : string -> string

(* returns [str] without [sub] *)
val remove_from_string : string -> string -> string

(* returns the path of an object represented by hash
 * precondition: hash is a valid  40 char string
 * postcondition: get_object_path raise Fatal if hash doens't exist *)
 val get_object_path : string -> string

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
val create_commit: string -> string -> string -> string -> string -> string

(* returns a commit record for the given commit ptr *)
val parse_commit: string -> commit

(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

(* returns the current HEAD of the cml repository *)
val get_head: unit -> string

(* sets the HEAD of the cml repository *)
val set_head: string -> unit

(* sets the HEAD of the cml repository to a commit hash *)
val set_detached_head: string -> unit

(* returns the commit hash the head was detached at *)
val get_detached_head: unit -> string

(* returns true if repo currently is in detached head mode, else false *)
val detached_head: unit -> bool

(* returns the HASH of a head of the given branch *)
val get_branch_ptr: string -> string

(* initializes a given commit to a given branch name *)
val set_branch_ptr: string -> string -> unit

(* overwrites file with version added to supplied index
 * if the file is not in the index, do nothing *)
val checkout_file: string -> index -> unit

(***************************** Index Manipulation *****************************)
(******************************************************************************)

(* returns the index which is a list that maps tracked filenames
 * to their most recent hash string value *)
val get_index: unit -> index

(* updates the index by adding a new mapping *)
val update_index: string * string -> index -> index

(* initializes an index in the cml directory *)
val set_index: index -> unit

(* removes [rm_files] list from the index *)
val rm_files_from_idx : string list -> string list -> unit

(* adds [add_files] list from the index *)
val add_files_to_idx : string list -> unit

(****************************** File Fetching *********************************)
(******************************************************************************)

(* returns true if dir is known link, or if is .cml *)
val is_bad_dir: string -> bool

(* returns a list of all files in working repo by absolute path *)
val get_all_files: string list -> string list -> string list

(* returns a list of all files staged (added) for commit *)
val get_staged: index -> index -> string list

(* returns a mapping of changed files to their old obj hashes *)
val get_changed_as_index: string list -> index -> index

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

(* create a new branch at the specified ptr if it doesn't exist *)
val create_branch: string -> string -> unit

(* delete a branch if it exists *)
val delete_branch: string -> unit

(* switch current working branch *)
(* precondition: [branch] exists *)
val switch_branch: string -> bool -> unit

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
val get_user_info: unit -> string

(* set the user info (username) *)
val set_user_info: string -> unit
