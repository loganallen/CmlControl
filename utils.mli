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

(* sets the cwd (current working directory) to the nearest .cml directory.
 * Raises Fatal exception if directory is not a Cml repository
 * (or any of the parent directories) *)
val chdir_to_cml : unit -> unit

(* returns the absolute path from the repository givent the relative path
 * from the cwd *)
val abs_path_from_cml : string -> string

(* returns the relative path from the cwd to the given path relative to the
 * cml repo (essentially the input is the path in idx) *)
val get_rel_path : string -> string

(* returns [str] without [sub] *)
val remove_from_string : string -> string -> string

(* returns whether the given argument is a flag (if arg is of the form
 * dash [-] followed by any number of characters > 0) *)
val arg_is_flag : string -> bool

(* precondition: [arg] is a flag.
 * postcondition: returns the list of flags from the argument.
 * example: [get_flags_from_arg "-hi" ~ ["h"; "i"]]
 * example: [get_flags_from_arg "--hi" ~ ["hi"]] *)
val get_flags_from_arg : string -> string list

(* recursively delete the empty directories in the cwd *)
val remove_empty_dirs : string -> unit

(************************ Object Creation & Parsing  **************************)
(******************************************************************************)

(* creates a blob object for the given file. Returns the hash. *)
val create_blob: string -> string

(* creates a commit object for the given commit. Returns the hash. *)
val create_commit: string -> string -> string -> string -> string -> string

(* returns a commit record for the given commit ptr *)
val parse_commit: string -> commit

(* takes a commit hash and returns  the index of the commit *)
val get_commit_index: string -> index

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

(* returns correct head depending on detached_head mode *)
val get_head_safe : unit -> string

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
val rm_files_from_idx : string list -> unit

(* removes [rm_files] list from the repository (deletes physical files).
 * the files given must be the path from .cml repo *)
val rm_files_from_repo : string list -> unit

(* adds [add_files] list from the index *)
val add_files_to_idx : string list -> unit

(* creates an index from the given files *)


(****************************** File Fetching *********************************)
(******************************************************************************)

(* returns true if dir is known link, or if is .cml *)
val is_bad_dir: string -> bool

(* returns true if the file is an ignored file *)
val is_ignored_file : string list -> string -> bool

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

(* returns the head pointer of the branch *)
val get_branch : string -> string

(* returns the index of the branch *)
val get_branch_index : string -> index

(* create a new branch at the specified ptr if it doesn't exist *)
val create_branch: string -> string -> unit

(* delete a branch if it exists *)
val delete_branch: string -> unit

(* switch current working branch *)
(* precondition: [branch] exists *)
val switch_branch: string -> bool -> unit

(* switches state of repo to state of given commit_hash *)
val switch_version: string -> unit

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
val get_user_info: unit -> string

(* set the user info (username) *)
val set_user_info: string -> unit
