(***************************** Filesystem Module ******************************)
(******************************************************************************)

(******************************* Path Functions *******************************)

(* returns the option path to the nearest .cml directory from the cwd
 * (current working directory). *)
 val cml_path : string -> string option

(* returns the absolute path to the nearest cml repository *)
val abs_cml_path : unit -> string

(* returns the absolute path from the repository givent the relative path
 * from the cwd *)
val abs_path_from_cml : string -> string

(* returns the relative path from the cwd to the given path relative to the
 * cml repo (essentially the input is the path in idx) *)
val get_rel_path : string -> string

(* sets the cwd (current working directory) to the nearest .cml directory.
 * Raises Fatal exception if directory is not a Cml repository
 * (or any of the parent directories) *)
val chdir_to_cml : unit -> unit

(*************************** Fetching and Comparing ***************************)

(* returns true if dir is known link, or if is .cml *)
val is_bad_dir: string -> bool

(* returns a list of all files in working repo by absolute path *)
val get_all_files: string list -> string list -> string list

(* returns a list of all files in the current repo *)
val verify_files_in_repo: string list -> string list

(* returns a list of the file names in [rel_path] to cwd, (the returned
 * filenames are relative to cml repo) *)
val get_files_from_rel_path: string -> string list

(* returns a list of all files staged (added) for commit *)
val get_staged: Common.index -> Common.index -> string list

(* returns a mapping of changed files to their old obj hashes *)
val get_changed_as_index: string list -> Common.index -> Common.index

(* returns a list of changed files *)
val get_changed: string list -> Common.index -> string list

(* returns a list of untracked files *)
val get_untracked: string list -> Common.index -> string list

(* returns a list of all files in the index *)
val files_in_index: Common.index -> string list

(* returns a list of files that were deleted since last commit *)
val get_deleted: string list -> Common.index -> string list

(******************************** Manipulation ********************************)

(* removes [rm_files] list from the repository (deletes physical files).
 * the files given must be the path from .cml repo *)
val rm_files_from_repo : string list -> unit

(* recursively delete the empty directories in the cwd *)
val remove_empty_dirs : string -> unit

(* overwrites file with version added to supplied index
 * if the file is not in the index, do nothing *)
val checkout_file: string -> Common.index -> unit
