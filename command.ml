open Utils
open Print
open Unix

type command =  | Add | Branch | Checkout | Commit | Diff | Help | Init | Merge
                | Rm | Stash | Status

type input = { command: command; arg: string; flags: string list}

exception Fatal of string

(* parses a string and returns an input type *)
let parse_input (args : string array) : input =
	match args.(1) with
		| "init" -> { command = Init; arg = ""; flags = [] }
		| _ -> raise (Fatal (args.(1) ^ ": command unimplemented"))

(* add file contents to the index. *)
let add (input : input) : unit =
  failwith "Unimplemented"

(* creates a new branch *)
let branch (branch_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* checkout Switch branches or restore working tree files. *)
let checkout (branch_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* commit Record changes to the repository.
 * Stores the current contents of the index in a new commit
 * along with a log message from the user describing the changes. *)
let commit (msg : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* diff Show changes between commits, commit and working tree, etc *)
let diff (commit : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* Display help information about CmlControl. *)
let help (options : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* init Create an empty CmlControl repository. *)
let init (flags : string list) : unit =
	try
		let handle = opendir ".cml" in
		let _ = closedir handle in
		raise (Fatal "Cannot init repo, already initialized")
	with
		Unix_error (Unix.ENOENT,_,_) -> mkdir ".cml" 0o777

(* merge Join two or more development histories together. *)
let merge (merge_branch : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* Remove files from working tree and from the index. *)
let rm (file_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* stash Stash the changes in a dirty working directory away. *)
let stash (options : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* displays any changes to the working directory *)
let status (options : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* executes a command *)
let execute (arg : input) : unit =
	match arg.command with
		| Init  -> init []
		| _ -> print_error "command not found" ""
