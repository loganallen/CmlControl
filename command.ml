open Utils
open Print
open Unix

type command =  | Add | Branch | Checkout | Commit | Diff | Help | Init | Merge
                | Rm | Stash | Status

type input = { command: command; arg: string; flags: string list}

exception Fatal of string

let perm = 0o777

(* parses a string and returns an input type *)
let parse_input (args : string array) : input =
  if Array.length args = 0 then raise (Fatal "no command given, try 'cml help'") else
	match args.(0) with
		| "add"       -> { command = Add; arg = ""; flags = [] }
		| "branch"    -> { command = Branch; arg = ""; flags = [] }
		| "checkout"  -> { command = Checkout; arg = ""; flags = [] }
		| "commit"    -> { command = Commit; arg = ""; flags = [] }
		| "diff"      -> { command = Diff; arg = ""; flags = [] }
		| "help"      -> { command = Help; arg = ""; flags = [] }
		| "init"      -> { command = Init; arg = ""; flags = [] }
		| "merge"     -> { command = Merge; arg = ""; flags = [] }
		| "rm"        -> { command = Rm; arg = ""; flags = [] }
		| "stash"     -> { command = Stash; arg = ""; flags = [] }
		| "status"    -> { command = Status; arg = ""; flags = [] }
		| _           -> raise (Fatal (args.(0) ^ ": invalid command"))

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
		Unix_error (Unix.ENOENT,_,_) ->
			begin
				mkdir ".cml" perm; chdir ".cml";
				mkdir "heads" perm; mkdir "objects" perm;
				let f = openfile "HEAD" [O_CREAT] perm in
				close f
			end

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
		| _ -> print_error "command unimplemented" ""
