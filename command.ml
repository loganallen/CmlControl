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
  if Sys.file_exists ".cml" then
    raise (Fatal "Cml repository already initialized")
  else
		mkdir ".cml" perm; mkdir ".cml/heads" perm; mkdir ".cml/objects" perm;
		let out = open_out ".cml/HEAD" in
		output_string out "heads/master"; close_out out;
    print_color "initialized empty Cml repository" "green"; print_camel ()

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
  try
	  match arg.command with
		| Init  -> init []
	  | _ -> print_error "command unimplemented" ""
  with
  | Fatal msg -> print ("fatal: "^msg)
