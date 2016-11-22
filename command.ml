open Utils
open Print
open Unix

type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status

type input = { command: command; arg: string; flags: string list}

exception Fatal of string
exception Unrecognized of string

let perm = 0o777

(* add file contents to the index *)
let add (input : input) : unit =
  failwith "Unimplemented"

(* list, create, or delete branches *)
let branch (branch_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* switch branches or restore working tree files *)
let checkout (branch_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* record changes to the repository:
 * stores the current contents of the index in a new commit
 * along with commit metadata. *)
let commit (msg : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* show changes between working tree and previous commits *)
let diff (commit : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* display help information about CmlControl. *)
let help () : unit =
  print_help ()

(* init Create an empty CmlControl repository. *)
let init (flags : string list) : unit =
  if cml_initialized "./" then
    raise (Fatal "Cml repository already initialized")
  else
		mkdir ".cml" perm; mkdir ".cml/heads" perm; mkdir ".cml/objects" perm;
		let out = open_out ".cml/HEAD" in
		output_string out "heads/master"; close_out out;
    print_color "initialized empty Cml repository" "green"; print_camel ()

(* display the current branches commit history *)
let log () : unit =
  failwith "Unimplemented"

(* join two or more development histories together *)
let merge (merge_branch : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* remove files from working tree and index *)
let rm (file_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* stashes changes made to the current working tree *)
let stash (options : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* show the working tree status *)
let status (options : string) (flags : string list) : unit =
  if not (cml_initialized "./") then
    raise (Fatal "Not a cml repository (or any of the parent directories)")
  else
    print ("On branch " ^ (get_current_branch ()));
    let cwd = get_all_files ["./"] [] in
    let idx = get_index () in
    let st = get_staged idx in
    let ch = get_changed cwd idx in
    let ut = get_untracked cwd idx in
      match (st,ch,ut) with
      | [],[],[] -> print "no changes to be committed, working tree clean"
      | _ -> let _ = print_staged st in
             let _ = print_changed ch in print_untracked ut

(* parses bash string input and returns a Cml input type *)
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
    | "log"       -> { command = Log; arg = ""; flags = [] }
    | "merge"     -> { command = Merge; arg = ""; flags = [] }
    | "reset"     -> { command = Reset; arg = ""; flags = [] }
    | "rm"        -> { command = Rm; arg = ""; flags = [] }
    | "stash"     -> { command = Stash; arg = ""; flags = [] }
    | "status"    -> { command = Status; arg = ""; flags = [] }
    | _           -> raise (Fatal (args.(0) ^ ": invalid command"))

(* executes a Cml command *)
let execute (arg : input) : unit =
  try
	  match arg.command with
		| Init   -> init []
    | Status -> status "" []
    | Help   -> help ()
    | Log    -> log ()
	  | _ -> print_error "command unimplemented" ""
  with
  | Fatal msg -> print ("fatal: "^msg)
  | Unrecognized cmd -> print ("cml: '"^cmd^"' is not a cml command. See git help.")
