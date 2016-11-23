open Utils
open Print
open Unix

type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status | User

type input = { command: command; arg: string; flags: string list}

exception Parsing of string

let perm = 0o777

(* add file contents to the index *)
let add (file_name : string) (flags: string list) : unit =
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
    print_color "initialized empty Cml repository" "b"

(* display the current branches commit history *)
let log () : unit =
  failwith "Unimplemented"

(* join two or more development histories together *)
let merge (merge_branch : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* reset the current HEAD to a specified state *)
let reset (file_name : string) (flags: string list) : unit =
  failwith "Unimplemented"

(* remove files from working tree and index *)
let rm (file_name : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* stashes changes made to the current working tree *)
let stash (options : string) (flags : string list) : unit =
  failwith "Unimplemented"

(* show the working tree status *)
let status (options : string) (flags : string list) : unit =
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

(* set the user info to [username] *)
let user (username : string) : unit =
  if username = "" then
    let name = get_user_info () in print ("Current user: "^name)
  else set_user_info username

(* parses bash string input and returns a Cml input type *)
let parse_input (args : string array) : input =
  if Array.length args = 0 then raise (Fatal "no command given, see [--help]") else
  match args.(0) with
    | "add"      -> { command = Add; arg = ""; flags = [] }
    | "branch"   -> { command = Branch; arg = ""; flags = [] }
    | "checkout" -> { command = Checkout; arg = ""; flags = [] }
    | "commit"   -> { command = Commit; arg = ""; flags = [] }
    | "diff"     -> { command = Diff; arg = ""; flags = [] }
    | "init"     -> { command = Init; arg = ""; flags = [] }
    | "log"      -> { command = Log; arg = ""; flags = [] }
    | "merge"    -> { command = Merge; arg = ""; flags = [] }
    | "reset"    -> { command = Reset; arg = ""; flags = [] }
    | "rm"       -> { command = Rm; arg = ""; flags = [] }
    | "stash"    -> { command = Stash; arg = ""; flags = [] }
    | "status"   -> { command = Status; arg = ""; flags = [] }
    | "--help"   -> { command = Help; arg = ""; flags = [] }
    | "--user"   -> begin
        try
          let name = args.(1) in { command = User; arg = name; flags = [] }
        with
        | Invalid_argument _ -> { command = User; arg = ""; flags = [] }
      end
    | cmd        -> raise (Parsing cmd)

(* executes a Cml command *)
let execute (i : input) : unit =
  try
    if not (i.command = Init || i.command = Help) && not (cml_initialized "./") then
      raise (Fatal "Not a Cml repository (or any of the parent directories)")
    else
      match i.command with
      | Add      -> add "" []
      | Branch   -> branch "" []
      | Checkout -> checkout "" []
      | Commit   -> commit "" []
      | Diff     -> diff "" []
      | Help     -> help ()
  		| Init     -> init []
      | Log      -> log ()
      | Merge    -> merge "" []
      | Reset    -> reset "" []
      | Rm       -> rm "" []
      | Stash    -> stash "" []
      | Status   -> status "" []
      | User     -> user i.arg
  with
  | Fatal msg -> print ("fatal: "^msg)
