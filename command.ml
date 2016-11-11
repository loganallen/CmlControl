open Utils
open Print

type command =  | Add | Branch | Checkout | Commit | Diff | Help | Init | Merge
                | Rm | Stash | Status

type input = { command: command; arg: string; flags: string list}

(* parses a string and returns an input type *)
let parse_input (args : string) : input =
  failwith "Unimplemented"

(* executes a command *)
let execute (input : input) : unit =
  failwith "Unimplemented"

(* add file contents to the index. *)
let add (input : input) : unit =
  failwith "Unimplemented"

(* creates a new branch *)
let branch (branch_name : string) (options : string list) : unit =
  failwith "Unimplemented"

(* checkout Switch branches or restore working tree files. *)
let checkout (branch_name : string) (options : string list) : unit =
  failwith "Unimplemented"

(* commit Record changes to the repository.
 * Stores the current contents of the index in a new commit
 * along with a log message from the user describing the changes. *)
let commit (msg : string) (options : string list) : unit =
  failwith "Unimplemented"

(* diff Show changes between commits, commit and working tree, etc *)
let diff (commit : string) (options : string list) : unit =
  failwith "Unimplemented"

(* Display help information about CmlControl. *)
let help (options : string) (options : string list) : unit =
  failwith "Unimplemented"

(* init Create an empty CmlControl repository. *)
let init (options : string) (options_other : string list) : unit =
  failwith "Unimplemented"

(* merge Join two or more development histories together. *)
let merge (merge_branch : string) (options : string list) : unit =
  failwith "Unimplemented"

(* Remove files from working tree and from the index. *)
let rm (file_name : string) (options : string list) : unit =
  failwith "Unimplemented"

(* stash Stash the changes in a dirty working directory away. *)
let stash (options : string) (options_other : string list) : unit =
  failwith "Unimplemented"

(* displays any changes to the working directory *)
let status (options : string) (options_other : string list) : unit =
  failwith "Unimplemented"
