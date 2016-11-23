(* Command Module --- types and functions for cml commands *)

(* A variant type for the commands supported by cml control. *)
type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status | User

(* a record type for the input a user gives to the terminal.
 * command is the desired command, arg is what the user wants to run the command
 * on, and flags are any flags to modify the command.
 *)
type input = { command: command; arg: string; flags: string list }

(* Parsing exception for unrecognized cml commands *)
exception Parsing of string

(* add file contents to the index *)
val add: string -> string list -> unit

(* list, create, or delete branches *)
val branch: string -> string list -> unit

(* switch branches or restore working tree files *)
val checkout: string -> string list -> unit

(* record changes to the repository:
 * stores the current contents of the index in a new commit
 * along with commit metadata. *)
val commit: string -> string list -> unit

(* show changes between working tree and previous commits *)
val diff: string -> string list -> unit

(* display help information about CmlControl. *)
val help: unit -> unit

(* init Create an empty CmlControl repository. *)
val init: string list -> unit

(* display the current branches commit history *)
val log: unit -> unit

(* join two or more development histories together *)
val merge: string -> string list -> unit

(* reset the current HEAD to a specified state *)
val reset: string -> string list -> unit

(* remove files from working tree and index *)
val rm: string -> string list -> unit

(* stashes changes made to the current working tree *)
val stash: string -> string list -> unit

(* show the working tree status *)
val status: string -> string list -> unit

(* set the user info to [username] *)
val user: string -> unit

(* parses bash string input and returns a Cml input type *)
val parse_input: string array -> input

(* executes a Cml command *)
val execute: input -> unit
