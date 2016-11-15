(* Command Module --- types and functions for cml commands *)

(* A variant type for the commands supported by cml control. *)
type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Merge | Rm
| Stash | Status

(* a record type for the input a user gives to the terminal.
 * command is the desired command, arg is what the user wants to run the command
 * on, and flags are any flags to modify the command.
 *)
type input = {
  command: command;
  arg: string;
  flags: string list
}

(* parses a string and returns an input type *)
val parse_input : string array -> input
(* executes a command *)
val execute     : input -> unit

(* add file contents to the index. *)
val add      : input -> unit
(* creates a new branch *)
val branch   : string -> string list -> unit
(* checkout Switch branches or restore working tree files. *)
val checkout : string -> string list -> unit
(* commit Record changes to the repository.
 * Stores the current contents of the index in a new commit
 * along with a log message from the user describing the changes. *)
val commit   : string -> string list -> unit
(* diff Show changes between commits, commit and working tree, etc *)
val diff     : string -> string list -> unit
(* Display help information about CmlControl. *)
val help     : string -> string list -> unit
(* init Create an empty CmlControl repository. *)
val init     : string list -> unit
(* merge Join two or more development histories together. *)
val merge    : string -> string list -> unit
(* Remove files from working tree and from the index. *)
val rm       : string -> string list -> unit
(* stash Stash the changes in a dirty working directory away. *)
val stash    : string -> string list -> unit
(* displays any changes to the working directory *)
val status   : string -> string list -> unit
