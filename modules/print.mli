(******************************** Print Module ********************************)
(******************************************************************************)

(* prints a string in the color specified *)
val print_color: string -> string -> unit

(* prints an error message in error format *)
val print_error: string -> unit

(* prints a specified amount of indentations *)
val print_indent: string -> string -> int -> unit

(* prints a normal string *)
val print: string -> unit

(* prints a newline *)
val print_newline: unit -> unit

(* prints the camel emoji *)
val print_camel: unit -> unit

(* prints the given branch (with a * if it's the current branch) *)
val branch_print: string -> string -> unit

(* prints the files staged for a commit *)
val print_staged: string list -> string list -> unit

(* prints the files not staged for commit *)
val print_changed: string list -> unit

(* prints untracked files *)
val print_untracked: string list -> unit

(* prints the commit message for [cml log] *)
val print_commit: out_channel -> string -> string -> string -> string -> unit

(* prints a help log for all Cml commands *)
val print_help: unit -> unit

(* print help info for add (long version) *)
val print_help_add_long : unit -> unit

(* print help info for rm (long version) *)
val print_help_rm_long : unit -> unit

(* print help info for reset (long version) *)
val print_help_reset_long : unit -> unit

(* print a warning message to user about being in detached HEAD mode *)
val print_detached_warning: string -> unit

(* print an error message that the repository data is corrupted *)
val print_corrupt: unit -> unit
