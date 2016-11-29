(* Print Module --- prints to the terminal *)


(* prints a string in the color specified *)
val print_color: string -> string -> unit

(* prints an error message in error format *)
val print_error: string -> string -> unit

(* prints a specified amount of indentations *)
val print_indent: string -> string -> int -> unit

(* prints a normal string *)
val print: string -> unit

(* prints a newline *)
val print_newline: unit -> unit

(* prints the camel emoji *)
val print_camel: unit -> unit

(* prints the files staged for a commit *)
val print_staged: string list -> unit

(* prints the files not staged for commit *)
val print_changed: string list -> unit

(* prints untracked files *)
val print_untracked: string list -> unit

(* prints the commit message for [cml log] *)
val print_commit: string -> string -> string -> string -> unit

(* print help info for add *)
val print_help_add: unit -> unit

(* print help info for reset *)
val print_help_reset: unit -> unit

(* print help info for rm *)
val print_help_rm: unit -> unit

(* print help info for stash *)
val print_help_stash: unit -> unit

(* print help info for branch *)
val print_help_branch: unit -> unit

(* print help info for checkout *)
val print_help_checkout: unit -> unit

(* print help info for commit *)
val print_help_commit: unit -> unit

(* print help info for diff *)
val print_help_diff: unit -> unit

(* print help info for merge *)
val print_help_merge: unit -> unit

(* prints a help log for all Cml commands *)
val print_help: unit -> unit