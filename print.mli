(* Print Module --- prints to the terminal *)


(* prints a string in the color specified *)
val print_color: string -> string -> unit

(* prints an error message in error format *)
val print_error: string -> string -> unit

(* prints a specified amount of indentations *)
val print_indent: string -> int -> unit

(* prints a normal string *)
val print: string -> unit

(* prints a newline *)
val print_newline: unit -> unit

(* prints the camel emoji *)
val print_camel: unit -> unit
