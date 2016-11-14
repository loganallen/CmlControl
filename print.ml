(* prints a string in the color specified *)
let print_color (msg : string) (color : string) : unit =
  let m = (msg ^ "\n") in
  match color with
    | "red" -> ANSITerminal.(print_string [red] m)
    | "green" -> ANSITerminal.(print_string [green] m)
    | "blue" -> ANSITerminal.(print_string [blue] m)
    | "yellow" -> ANSITerminal.(print_string [yellow] m)
    | _ -> ANSITerminal.(print_string [red] m)

(* prints an error message in error format *)
let print_error (msg : string) (options : string) : unit =
  ANSITerminal.(print_string [red] (msg ^ "\n"))

(* prints a specified amount of indentations *)
let rec print_indent (msg : string) (num_indents : int) : unit =
  print_string " "; print_indent msg (num_indents - 1)

(* prints a newline *)
let print_newline () :  unit =
  print_string "\n"

(* prints the camel emoji *)
let print_camel () : unit =
  failwith "Unimplemented"
