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
let rec print_indent (msg : string) (c : string) (n : int) : unit =
  if n = 0 then print_color msg c else
  let _ = print_string "  " in print_indent msg c (n - 1)

(* prints a normal string *)
let print (msg : string) : unit =
  print_endline msg

(* prints a newline *)
let print_newline () :  unit =
  print_string "\n"

(* prints the camel emoji *)
let print_camel () : unit =
	print_endline "🐪 "

(* prints the files staged for a commit *)
let print_staged (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print "Changes to be committed:";
    List.iter (fun s -> print_indent s "green" 3) files;
  end

(* prints the files not staged for commit *)
let print_changed (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print ("Changes not staged for commit:");
    print "  (use \"cml add <file>\" to stage for commit";
    List.iter (fun s -> print_indent s "red" 3) files;
  end

(* prints untracked files *)
let print_untracked (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print ("Untracked files:");
    print "  (use \"cml add <file>\" to include in commit";
    List.iter (fun s -> print_indent s "red" 3) files;
  end