(* prints a string in the color specified *)
let print_color (msg : string) (color : string) : unit =
  let m = (msg ^ "\n") in
  match color with
    | "r" | "red"    -> ANSITerminal.(print_string [red] m)
    | "g" | "green"  -> ANSITerminal.(print_string [green] m)
    | "b" | "blue"   -> ANSITerminal.(print_string [blue] m)
    | "y" | "yellow" -> ANSITerminal.(print_string [yellow] m)
    | _              -> ANSITerminal.(print_string [black] m)

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
    print_indent "(use \"cml add <file>\" to stage for commit)" "" 1;
    List.iter (fun s -> print_indent s "red" 3) files;
  end

(* prints untracked files *)
let print_untracked (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print ("Untracked files:");
    print_indent "(use \"cml add <file>\" to include in commit)" "" 1;
    List.iter (fun s -> print_indent s "red" 3) files;
  end

(* prints a help log for all Cml commands *)
let print_help () : unit =
  print_camel (); print_newline ();
  print_color "usage: [--help] [--user [<name>]]" "b"; print_newline ();
  print "The following are cml commands and usages:";
  print_newline ();
  print_color "starting a cml version control repository" "";
  print_indent "init\t\tCreate an empty Cml repository" "b" 1;
  print_newline ();
  print "work on current changes";
  print_indent "add\t\tAdd file contents to the index" "b" 1;
  print_indent "reset\t\tReset the current HEAD to a specified state" "b" 1;
  print_indent "rm\t\tRemove files from the working tree and index" "b" 1;
  print_indent "stash\t\tStashes changes made to the current working tree" "b" 1;
  print_newline ();
  print "examine the history and state";
  print_indent "log\t\tShow commit logs" "b" 1;
  print_indent "status\tShow the working tree status" "b" 1;
  print_newline ();
  print "grow and tweak the cml history";
  print_indent "branch\tList, create, or delete branches" "b" 1;
  print_indent "checkout\tSwitch branches or restore working tree files" "b" 1;
  print_indent "commit\tRecord changes to the repository" "b" 1;
  print_indent "diff\t\tShow changes between working tree and previous commits" "b" 1;
  print_indent "merge\t\tJoin two or more development histories together" "b" 1;
  print_newline (); print_camel ();
