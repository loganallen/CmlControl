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

(* prints the given branch (with a * if it's the current branch) *)
let branch_print (cur : string) (branch : string) : unit =
  if branch = cur then (print_string "* "; print_color cur "g")
  else print ("  "^branch)

(* prints the files staged for a commit *)
let print_staged (staged_files : string list) (deleted_files : string list): unit =
  match (staged_files, deleted_files) with
  | [],[] -> ()
  | _  -> begin
    print "Changes to be committed:";
    print_indent "(use \"cml rm <filename>...\" to unstage)\n" "" 1;
    List.iter (fun s -> print_indent s "green" 3) ((staged_files @ deleted_files) |> List.sort Pervasives.compare);
    print_newline ()
  end

(* prints the files not staged for commit *)
let print_changed (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print ("Changes not staged for commit:");
    print_indent "(use \"cml add <filename>...\" to stage for commit)\n" "" 1;
    List.iter (fun s -> print_indent s "red" 3) files; print_newline ()
  end

(* prints untracked files *)
let print_untracked (files : string list) : unit =
  match files with
  | [] -> ()
  | _  -> begin
    print ("Untracked files:");
    print_indent "(use \"cml add <filename>\" to include in commit)\n" "" 1;
    List.iter (fun s -> print_indent s "red" 3) files; print_newline ()
  end

(* prints the commit message for [cml log] *)
let print_commit (oc : out_channel) (ptr : string) (author : string) (time : string) (msg : string) : unit =
  Printf.fprintf oc "[33mcommit %s\n" ptr;
  Printf.fprintf oc "Author: %s\n" author;
  Printf.fprintf oc "Date: %s\n\n" time;
  Printf.fprintf oc "    %s\n\n" msg

(* print help info for add *)
let print_help_add () : unit =
  print_indent "add\t\tAdd file contents to the index" "b" 1;
  print_indent "usage:  -A | . | <filename> " "y" 8

(* print help info for reset *)
let print_help_reset () : unit =
  print_indent "reset\t\tReset the current HEAD to a specified state" "b" 1;
  print_indent "usage:  <commit>" "y" 8

(* print help info for rm *)
let print_help_rm () : unit =
  print_indent "rm\t\tRemove files from the index" "b" 1;
  print_indent "usage:  . | <filename>" "y" 8

(* print help info for stash *)
let print_help_stash () : unit =
  print_indent "stash\t\tStashes changes made to the current working tree" "b" 1;
  print_indent "returns working directory to current HEAD pointer" "b" 1;
  print_indent "usage: [ apply ] will reapply the stashed changes" "y" 8

(* print help info for branch *)
let print_help_branch () : unit =
  print_indent "branch\tCreate, list, or delete branches" "b" 1;
  print_indent "usage:  <branch> | \" \" | [-d | -D] <branch>" "y" 8

(* print help info for checkout *)
let print_help_checkout () : unit =
  print_indent "checkout\tSwitch branches or restore working tree files" "b" 1;
  print_indent "usage:  <branch> | <commit>" "y" 8

(* print help info for commit *)
let print_help_commit () : unit =
  print_indent "commit\tRecord changes to the repository" "b" 1;
  print_indent "usage:  [-am | -m] <message>" "y" 8

(* print help info for diff *)
let print_help_diff () : unit =
  print_indent "diff\t\tShow changes between working tree and previous commits" "b" 1;
  print_indent "usage:  \" \" | <filename>" "y" 8

(* print help info for merge *)
let print_help_merge () : unit =
  print_indent "merge\t\tJoin two or more development histories together" "b" 1;
  print_indent "usage:  <branch>" "y" 8

(* prints a help log for all Cml commands *)
let print_help () : unit =
  print_camel (); print_newline ();
  print_color "usage: [--help | help] [--user [<name>]]" "b"; print_newline ();
  print "The following are cml commands and usages:";
  print_newline ();
  print_color "starting a cml version control repository" "";
  print_indent "init\t\tCreate an empty Cml repository" "b" 1;
  print_newline ();
  print "work on current changes";
  print_help_add (); print_help_reset (); print_help_rm (); print_help_stash ();
  print_newline ();
  print "examine the history and state";
  print_indent "log\t\tShow commit logs" "b" 1;
  print_indent "status\tShow the working tree status" "b" 1;
  print_newline ();
  print "grow and tweak the cml history";
  print_help_branch (); print_help_checkout (); print_help_commit ();
  print_help_diff (); print_help_merge ();
  print_newline (); print_camel ()

(* print a warning message to user about being in detached HEAD mode *)
let print_detached_warning (commit : string) : unit =
  print_color "warning: cml is in detached HEAD mode" "r";
  print_string "detached HEAD set at "; print_color commit "y"
