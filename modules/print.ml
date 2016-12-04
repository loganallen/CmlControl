(******************************** Print Module ********************************)
(******************************************************************************)

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
let print_error (msg : string) : unit =
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

(* prints the a warning specifying files that need to be delt with *)
let print_invalid_cml_state (files: string list) : unit =
  print "Your changes to the following files would be overwritten:\n";
  List.iter (fun f -> print_indent f "y" 3) files;
  print "\nPlease commit or stash your changes beforehand."

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
    (staged_files @ deleted_files) |> List.sort Pervasives.compare |>
    List.iter (fun s -> print_indent s "green" 3);
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

(* print help info for add (long version) *)
let print_help_add_long () : unit =
  print_indent "usage: cml add [-A] <pathspec>..." "b" 1;
  print_newline ();
  print_indent "This command updates the index using the current content found in" "black" 1;
  print_indent "the working tree, to prepare the content staged for the next" "black" 1;
  print_indent "commit." "black" 1;
  print_newline ();
  print_indent "The \"index\" holds a snapshot of the content of the working tree," "black" 1;
  print_indent "and it is this snapshot that is taken as the contents of the next" "black" 1;
  print_indent "commit. Thus after making any changes to the working tree, and" "black" 1;
  print_indent "before running the commit command, you must use the add command to" "black" 1;
  print_indent "add any new or modified files to the index." "black" 1;
  print_newline ();
  print_indent "This command can be performed multiple times before a commit. It" "black" 1;
  print_indent "only adds the content of the specified file(s) at the time the add" "black" 1;
  print_indent "command is run; if you want subsequent changes included in the" "black" 1;
  print_indent "next commit, then you must run cml add again to add the new" "black" 1;
  print_indent "content to the index." "black" 1;
  print_newline ();
  print_indent "The cml status command can be used to obtain a summary of which" "black" 1;
  print_indent "files have changes that are staged for the next commit." "black" 1;
  print_newline ();
  print_indent "<pathspec>..." "black" 1;
  print_indent "Files to add. A leading directory name (e.g.  dir to add dir/)" "black" 3;
  print_indent "can be given to add the entire directory." "black" 3;
  print_newline ();
  print_indent "-A" "black" 1;
  print_indent "All files in the entire working tree are updated" "black" 3

(* print help info for add *)
let print_help_add () : unit =
  print_indent "add\t\tAdd file contents to the index" "b" 1;
  print_indent "usage: cml add [-A] <pathspec>..." "y" 8

(* print help info for reset (long version) *)
let print_help_reset_long () : unit =
  print_indent "cml reset [<mode>] [<commit>]" "b" 1;
  print_newline ();
  print_indent "Resets the current branch head to <commit> and possibly" "black" 1;
  print_indent "updates the index (resetting it to the tree of <commit>) and the" "black" 1;
  print_indent "working tree depending on <mode>. If <commit> is omitted," "black" 1;
  print_indent "the most recent commit. If <mode> is omitted, defaults to" "black" 1;
  print_indent "\"--mixed\". The <mode> must be one of the following:" "black" 1;
  print_newline ();
  print_indent "--soft" "black" 1;
  print_indent "Does not touch the index file or the working tree at all (but" "black" 3;
  print_indent "resets the head to <commit>, just like all modes do). This" "black" 3;
  print_indent "leaves all your changed files \"Changes to be committed\", as cml" "black" 3;
  print_indent "status would put it." "black" 3;
  print_newline ();
  print_indent "--mixed" "black" 1;
  print_indent "Resets the index but not the working tree (i.e., the changed" "black" 3;
  print_indent "files are preserved but not marked for commit) and reports what" "black" 3;
  print_indent "has not been updated. This is the default action." "black" 3;
  print_newline ();
  print_indent "--hard" "black" 1;
  print_indent "Resets the index and working tree. Any changes to tracked files" "black" 3;
  print_indent "in the working tree since <commit> are discarded." "black" 3

(* print help info for reset *)
let print_help_reset () : unit =
  print_indent "reset\t\tReset the current HEAD to a specified state" "b" 1;
  print_indent "usage: cml reset [--soft | --mixed | --hard] [<commit>]" "y" 8

(* print help info for rm (long version) *)
let print_help_rm_long () : unit =
  print_indent "cml rm [-f] <pathspec>..." "b" 1;
  print_newline ();
  print_indent "Remove files from the index, or from the working tree and the index." "black" 1;
  print_indent "cml rm will not remove a file from just your working directory." "black" 1;
  print_indent "(There is no option to remove a file only from the working tree and" "black" 1;
  print_indent "yet keep it in the index; use /bin/rm if you want to do that.) The" "black" 1;
  print_indent "files being removed have to be identical to the tip of the branch." "black" 1;
  print_indent "When -f is given, the files specified will be removed from the index" "black" 1;
  print_indent "and the working tree. By default the files are only removed from the" "black" 1;
  print_indent "index." "black" 1;
  print_newline ();
  print_indent "<pathspec>..." "black" 1;
  print_indent "Files to remove. A leading directory name (e.g.  dir to remove dir/)" "black" 3;
  print_indent "can be given to remove the entire directory." "black" 3;
  print_newline ();
  print_indent "--cached" "black" 1;
  print_indent "Use this option to remove files from the working tree and the index" "black" 3

(* print help info for rm *)
let print_help_rm () : unit =
  print_indent "rm\t\tRemove files from index or the working tree and index)" "b" 1;
  print_indent "usage: cml rm [-f] <pathspec>..." "y" 8

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

(* print an error message that the repository data is corrupted *)
let print_corrupt () : unit =
  print_error "error: your CmlControl repository may be corrupted"
