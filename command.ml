open Common
open Utils
open Print
open Unix
open Tree


type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status | User

type input = { cmd: command; args: string list }
type args_input = { flags: string list; args: string list }

exception Parsing of string

let perm = 0o777

(* parses string of args into flags and actual arguments *)
let parse_args (args : string list) : args_input =
  let acc_flags = fun acc arg ->
    if arg_is_flag arg then (get_flags_from_arg arg) @ acc else acc
  in
  let acc_args = fun acc arg ->
    if arg_is_flag arg then acc else arg::acc
  in
  {
    flags = List.fold_left acc_flags [] args;
    args = List.fold_left acc_args [] args;
  }

let rec verify_allowed_flags allowed_flags flags =
  match flags with
  | [] -> ()
  | h::t -> begin
    if List.mem h allowed_flags then verify_allowed_flags allowed_flags t
    else raise (Fatal ("invalid flag '"^h^"'"))
  end

(* helper function that returns a list of files staged for commit *)
let get_staged_help (idx : index) : string list =
  try
    let cmt = get_head_safe () |> parse_commit in
    Tree.read_tree "" cmt.tree |> Tree.tree_to_index |> get_staged idx
  with
  | Fatal _ -> get_staged idx []

let rec print_list = function
  | [] -> ()
  | h::t -> print_endline h; print_list t

let files_in_index idx =
  List.map (fun (file,_) -> file) idx

(* returns a list of files that were deleted since last commit *)
let get_deleted cwd_files idx =
  let idx_files = files_in_index idx in
  try
    let cmt = get_head_safe () |> parse_commit in
    let cmt_files = Tree.read_tree "" cmt.tree |> Tree.tree_to_index |> files_in_index in
    List.filter (fun file -> (not (List.mem file idx_files)) || (not (List.mem file cwd_files))) cmt_files |> List.sort Pervasives.compare
  with
  | Fatal _ -> []

(* add print message of 'new file:' or 'modified:' to the files
 * precondition: cwd is the .cml repo (chdir_to_cml ()) *)
let add_print_msg files =
  let cmt_idx = try begin
    let cmt = get_head_safe () |> parse_commit in
    Tree.read_tree "" cmt.tree |> Tree.tree_to_index |> files_in_index
  end with
  | Fatal _ -> []
  in
  let add_msg file =
    if List.mem file cmt_idx then "modified:   "^file
    else "new file:   "^file
  in
  List.map add_msg files

(* add print message of 'deleted:' to the files *)
let add_delete_print_msg files =
  List.map (fun file -> "deleted:    "^file) files

let verify_files_in_repo files =
  let filter file =
    if not (Sys.file_exists file) then
      raise (Fatal ("pathspec '"^file^"' is outside the repository"))
    else true
  in
  List.filter filter files

(* returns a list of the file names in [rel_path] to cwd, (the returned
 * filenames are relative to cml repo) *)
let get_files_from_rel_path rel_path =
  let path_from_cml = abs_path_from_cml rel_path in
  begin
    if Sys.is_directory rel_path then
      let rel_path' = begin
        if Str.string_match (Str.regexp ".*/$") rel_path 0 then
          rel_path
        else
          rel_path^"/"
      end in
      get_all_files [rel_path'] []
      |> List.map (fun s ->
        let name = Str.replace_first (Str.regexp "^/") "" (rel_path')
          |> Str.global_replace (Str.regexp "\\.") "\\." in
        Str.replace_first (Str.regexp name) "" s)
      |> List.map (fun s -> (path_from_cml^"/"^s))
      |> List.map (fun s -> Str.global_replace (Str.regexp "\\.\\./") "" s)
      |> List.map (fun s -> Str.global_replace (Str.regexp "\\./") "" s)
    else
      [path_from_cml]
  end |> List.map (fun s -> Str.replace_first (Str.regexp "//") "/" s)
      |> List.map (fun s -> Str.replace_first (Str.regexp "^/") "" s)
      |> verify_files_in_repo

(* add file contents to the index *)
let add (args: string list) : unit =
  if args = [] then
    raise (Fatal "no files specified")
  else
    let add_to_idx rel_path =
      if Sys.file_exists rel_path then begin
        try begin
          let add_files = get_files_from_rel_path rel_path in
          add_files_to_idx add_files
        end
          with _ -> raise (Fatal ("pathspec '"^rel_path^"' is outside the repository"))
      end else if rel_path = "-A" then begin
        let cwd = Sys.getcwd () in
        chdir_to_cml ();
        let add_files = get_all_files ["./"] [] in
        Sys.chdir cwd;
        add_files_to_idx add_files
      end else
        raise (Fatal ("pathspec '"^rel_path^"' does not match an file(s)"))
    in
    List.iter add_to_idx args

(* list, create, or delete branches *)
let branch (args: string list) : unit =
  chdir_to_cml ();
  let isdetached = detached_head () in
  match args with
  | [] -> begin
    let cur = if isdetached then "" else get_current_branch () in
    get_branches () |> List.iter (branch_print cur)
  end
  | b::[] -> begin
    if b = "-d" || b = "-D" then raise (Fatal "branch name required")
    else
      let ptr = if isdetached then get_detached_head () else get_head () in
      create_branch b ptr
  end
  | flag::b::_ -> begin
    if flag = "-d" || flag = "-D" then delete_branch b
    else raise (Fatal "invalid flags, see [--help]")
  end

let invalid_cml_state (st : string list) (ch : string list) : unit =
  let _ = print "Your changes to the following files would be overwritten:\n" in
  let rec loop = function
    | [] -> ()
    | h::t -> print_indent h "y" 3; loop t
  in loop (st @ ch);
  print "\nPlease commit or stash your changes beforehand."

(* switch branches or restore working tree files *)
let checkout (args: string list) : unit =
  chdir_to_cml ();
  let cwd = get_all_files ["./"] [] in
  let idx = get_index () in
  let st = get_staged_help idx in
  let ch = get_changed cwd idx in
  let isdetached = detached_head () in
  match args with
  | []    -> raise (Fatal "branch name or HEAD version required")
  | [arg] ->
    begin
      if ((get_all_files ["./"] []) |> List.mem arg) then
        get_index () |> checkout_file arg
      else if st <> [] || ch <> [] then
        invalid_cml_state st ch
      else if (get_branches () |> List.mem arg) then
        if switch_branch arg isdetached then
        get_branch_ptr arg |> switch_version true else ()
      else
        try
          if isdetached && arg = get_detached_head () then
            print ("Already detached HEAD at " ^ arg)
          else
            let commit = parse_commit arg in
            let tree = Tree.read_tree "" commit.tree in
            Tree.recreate_tree "" tree;
            set_index (Tree.tree_to_index tree);
            set_detached_head arg;
            print_detached_warning arg
        with
          | Fatal _ -> raise (Fatal ("pathspec '"^arg^"' does not match an file(s)/branch/commit"))
    end
  | flag::b::_ ->
    begin
      if flag = "-b" || flag = "-B" then
        if st <> [] || ch <> [] then
          invalid_cml_state st ch
        else
          let _ = get_head_safe () |> create_branch b in
          let _ = switch_branch b isdetached in ()
      else
        raise (Fatal ("invalid flags, see [--help]"))
    end

(* record changes to the repository:
 * stores the current contents of the index in a new commit
 * along with commit metadata. *)
let commit (args: string list) : unit =
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  let user = get_user_info () in
  let isdetached = detached_head () in
  let new_head =
    match args with
    | [] -> raise (Fatal "no commit arguments found, see [--help]")
    | h::[] -> begin
      let err =
        if h = "-am" || h = "-m" then
          "missing commit message, try ["^h^" <message>]"
        else
          "unrecognized flag, see [--help]"
      in raise (Fatal err)
    end
    | flag::lst -> begin
      if flag <> "-am" && flag <> "-ma" && flag <> "-m" then
        raise (Fatal "unrecognized flags, see [--help]")
      else
        let cwd_files = get_all_files ["./"] [] in
        let changed_files = get_changed cwd_files (get_index ()) in
        begin if (flag = "-am" || flag = "-ma") && changed_files <> [] then add changed_files else () end;
        let deleted_files = get_deleted (get_all_files ["./"] []) (get_index ()) in
        rm_files_from_idx deleted_files;
        let idx = get_index () in
        let staged_files = get_staged_help idx in
        if staged_files = [] && deleted_files = [] then begin
          let untracked_files = get_untracked cwd_files (get_index ()) in
          if untracked_files = [] then
            raise (Fatal "nothing added to commit")
          else begin
            Sys.chdir cwd;
            print_untracked (untracked_files |> List.map get_rel_path);
            chdir_to_cml ();
            raise (Fatal "nothing added to commit but untracked files are present")
          end
        end else begin
          let tree = Tree.index_to_tree idx |> Tree.write_tree in
          let msg = List.rev lst |> List.fold_left (fun acc s -> s^" "^acc) ""
                    |> String.trim in
          let tm = time () |> localtime |> Time.get_time in
          let last_commit =
            try if isdetached then get_detached_head () else get_head ()
            with Fatal n -> "None" in
          create_commit tree user tm msg last_commit
        end
    end
  in
  if isdetached then
    let _ = set_detached_head new_head in
    print_detached_warning new_head
  else set_head new_head

(* precondition: cwd is cml repo root *)
let get_diff_current_index () =
  let idx = get_index () in
  let cwd_files = get_all_files ["./"] [] in
  let deleted_files = get_deleted cwd_files idx in
  let changed_files = get_changed cwd_files idx in
  let changed_diff_index = List.map (fun file -> (file, file)) changed_files
                           |> Diff.index_to_diff_index false in
  List.filter (fun (file,_) -> not (List.mem file deleted_files)) idx
  |> Diff.index_to_diff_index true
  |> List.map (fun (file,hash) ->
    if List.mem_assoc file changed_diff_index then
      (file, List.assoc file changed_diff_index)
    else
      (file,hash))

(* precondition: [abs_path_lst] holds the absolute paths from cml.
                 Also, all the files are uncompressed *)
let diff_idx_current_files abs_path_lst =
  List.map (fun f -> (f,f)) abs_path_lst |> Diff.index_to_diff_index false

(* return the diff index of the cmt_idx for each file in [files] *)
let diff_idx_commit idx files =
  let acc_idx acc file =
    try begin
      let hash = List.assoc file idx in
      (file,hash)::acc
    end with
      | Not_found -> acc
  in
  List.fold_left acc_idx [] files |> Diff.index_to_diff_index true

(* show changes between working tree and previous commits *)
let diff (args: string list) : unit =
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  let current_diff_idx = get_diff_current_index () in
  let commit_index = get_commit_index (get_head_safe ()) in
  let commit_diff_idx = commit_index |> Diff.index_to_diff_index true in
  let get_arg_file arg =
    if Sys.file_exists arg then begin
      Sys.chdir cwd;
      let arg_file = abs_path_from_cml arg |> Str.(replace_first (regexp "/") "") in
      chdir_to_cml ();
      arg_file
    end else arg
  in
  match args with
  | [] -> begin
    Diff.diff_indexes commit_diff_idx current_diff_idx
  end
  | arg::[] -> begin
    let arg_file = get_arg_file arg in
    (* print_endline ("arg_file: "^ arg_file); *)
    if Sys.file_exists arg_file || arg_file = "" then
      let files = get_files_from_rel_path arg_file in
      let current_diff_idx = diff_idx_current_files files in
      let commit_diff_idx = diff_idx_commit commit_index files in
      Diff.diff_indexes commit_diff_idx current_diff_idx
    else if Sys.file_exists arg then
      raise (Fatal ("pathspec '"^arg^"' is outside the repository"))
    else if List.mem arg (get_branches ()) then
      let prev_commit_diff_idx = get_branch_index arg |> Diff.index_to_diff_index true in
      Diff.diff_indexes prev_commit_diff_idx current_diff_idx
    else begin
      let prev_commit_diff_idx = get_commit_index arg |> Diff.index_to_diff_index true in
      Diff.diff_indexes prev_commit_diff_idx current_diff_idx
    end
  end
  | arg1::arg2::[] -> begin
    let old_idx =
      if List.mem arg1 (get_branches ()) then
        get_branch_index arg1
      else
        get_commit_index arg1
    in
    let arg_file = get_arg_file arg2 in
    if Sys.file_exists arg_file || arg_file = "" then
      let files = get_files_from_rel_path arg_file in
      let current_diff_idx = diff_idx_current_files files in
      let old_diff_idx = diff_idx_commit old_idx files in
      Diff.diff_indexes old_diff_idx current_diff_idx
    else
      let old_diff_idx = old_idx |> Diff.index_to_diff_index true in
      let new_diff_idx = get_commit_index arg2 |> Diff.index_to_diff_index true in
      Diff.diff_indexes old_diff_idx new_diff_idx
  end
  | _ -> raise (Fatal "invalid arguments, see [--help]")

(* display help information about CmlControl. *)
let help () : unit =
  print_help ()

(* init Create an empty CmlControl repository. *)
let init () : unit =
  if cml_initialized "./" then
    raise (Fatal "Cml repository already initialized")
  else
    mkdir ".cml" perm; mkdir ".cml/heads" perm; mkdir ".cml/objects" perm;
    let _ = create_branch "master" "" in
		let out = open_out ".cml/HEAD" in
		output_string out "heads/master"; close_out out;
    print_color "initialized empty Cml repository" "b"

(* display the current branches commit history *)
let log () : unit =
  chdir_to_cml ();
  let oc = open_out ".cml/log" in
  let rec log_loop oc ptr cmt =
    let _ = print_commit oc ptr cmt.author cmt.date cmt.message in
    if cmt.parent = "None" then close_out oc
    else cmt.parent |> parse_commit |> log_loop oc cmt.parent
  in try
    let head = get_head_safe () in
    parse_commit head |> log_loop oc head;
    let _ = Sys.command "less -RXF .cml/log" in ()
  with
  | Fatal m -> begin
    if m = "HEAD not initialized" then
      let br = get_current_branch () in
      raise (Fatal ("current branch '"^br^"' does not have any commits yet"))
    else raise (Fatal m)
  end

(* perform fast-forward merge by updating the head to the branch head *)
let merge_fast_forward (branch : string) (ptr : string) : unit =
  print ("Updating branch '" ^ branch ^ "' with fast-forward merge...");
  set_head ptr; switch_version true ptr;
  print "\nMerge successful."

(* join two or more development histories together *)
let merge (args: string list) : unit =
  match args with
  | []     -> raise (Fatal "no branch specified, see [--help]")
  | br::[] -> begin
    let cwd = get_all_files ["./"] [] in
    let idx = get_index () in
    let st = get_staged_help idx in
    let ch = get_changed cwd idx in
    if st <> [] || ch <> [] then invalid_cml_state st ch
    else begin
      let cur_hd = get_head () in
      let branch_hd = get_branch_ptr br in
      (* check for up-to-date merge *)
      let rec soft_loop_check ptr =
        if ptr = "None" then true
        else if ptr = branch_hd then
          let _ = print "Already up-to-date." in false
        else
          let cmt = parse_commit ptr in soft_loop_check cmt.parent
      in
      if soft_loop_check cur_hd then
        (* fast-forward loop check *)
        let rec ff_loop_check ptr =
          if ptr = "None" then true
          else if ptr = cur_hd then
            let _ = merge_fast_forward (get_current_branch ()) branch_hd in false
          else
            let cmt = parse_commit ptr in ff_loop_check cmt.parent
        in
        if ff_loop_check branch_hd then
          print_color "TODO: Actual merging :(" "r"
    end
  end
  | _ -> raise (Fatal "too many arguments, see [--help]")


(* reset the current HEAD to a specified state *)
let reset (args: string list) : unit =
  let {flags; args; } = parse_args args in
  let allowed_flags = ["soft"; "hard"; "mixed"] in
  verify_allowed_flags allowed_flags flags;
  begin if List.length flags > 1 then
    raise (Fatal "usage: git reset [--soft | --mixed | --hard] [<commit>]")
  else () end;
  chdir_to_cml ();
  let head_hash = match args with
    | [] -> get_head_safe ()
    | h::[] -> h
    | _ -> raise (Fatal "usage: git reset [--soft | --mixed | --hard] [<commit>]")
  in
  let commit = parse_commit head_hash in   (* parse_commit does validation *)
  set_head head_hash;
  if List.mem "soft" flags then ()
  else begin
    let tree = Tree.read_tree "" commit.tree in
    let index = Tree.tree_to_index tree in
    set_index index;
    if List.mem "hard" flags then
      Tree.recreate_tree "" tree
    else
      ()
  end

(* remove files from working tree and index *)
let rm (args: string list) : unit =
  let { flags; args; } = parse_args args in
  let allowed_flags = ["f"] in
  verify_allowed_flags allowed_flags flags;
  if args = [] then
    raise (Fatal "no files specified")
  else begin
    let remove_from_idx rel_path =
      if Sys.file_exists rel_path then begin
        try begin
          let rm_files = get_files_from_rel_path rel_path in
          rm_files_from_idx rm_files;
          if List.mem "f" flags then begin
            if Sys.is_directory rel_path then begin
              rm_files_from_repo rm_files;
              try Unix.rmdir rel_path with _ -> ()
            end else
              rm_files_from_repo rm_files
          end else ()
        end
          with _ -> raise (Fatal ("pathspec '"^rel_path^"' is outside the repository"))
      end else
        raise (Fatal ("pathspec '"^rel_path^"' does not match an file(s)"))
    in
    List.iter remove_from_idx args
  end

(* stashes changes made to the current working tree *)
let stash (args: string list) : unit =
  match args with
    | [] -> begin
      try
        chdir_to_cml ();
        let cwd = get_all_files ["./"] [] in
        let idx = get_index () in
        let ch = get_changed cwd idx in
        let st = get_staged_help idx in
        if ch = [] && st = [] then print "No changes to stash"
        else begin
          let ch_idx = List.map (fun file -> (file,create_blob file)) ch in
          let st_idx = List.map (fun file -> (file, List.assoc file idx)) st in
          let f_idx = ch_idx @ st_idx in
          let new_tree = Tree.index_to_tree f_idx |> Tree.write_tree in
          let user = get_user_info () in
          let tm = time () |> localtime |> Time.get_time in
          let head = get_head () in
          let commit = create_commit new_tree user tm "Stash" head in
          switch_version true head;
          let oc = open_out ".cml/stash" in
          output_string oc commit;
          close_out oc;
        end
      with
      | Fatal f -> print_endline "cannot stash - no commit history"
    end
    | h::t -> begin
      if h = "apply" then
        try
          let ic = open_in ".cml/stash" in
          let version = input_line ic in
          switch_version false version;
          open_out ".cml/stash" |> close_out;
        with
          | Sys_error _ | End_of_file -> print "No saved stashes to apply"
      else raise (Fatal ("not a valid argument to the stash command"))
    end

(* helper for printing status message *)
let status_message () =
  if detached_head () then
    let head = get_detached_head () in
    print_color ("HEAD detached at " ^ head) "r"
  else
    print ("On branch "^(get_current_branch ())^"\n")

(* show the working tree status *)
let status () : unit =
  let cwd = Sys.getcwd () in
  chdir_to_cml ();
  begin
    if detached_head () then
      print_color ("HEAD detached at "^(get_detached_head ())) "red"
    else
      print ("On branch "^(get_current_branch ())^"\n")
  end;
  let cwd_files = get_all_files ["./"] [] in
  let idx = get_index () in
  let st = get_staged_help idx in
  let ch = get_changed cwd_files idx in
  let ut = get_untracked cwd_files idx in
  let deleted_files = get_deleted cwd_files idx in
  Sys.chdir cwd;
  let st' = st |> List.map get_rel_path in
  let ch' = ch |> List.map get_rel_path in
  let ut' = ut |> List.map get_rel_path in
  let deleted_files' = deleted_files |> List.map get_rel_path in
  match (st', ch', ut', deleted_files') with
    | [],[],[],[] -> print "no changes to be committed, working tree clean"
    | _ -> begin
      print_staged (st' |> add_print_msg) (deleted_files' |> add_delete_print_msg);
      print_changed (ch' |> add_print_msg);
      print_untracked ut'
    end

(* set the user info to [username] *)
let user (args: string list) : unit =
  chdir_to_cml ();
  match args with
  | []   -> let name = get_user_info () in print ("Current user: "^name)
  | lst -> begin
    List.rev lst |> List.fold_left (fun acc s -> s^" "^acc) "" |>
    String.trim |> set_user_info
  end

(* parses bash string input and returns a Cml input type *)
let parse_input (args : string array) : input =
  match (Array.to_list args) with
  | [] -> {cmd = Help; args = []}
  | h::t -> begin
    match h with
    | "add"      -> {cmd = Add; args = t}
    | "branch"   -> {cmd = Branch; args = t}
    | "checkout" -> {cmd = Checkout; args = t}
    | "commit"   -> {cmd = Commit; args = t}
    | "diff"     -> {cmd = Diff; args = t}
    | "init"     -> {cmd = Init; args = t}
    | "log"      -> {cmd = Log; args = t}
    | "merge"    -> {cmd = Merge; args = t}
    | "reset"    -> {cmd = Reset; args = t}
    | "rm"       -> {cmd = Rm; args = t}
    | "stash"    -> {cmd = Stash; args = t}
    | "status"   -> {cmd = Status; args = t}
    | "help"     -> {cmd = Help; args = t}
    | "--help"   -> {cmd = Help; args = t}
    | "--user"   -> {cmd = User; args = t}
    | cmd        -> raise (Parsing cmd)
  end

(* executes a Cml command *)
let execute (i : input) : unit =
  try
    if not (i.cmd = Init || i.cmd = Help) && not (cml_initialized_r "./") then
      raise (Fatal "Not a Cml repository (or any of the parent directories)")
    else
      match i.cmd with
      | Add      -> add i.args
      | Branch   -> branch i.args
      | Checkout -> checkout i.args
      | Commit   -> commit i.args
      | Diff     -> diff i.args
      | Help     -> help ()
  		| Init     -> init ()
      | Log      -> log ()
      | Merge    -> merge i.args
      | Reset    -> reset i.args
      | Rm       -> rm i.args
      | Stash    -> stash i.args
      | Status   -> status ()
      | User     -> user i.args
  with
  | Fatal msg -> print ("fatal: "^msg)
