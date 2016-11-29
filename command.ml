open Utils
open Print
open Unix
open Tree

type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status | User

type input = { cmd: command; args: string list }

exception Parsing of string

let perm = 0o777

(* helper function that returns a list of files staged for commit *)
let get_staged_help (idx : index) : string list =
  try
    let cmt = get_head () |> parse_commit in
    Tree.read_tree "" cmt.tree |> Tree.tree_to_index |> get_staged idx
  with
  | Fatal _ -> get_staged idx []

let rec print_list = function
  | [] -> ()
  | h::t -> print_endline h; print_list t

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

(* add file contents to the index *)
let add (args: string list) : unit =
  if args = [] then
    raise (Fatal "no files specified")
  else
    let add_to_idx rel_path =
      if Sys.file_exists rel_path then begin
        let add_files = get_files_from_rel_path rel_path in
        add_files_to_idx add_files
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

(* switches state of repo to state of given commit_hash *)
let switch_version (commit_hash : string) : unit =
  let commit = parse_commit commit_hash in
  let tree = Tree.read_tree "" commit.tree in
  Tree.recreate_tree "" tree;
  set_index (Tree.tree_to_index tree)

(* switch branches or restore working tree files *)
let checkout (args: string list) : unit =
  chdir_to_cml ();
  let isdetached = detached_head () in
  match args with
  | []    -> raise (Fatal "branch name or HEAD version required")
  | [arg] ->
    begin
        if (get_branches () |> List.mem arg) then
          let _ = switch_version (get_branch_ptr arg) in
          let _ = switch_branch arg isdetached in
          print ("Switched to branch '"^arg^"'")
        else if ((get_all_files ["./"] []) |> List.mem arg) then
          get_index () |> checkout_file arg
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
        let _ = get_head () |> create_branch b in switch_branch b isdetached
      else
        raise (Fatal ("invalid flags, see [--help]"))
    end

(* record changes to the repository:
 * stores the current contents of the index in a new commit
 * along with commit metadata. *)
let commit (args: string list) : unit =
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
      if flag <> "-am" && flag <> "-m" then
        raise (Fatal "unrecognized flags, see [--help]")
      else
        if flag = "-am" then
          begin
            let idx = get_index () in
            let cwd = get_all_files ["./"] [] in
            if (get_changed cwd idx) = [] then
            match get_changed cwd idx with
            | []  -> if (get_staged_help idx) = [] then
              raise (Fatal "nothing added to commit but untracked files are present")
            | files -> add files
          end;
        let idx = get_index () in
        if idx = [] then raise (Fatal "nothing added to commit")
        else begin
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

(* diff map helper function *)
let diff_map_help (file, hash) = (file, get_object_path hash)

(* show changes between working tree and previous commits *)
let diff (args: string list) : unit =
  let ch_idx = get_index () |> get_changed_as_index (get_all_files ["./"] []) in
  try match args with
    | [] -> List.map diff_map_help ch_idx |> Diff.diff_mult
    | hd::[] -> begin
      if hd = "." || hd = "-a" then
        List.map diff_map_help ch_idx |> Diff.diff_mult
      else
        get_object_path (List.assoc hd ch_idx) |> Diff.diff_file hd
    end
    | hd::t -> begin
      if hd = "." || hd = "-a" then
        raise (Fatal "invalid arguments, see [--help]")
      else
        List.map (fun f -> (f, get_object_path (List.assoc f ch_idx))) args
        |> Diff.diff_mult
    end
  with
  | Not_found -> ()

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
  let rec log_loop ptr cmt =
    let _ = print_commit ptr cmt.author cmt.date cmt.message in
    if cmt.parent = "None" then ()
    else cmt.parent |> parse_commit |> log_loop cmt.parent
  in try
    let head = get_head () in parse_commit head |> log_loop head
  with
  | Fatal m -> begin
    if m = "HEAD not initialized" then
      let br = get_current_branch () in
      raise (Fatal ("current branch '"^br^"' does not have any commits yet"))
    else raise (Fatal m)
  end

(* join two or more development histories together *)
let merge (args: string list) : unit =
  failwith "Unimplemented"

(* reset the current HEAD to a specified state *)
let reset (args: string list) : unit =
  failwith "Unimplemented"

(* remove files from working tree and index *)
let rm (args: string list) : unit =
  if args = [] then
    raise (Fatal "no files specified")
  else begin
    let cwd = Sys.getcwd () in
    chdir_to_cml ();
    let idx = get_index () in
    let removable_files = get_staged_help idx in
    Sys.chdir cwd;
    let remove_from_idx rel_path =
      if Sys.file_exists rel_path then begin
        let rm_files = get_files_from_rel_path rel_path in
        rm_files_from_idx rm_files removable_files
      end else if rel_path = "-A" then begin
        let cwd = Sys.getcwd () in
        chdir_to_cml ();
        let rm_files = get_all_files ["./"] [] in
        Sys.chdir cwd;
        rm_files_from_idx rm_files removable_files
      end else
        raise (Fatal ("pathspec '"^rel_path^"' does not match an file(s)"))
    in
    List.iter remove_from_idx args
  end

(* stashes changes made to the current working tree *)
let stash (args: string list) : unit =
  failwith "Unimplemented"

(* helper for printing status message *)
let status_message () =
  if detached_head () then
    let head = get_detached_head () in
    print_color ("HEAD detached at " ^ head) "r"
  else
    print ("On branch "^(get_current_branch ())^"\n")

(* show the working tree status *)
let status () : unit =
  chdir_to_cml ();
  status_message ();
  let cwd = get_all_files ["./"] [] in
  let idx = get_index () in
  let st = get_staged_help idx in
  let ch = get_changed cwd idx in
  let ut = get_untracked cwd idx in
    match (st,ch,ut) with
    | [],[],[] -> print "no changes to be committed, working tree clean"
    | _ -> let _ = print_staged st in
           let _ = print_changed ch in print_untracked ut

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
  | [] -> raise (Fatal "no command given, see [--help]")
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
