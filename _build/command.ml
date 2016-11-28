open Utils
open Print
open Unix

type command =
| Add | Branch | Checkout | Commit | Diff | Help | Init | Log
| Merge | Reset | Rm | Stash | Status | User

type input = { cmd: command; args: string list }

exception Parsing of string

let perm = 0o777

(* add file contents to the index *)
let add (args: string list) : unit =
  let add_help idx_acc file =
    if Sys.file_exists file then
      let hash = create_blob file in
      update_index (file,hash) idx_acc
    else
      raise (Fatal ("pathspec '"^file^"' does not match an file(s)"))
  in
  match args with
  | [] -> raise (Fatal "nothing files specified")
  | f::[] -> begin
    let idx = get_index () in
      if f = "." || f = "-A" then (* add all files *)
        let cwd = get_all_files ["./"] [] in
        let ch = get_changed cwd idx in
        let ut = get_untracked cwd idx in
          List.fold_left add_help idx (ut@ch) |> set_index
      else
        add_help idx f |> set_index
  end
  | _ -> List.fold_left add_help (get_index ()) args |> set_index

(* list, create, or delete branches *)
let branch (args: string list) : unit =
  match args with
  | [] -> begin
    let cur = get_current_branch () in
    let branch_print b =
      if b = cur then (print_string "* "; print_color cur "g")
      else print ("  "^b)
    in
    get_branches () |> List.iter branch_print
  end
  | b::[] -> begin
    if b = "-d" || b = "-D" then raise (Fatal "branch name required")
    else create_branch b
  end
  | flag::b::_ -> begin
    if flag = "-d" || flag = "-D" then delete_branch b
    else raise (Fatal "invalid flags, see [--help]")
  end

(* switch branches or restore working tree files *)
let checkout (args: string list) : unit =
  match args with
  | []    -> raise (Fatal "branch name or HEAD version required")
  | h::[] -> begin
    if h = "-b" || h = "-B" then
      raise (Fatal "branch name required")
    else
      begin
        if (get_branches () |> List.mem h) then switch_branch h
        else if (get_versions () |> List.mem h) then switch_version h
        else raise (Fatal ("pathspec '"^h^"' does not match an file(s)"))
      end
  end
  | flag::b::_ -> begin
    if flag = "-b" || flag = "-B" then
      let _ = create_branch b in switch_branch b
    else
      raise (Fatal ("invalid flags, see [--help]"))
  end

(* record changes to the repository:
 * stores the current contents of the index in a new commit
 * along with commit metadata. *)
let commit (args: string list) : unit =
  failwith "Unimplemented"

(* show changes between working tree and previous commits *)
let diff (args: string list) : unit =
  failwith "Unimplemented"

(* display help information about CmlControl. *)
let help () : unit =
  print_help ()

(* init Create an empty CmlControl repository. *)
let init () : unit =
  if cml_initialized "./" then
    raise (Fatal "Cml repository already initialized")
  else
    mkdir ".cml" perm; mkdir ".cml/heads" perm; mkdir ".cml/objects" perm;
    let _ = create_branch "master" in
		let out = open_out ".cml/HEAD" in
		output_string out "heads/master"; close_out out;
    print_color "initialized empty Cml repository" "b"

(* display the current branches commit history *)
let log () : unit =
  failwith "Unimplemented"

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
  else
    let old_path = Sys.getcwd () in
    let nu_path = (chdir_to_cml (); Sys.getcwd ()) in
    let file_path  = remove_from_string old_path nu_path in
    Sys.chdir old_path;
    let remove_from_idx file =
      if Sys.file_exists file then
        let abs_file_path = if file_path = "" then
            file
          else
            file_path^"/"^file |> Str.replace_first (Str.regexp "^/") "" in
            let file' = begin
              if Sys.is_directory file then
                List.map (Str.replace_first (Str.regexp "//") "/") (get_all_files [file] [])
              else
                [abs_file_path]
            end
        in
        let rm_predicate (path,_) =
          not (List.mem path file')
        in
        chdir_to_cml ();
        let idx = get_index () in
        Sys.chdir old_path;
        let idx' = List.filter rm_predicate idx in
        chdir_to_cml ();
        set_index idx';
        Sys.chdir old_path
      else
        raise (Fatal ("pathspec '"^file^"' does not match an file(s)"))
    in
    List.iter remove_from_idx args


(* stashes changes made to the current working tree *)
let stash (args: string list) : unit =
  failwith "Unimplemented"

(* show the working tree status *)
let status () : unit =
    chdir_to_cml ();
    print ("On branch "^(get_current_branch ())^"\n");
    let cwd = get_all_files ["./"] [] in
    let idx = get_index () in
    let st = get_staged idx in
    let ch = get_changed cwd idx in
    let ut = get_untracked cwd idx in
      match (st,ch,ut) with
      | [],[],[] -> print "no changes to be committed, working tree clean"
      | _ -> let _ = print_staged st in
             let _ = print_changed ch in print_untracked ut

(* set the user info to [username] *)
let user (args: string list) : unit =
  match args with
  | []   -> let name = get_user_info () in print ("Current user: "^name)
  | h::_ -> set_user_info h

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
