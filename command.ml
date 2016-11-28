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
  | [] -> raise (Fatal "no files specified")
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
  let user = get_user_info () in
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
          let last_commit = try get_head () with Fatal n -> "None" in
            create_commit tree user tm msg last_commit
        end
    end
  in
  set_head new_head

(* show changes between working tree and previous commits *)
let diff (args: string list) : unit =
  let idx = get_index () in
  match args with
    | [] -> Diff.print_diff_files_mult (List.map (fun (fn, hn) -> (fn, get_object_path hn)) idx)
    | file_name::_ -> Diff.print_diff_files file_name (get_object_path (List.assoc file_name idx))

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
  failwith "Unimplemented"

(* stashes changes made to the current working tree *)
let stash (args: string list) : unit =
  failwith "Unimplemented"

(* show the working tree status *)
let status () : unit =
    print ("On branch "^(get_current_branch ())^"\n");
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
