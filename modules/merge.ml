open Unix
open Universal
open Tree

(******************************** Merge Module ********************************)
(******************************************************************************)

(* returns a commit history that is the merge of two histories, ordered by
 * commit timestamp *)
let merge_histories (h1: (string * string) list) (h2: (string * string) list) : (string * string) list =
  let base = List.filter (fun (c,_) -> List.mem_assoc c h2) h1 in
  let h1' = List.filter (fun (c,_) -> not (List.mem_assoc c base)) h1 in
  let h2' = List.filter (fun (c,_) -> not (List.mem_assoc c base)) h2 in
  let sort_help (_,d1) (_,d2) = begin
    if d1 < d2 then -1 else if d1 = d2 then 0 else 1
  end in
  let tail = h1' @ h2' |> List.sort sort_help in
  base @ tail

(* recursively builds the commit history, represented by a list of
 * pairs s.t. in [(h,d);...;(hn,dn)], the tuple [(h,d)] is the
 * commit's hash [h] and the commit's date [d] *)
let rec commit_history_loop (des: (string * string) list) (acc : (string * string) list) (ptr: string) : (string * string) list =
  let cmt = Object.parse_commit ptr in
  let acc' = (ptr,cmt.date)::acc in
  match cmt.parents with
  | [p] -> if p = "None" then acc' else commit_history_loop des acc' p
  | [p1;p2] -> begin
    let des' = acc' @ des in
    let h1 = commit_history_loop des' [] p1 in
    let h2 = commit_history_loop des' [] p2 in
    (merge_histories h1 h2) @ des'
  end
  | _ -> raise Corrupt

(* returns the commit history for the given commit hash pointer *)
let get_commit_history (ptr: string) : string list =
  ptr |> commit_history_loop [] [] |> List.map (fun (h,_) -> h)

(* returns the commit ptr of the common ancestor between two branches
 * and the next commit for the branch *)
let get_common_ancestor (cur_ptr: string) (br_ptr: string) : string =
  let h1 = get_commit_history cur_ptr in
  let h2 = get_commit_history br_ptr in
  let common = List.filter (fun c -> List.mem c h2) h1 in
  match List.rev common with
  | []   -> raise (Fatal "These branches don't have any ancestor in common")
  | h::_ -> h

(* returns an index with a new (f,h) mapping if f is not in [idx] *)
let get_new_files (idx: index) (acc: index) (f,h) : index =
  if List.mem_assoc f idx then acc else (f,h)::acc

(* perform a true merge if there are no merge conflicts by creating
 * a new commit that combines the states of the two branches *)
let true_merge (cur_ptr: string) (br_ptr: string) (branch: string) : unit =
  let cur = Object.parse_commit cur_ptr in
  let cur_idx = Tree.(read_tree "" cur.tree |> tree_to_index) in
  let br = Object.parse_commit br_ptr in
  let br_idx = Tree.(read_tree "" br.tree |> tree_to_index) in
  let anc = get_common_ancestor cur_ptr br_ptr |> Object.parse_commit in
  let anc_idx = Tree.(read_tree "" anc.tree |> tree_to_index) in
  let compare_base (acc,inc_f) (f,h) =
    let c_hash = List.assoc f cur_idx in
    let b_hash = List.assoc f br_idx in
    (* check if branches changed the same file *)
    match (h=c_hash,h=b_hash) with
    | (true,true)   -> ((f,h)::acc, inc_f)
    | (true,false)  -> ((f,b_hash)::acc, inc_f)
    | (false,true)  -> ((f,c_hash)::acc, inc_f)
    | (false,false) -> if c_hash = b_hash then ((f,c_hash)::acc, inc_f)
                       else (acc, f::inc_f)
  in
  let (merged_base,incomp_fs) = List.fold_left compare_base ([],[]) anc_idx in
  if incomp_fs = [] then
    let new_cur = List.fold_left (get_new_files merged_base) [] cur_idx in
    let new_br = List.fold_left (get_new_files merged_base) [] br_idx in
    (* merge the indexes, create a merge commit, and repopulate the repo, *)
    let merged_idx = merged_base @ new_cur @ new_br in
    let tree = Tree.index_to_tree merged_idx in
    let tree_hash = Tree.write_tree tree in
    let user = Utils.get_user_info () in
    let msg = "Merged branch '"^branch^"' into "^Branch.get_current_branch () in
    let tm = time () |> string_of_float in
    [cur_ptr;br_ptr] |> Object.create_commit tree_hash user tm msg |> Head.set_head;
    Index.set_index merged_idx; Tree.recreate_tree "" tree; Print.print msg
  else begin
    Print.print "Unable to merge branches because of the following incompatible files:\n";
    List.iter (fun f -> Print.print_indent f "y" 3) incomp_fs;
    Print.print "\nPlease resolve these conflicts so that the file versions match."
  end

(* perform fast-forward merge by updating the head to the branch head *)
let fast_forward_merge (branch: string) (ptr: string) : unit =
  Print.print ("Updating branch '" ^ branch ^ "' with fast-forward merge...");
  Head.set_head ptr; Index.switch_version true ptr;
  Print.print "\nMerge successful."
