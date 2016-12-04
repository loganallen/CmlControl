open Unix
open Universal

(******************************* Tree Module **********************************)
(******************************************************************************)

(* TreeSig: same as defined in tree.mli *)
module type TreeSig = sig

  type t

  type index = (string * string) list

  val empty : t
  val insert : t -> string * string -> t
  val index_to_tree : index -> t
  val tree_to_index : t -> index
  val read_tree : string -> string -> t
  val write_tree : t -> string
  val recreate_tree : string -> t -> unit

end

module Tree = struct

  type t = Blob of string * string | Tree of string * t list

  type index = (string * string) list

  (* an empty Tree *)
  let empty : t = Tree ("", [])

  (* [insert (fn, hn)] inserts a Blob into the tree.
   * precondition: tree is a variant Tree, otherwise exception is thrown *)
  let rec insert (tree : t) (fn, hn) : t =
    let rec loop acc (fn, hn) = function
      | [] ->
        begin
          try
            let (dir_name, file_name) = split_path fn in
            (Tree (dir_name, [Blob(file_name, hn)]))::acc
          with Not_found -> Blob (fn, hn)::acc
        end
      | (Tree (n, lst))::t ->
        begin
          try
            let (dir_name, file_name) = split_path fn in
            if dir_name = n then
              (insert (Tree (n, lst)) (file_name, hn))::(acc @ t)
            else raise Not_found
          with Not_found -> loop ((Tree (n, lst))::acc) (fn, hn) t
        end
      | (Blob (fx, hx))::t -> loop ((Blob (fx, hx))::acc) (fn, hn) t
    in match tree with
      | Tree (n, lst) -> Tree (n, loop [] (fn, hn) lst)
      | _ -> raise Corrupt

  let index_to_tree (idx : index) : t =
    List.fold_left insert empty idx

  (* [tree_to_index tree] returns a Util.index from a tree
   * precondition: tree is of variant type Tree, else returns empty index *)
  let tree_to_index (tree : t) : index =
    let rec helper path acc = function
      | [] -> acc
      | (Tree (dn, lst))::t ->
          let path' = if path = "" then dn else path^"/"^dn in
          helper path ((helper path' [] lst)@acc) t
      | (Blob (fn, hn))::t ->
          let path' = if path = "" then fn else path^"/"^fn in
          helper path ((path', hn)::acc) t
    in match tree with
      | Tree (n, lst) -> helper n [] lst
      | Blob _ -> []

  (* [parse_tree_line str] is a helper for the tree module
   * precondition: str is matched by ("tree"||"blob") ^ " " ^ hash ^ " " ^ name
   * returns tuple with parsed data *)
  let parse_tree_line (raw : string) =
    let sp1 = 1 + String.index raw ' ' in
    let sp2 = String.index_from raw sp1 ' ' in
    let obj_type = String.sub raw 0 (sp1 - 1) in
    let obj_hash = String.sub raw sp1 (sp2 - sp1) in
    let obj_name = String.sub raw (sp2 + 1) (String.length raw - (sp2 + 1)) in
    (obj_type, obj_hash, obj_name)

  (* [read_tree ptr] reads a tree from the filesystem and converts it to type t *)
  let rec read_tree (tree_name : string) (ptr : string) : t =
    let map_helper (dn, ptr) = read_tree dn ptr in
    let rec loop ic dirs acc =
      try
        let raw = input_line ic in
        let (obj_type, obj_hash, obj_name) = parse_tree_line raw in
        match obj_type with
          | "blob" -> loop ic dirs ((Blob (obj_name, obj_hash))::acc)
          | "tree" -> loop ic ((obj_name, obj_hash)::dirs) acc
          | _ -> raise Corrupt
      with
      | _ -> close_in ic; acc@(List.map map_helper dirs)
    in
    let ic = open_in (get_object_path ptr) in
    Tree(tree_name, loop ic [] [])

  (* [write_tree tree] writes a tree to the filesystem
   * precondition: tree is a variant type tree *)
  let rec write_tree (tree : t) : string =
    let rec tree_data acc = function
      | [] -> acc
      | (Blob (fn,hn))::t -> tree_data (("blob " ^ hn ^ " " ^ fn)::acc) t
      | (Tree (n, lst))::t -> tree_data (("tree " ^
                                (write_tree (Tree (n, lst))) ^ " " ^ n)::acc) t
    in
    let rec write_lines ch = function
      | [] -> close_out ch;
      | h::t -> Printf.fprintf ch "%s\n" h; write_lines ch t
    in
    match tree with
      | Tree (n, lst) ->
        begin
          let temp_name = ".cml/temp_"^n in
          let ch = open_out temp_name in
          let _ = lst |> tree_data [] |> write_lines ch in
          let hsh = Crypto.hash temp_name in
          let (d1,path) = split_hash hsh in
          if not (Sys.file_exists (".cml/objects/"^d1)) then
          mkdir (".cml/objects/"^d1) 0o777;
          Sys.rename temp_name path; hsh
        end
      | _ -> raise Corrupt

  (* [recreate_tree nm tree] updates working repo with tree content *)
  let rec recreate_tree (path : string) (tree : t) : unit =
    match tree with
      | Blob (fn, hsh) -> Crypto.decompress (get_object_path hsh)
                                    (if path = "" then fn else path ^ "/" ^ fn)
      | Tree (dn, lst) ->
        begin
          let np = if path = "" then dn else path ^ "/" ^ dn in
          if not (Sys.file_exists np) && np <> "" then mkdir np 0o777;
          List.iter (recreate_tree np) lst
        end

end
