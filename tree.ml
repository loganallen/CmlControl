open Unix
open Common

module type TreeSig = sig

  type t

  type index = Utils.index

  (* an empty Tree *)
  val empty : t
  (* inserts a file_name to hash mapping into the given tree *)
  val insert : t -> string * string -> t
  (* converts an index to a tree *)
  val index_to_tree : index -> t
  (* converts a tree to an index *)
  val tree_to_index : t -> index
  (* reads a tree from the filesystem and converts it to a tree *)
  val read_tree : string -> string -> t
  (* writes the tree to the filesystem *)
  val write_tree : t -> string
  (* updates working repo with tree content *)
  val recreate_tree : t -> unit

end

module Tree = struct

  type t = Blob of string * string | Tree of string * t list

  type index = Utils.index

  let empty : t = Tree (".", [])

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
              let _ = print_endline "here" in
              (insert (Tree (n, lst)) (file_name, hn))::(acc @ t)
            else raise Not_found
          with Not_found -> loop ((Tree (n, lst))::acc) (fn, hn) t
        end
      | (Blob (fx, hx))::t -> loop ((Blob (fx, hx))::acc) (fn, hn) t
    in match tree with
      | Tree (n, lst) -> Tree (n, loop [] (fn, hn) lst)
      | _ -> failwith "error"

  let index_to_tree (idx : index) : t =
    List.fold_left insert empty idx

  let tree_to_index (tree : t) : index =
    let rec helper path acc = function
      | [] -> acc
      | (Tree (n, lst))::t ->
          helper path (helper (if path = "" then n else path ^ "/" ^ n) [] lst) t
      | (Blob (fn, hn))::t -> helper path (((path ^ fn), hn)::acc) t
    in match tree with
      | Tree (n, lst) -> helper n [] lst
      | Blob _ -> []

  let parse_tree_line (raw : string) =
    let sp1 = 1 + String.index raw ' ' in
    let sp2 = String.index_from raw sp1 ' ' in
    let obj_type = String.sub raw 0 (sp1 - 1) in
    let obj_hash = String.sub raw sp1 (sp2 - sp1) in
    let obj_name = String.sub raw (sp2 + 1) (String.length raw - (sp2 + 1)) in
    (obj_type, obj_hash, obj_name)

  let rec read_tree (tree_name : string) (ptr : string) : t =
    let map_helper (dn, ptr) = read_tree dn ptr in
    let rec loop ic dirs acc =
      try
        let raw = input_line ic in
        let (obj_type, obj_hash, obj_name) = parse_tree_line raw in
        match obj_type with
          | "blob" -> loop ic dirs ((Blob (obj_name, obj_hash))::acc)
          | "tree" -> loop ic ((obj_name, obj_hash)::dirs) acc
          | _ -> failwith "read_tree: error"
      with
        End_of_file -> close_in ic; acc@(List.map map_helper dirs)
    in
    let ic = open_in (Utils.get_object_path ptr) in
    Tree(tree_name, loop ic [] [])

  let rec write_tree (tree : t) : string =
    let rec tree_data acc = function
      | [] -> acc
      | (Blob (fn,hn))::t -> tree_data (("blob " ^ hn ^ " " ^ fn)::acc) t
      | (Tree (n, lst))::t -> tree_data (("tree " ^ (write_tree (Tree (n, lst))) ^ " " ^ n)::acc) t
    in let rec write_lines ch = function
      | [] -> close_out ch;
      | h::t -> Printf.fprintf ch "%s\n" h; write_lines ch t
    in match tree with
      | Tree (n, lst) ->
        begin
          let temp_name = ".cml/temp_"^n in
          let ch = open_out temp_name in
          let _ = tree_data [] lst |> write_lines ch in
          let hsh = Utils.hash temp_name in
          let (d1,path) = split_hash hsh in
          if not (Sys.file_exists (".cml/objects/"^d1)) then
          mkdir (".cml/objects/"^d1) 0o777;
          Sys.rename temp_name path; hsh
        end
      | _ -> failwith "write-tree error"

    let recreate_tree (tree : t) : unit =
      ()

end
