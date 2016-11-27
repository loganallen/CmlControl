open Unix

module type TreeSig = sig

  type t

  type index = (string * string) list

  val empty : t

  val insert : t -> string * string -> t

  val index_to_tree : index -> t

  val tree_to_index : t -> index

  val read_tree : string -> t

  val write_tree : t -> string

end

module Tree = struct

  type t = Blob of string * string | Tree of string * t list

  type index = (string * string) list

  let empty = Tree (".", [])

  (* returns (dir_name, file_name) for any string with the format "dir_name/file_name" *)
  let split_path fn =
    let split = String.index fn '/' in
    let dir_name = String.sub fn 0 split in
    let file_name = String.sub fn (split + 1) (String.length fn - split - 1) in
    (dir_name, file_name)

  let rec insert tree (fn, hn) =
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

  let index_to_tree (idx : index) =
    List.fold_left insert empty idx

  let tree_to_index (tree : t) = []

  let read_tree (ptr : string) = empty

  let rec write_tree tree =
    let rec tree_data acc = function
      | [] -> acc
      | (Blob (fn,hn))::t -> tree_data (("blob " ^ hn ^ " " ^ fn)::acc) t
      | (Tree (n, lst))::t -> tree_data (("tree " ^ (write_tree (Tree (n, lst))) ^ " " ^ n)::acc) t
    in let rec write_lines oc = function
      | [] -> close_out oc;
      | h::t -> Printf.fprintf oc "%s\n" h; write_lines oc t
    in match tree with
      | Tree (n, lst) ->
        begin
          let temp_name = ".cml/temp_"^n in
          let oc = open_out temp_name in
          let _ = write_lines oc (tree_data [] lst) in
          let hsh = Utils.hash temp_name in
          let d1 = String.sub hsh 0 2 in
          if not (Sys.file_exists (".cml/objects/"^d1)) then
          mkdir (".cml/objects/"^d1) 0o777;
          let f1 = String.sub hsh 2 (String.length hsh -2) in
          let path = ".cml/objects/"^d1^"/"^f1 in
          Sys.rename temp_name path; (d1 ^ f1)
        end
      | _ -> failwith "write-tree error"

end
