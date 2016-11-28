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

module Tree : TreeSig
