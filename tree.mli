module type TreeSig = sig

  type t

  type index = (string * string) list

  val empty : t

  val insert : t -> string * string -> t

  val index_to_tree : index -> t

  val tree_to_index : t -> index

  val write_tree : t -> string

end

module Tree : TreeSig
