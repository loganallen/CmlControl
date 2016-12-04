(******************************* Tree Module **********************************)
(******************************************************************************)

module type TreeSig = sig

  type t

  (* type index is the same as defined in Utils *)
  type index = (string * string) list

  (* an empty Tree *)
  val empty : t

  (* [insert (fn, hn)] inserts a Blob into the tree. *)
  val insert : t -> string * string -> t

  (* [index_to_tree idx] returns a tree from a Utils.index *)
  val index_to_tree : index -> t

  (* [tree_to_index tree] returns a Util.index from a tree *)
  val tree_to_index : t -> index

  (* [read_tree ptr] reads a tree from the filesystem and converts it to type t *)
  val read_tree : string -> string -> t

  (* [write_tree tree] writes a tree to the filesystem *)
  val write_tree : t -> string

  (* [recreate_tree nm tree] updates working repo with tree content *)
  val recreate_tree : string -> t -> unit

end

module Tree : TreeSig
