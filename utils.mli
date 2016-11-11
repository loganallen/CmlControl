(* Utils Module --- helper functions for directory data structure *)


(* a record representing all the information about a commit type.
 * note: pointers in this case are string file names.
 *)
type commit = {
  author: string;
  message: string;
  date: string;
  tree_ptr: string;
  prev_commit_ptr: string;
}

(* type blob represents a compressed file object *)
type blob = string
(* type tree represents a directory with pointers to blobs/other trees
 * note: pointers in this case are string file names
 *)
type tree = string list

(* type mapping is a mapping between a filename and its hash *)
type mapping = string * string
(* type index is a list of mappings, referred to as the index in git *)
type index = mapping list

(* a variant for an cml object, which can be a Blob, Tree, or Commit  *)
type obj = Blob of blob | Tree of tree | Commit of commit


(* hash returns a SHA-1 hash of a given input *)
val hash : string -> string
(* compress compresses a file/directory
 * takes initial path and final path as arguments.
 *)
val compress: string -> string -> unit
(* decompress decompresses a file/directory
 * takes initial and final path as arguments.
 *)
val decompress: string -> string -> unit
(* creates and object in the object directory and returns its name (hashed) *)
val create_obj: obj -> string
(* takes hash and returns an object type *)
val parse_obj: string -> obj
(* returns the current HEAD of the cml repository *)
val get_head: unit -> string
(* sets the HEAD of the cml repository *)
val set_head: string -> unit
(* returns the HASH of a head of the given branch *)
val get_branch_ptr: string -> string
(* initializes a given commit to a given branch name *)
val set_branch_ptr: string -> string -> unit
(* updates the index by adding a mapping type *)
val update_index: index -> mapping -> index
(* returns teh index *)
val get_index: unit -> index
(* initializes an index in the cml directory *)
val set_index: index -> unit
