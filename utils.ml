type commit = {
  author: string;
  message: string;
  date: string;
  tree_ptr: string;
  prev_commit_ptr: string;
}

type blob = string
type tree = string list
type mapping = string * string
type index = mapping list

type obj = Blob of blob | Tree of tree | Commit of commit


(* hash returns a SHA-1 hash of a given input *)
let hash (file_name : string) : string =
  failwith "Unimplemented"

(* compress compresses a file/directory
 * takes initial path and final path as arguments.
 *)
let compress (file_name : string) (dest_path : string) : unit =
  failwith "Unimplemented"

(* decompress decompresses a file/directory
 * takes initial and final path as arguments.
 *)
let decompress (file_name : string) (dest_path : string) : unit =
  failwith "Unimplemented"

(* creates and object in the object directory and returns its name (hashed) *)
let create_obj (obj : obj) : string =
  failwith "Unimplemented"

(* takes hash and returns an object type *)
let parse_obj (file_name : string) : obj =
  failwith "Unimplemented"

(* returns the current HEAD of the cml repository *)
let get_head () : string =
  failwith "Unimplemented"

(* sets the HEAD of the cml repository *)
let set_head (commit_hash : string) : unit =
  failwith "Unimplemented"

(* returns the HASH of a head of the given branch *)
let get_branch_ptr (branch_name : string) : string =
  failwith "Unimplemented"

(* initializes a given commit to a given branch name *)
let set_branch_ptr (branch_name : string) (commit_hash : string) : unit =
  failwith "Unimplemented"

(* updates the index by adding a mapping type *)
let update_index (idx : index) (map : mapping) : index =
  failwith "Unimplemented"

(* returns teh index *)
let get_index () : index =
  failwith "Unimplemented"

(* initializes an index in the cml directory *)
let set_index (idx : index) : unit =
  failwith "Unimplemented"
