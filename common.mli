(* Fatal exception for internal cml execution errors *)
exception Fatal of string

(* returns a pairs (d1,path) where [d1] is the first 2 chars of the hash
 * and [path] is the .cml/objects path of the hash *)
val split_hash: string -> string * string

(* returns (dir,file_name) for any string with the format "dir/fn" *)
val split_path: string -> string * string

(* returns the path of an object represented by hash
 * precondition: hash is a valid  40 char string
 * postcondition: get_object_path raise Fatal if hash doens't exist *)
 val get_object_path : string -> string
