exception Fatal of string

(* returns a pairs (d1,path) where [d1] is the first 2 chars of the hash
 * and [path] is the .cml/objects path of the hash *)
let split_hash (hash : string) : string * string =
  let d1 = String.sub hash 0 2 in
  let f1 = String.sub hash 2 (String.length hash - 2) in
  let path = ".cml/objects/"^d1^"/"^f1 in
    (d1,path)

(* returns (dir,file_name) for any string with the format "dir/fn" *)
let split_path (file : string) : string * string =
  let split = String.index file '/' in
  let dir = String.sub file 0 split in
  let file_name = String.sub file (split + 1) (String.length file - split - 1) in
    (dir,file_name)

(* returns the path of an object with file name hash
 * precondition: hash is a valid  40 char string *)
let get_object_path (hash : string) =
  let root = ".cml/objects/" in
  let subdir = String.sub hash 0 2 in
  let fn = String.sub hash 2 (String.length hash - 2) in
  let path = root ^ subdir ^ "/" ^ fn in
  if Sys.file_exists path then path
  else raise (Fatal ("tree - "^hash^": doesn't exist"))
