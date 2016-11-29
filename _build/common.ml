
(* ($) is an infix operator for appending a char to a string *)
let ($) (str : string) (c : char) : string =  str ^ Char.escaped c

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