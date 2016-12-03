(***************************** Cryptokit Module *******************************)
(******************************************************************************)

(* returns the string list of lines in the file_name
 * precondition: file_name exists from the cwd *)
val parse_lines : string -> string list

(* hash returns a SHA-1 hash of a given input *)
val hash : string -> string

(* compresses a file/directory to the specified destination *)
val compress: string -> string -> unit

(* returns the compressed [in_string] *)
val compress_string : string -> string

(* decompresses a file/directory to the specified destination *)
val decompress: string -> string -> unit

(* returns the decompressed [in_string] *)
val decompress_string : string -> string

(* returns the string list of lines in the decompressed file given the
 * file_name of a compressed file *)
val decompress_contents : string -> string list

(* THIS FUNCTION IS NOT USED ANYWHERE *)
(* copy creates copy of a file in a new destination *)
val copy: string -> string -> unit
