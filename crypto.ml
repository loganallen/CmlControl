open Unix
open Universal
open Cryptokit

(****************************** Crypto Module *********************************)
(******************************************************************************)

(* returns the string list of lines in the file_name
 * precondition: file_name exists from the cwd *)
let parse_lines file_name =
  let rec acc_lines acc ic =
    try begin
      let line = input_line ic in acc_lines (acc@[line]) ic
    end with
    | End_of_file -> close_in ic; acc
    | _ -> raise (Fatal ("Failure reading file '"^file_name^"'"))
  in
  try acc_lines [] (open_in file_name) with
  | _ -> raise (Fatal ("Failure reading file '"^file_name^"'"))

(* hash returns a SHA-1 hash of a given input *)
let hash (file_name : string) : string =
	try
		let fd = openfile file_name [O_RDONLY] 0o777 in
		let channel = in_channel_of_descr fd in
		let hash = hash_channel (Hash.sha1 ()) channel in
		close_in channel; transform_string (Hexa.encode ()) hash
	with
		Unix_error (Unix.ENOENT,_,_) -> raise (Fatal ("Could not find file: "^file_name))

(* compresses a file/directory to the specified destination *)
let compress (file_name : string) (path : string) : unit =
  try begin
    let ic = open_in file_name in
    let oc = open_out path in
    let compress = Cryptokit.Zlib.compress () in
    Cryptokit.transform_channel compress ic oc; close_in ic; close_out oc
  end with
    | _ -> raise (Fatal ("Failure compressing '"^file_name^"'"))

(* returns the compressed [str] *)
let compress_string str =
    Cryptokit.transform_string (Cryptokit.Zlib.compress ()) str

(* decompresses a file/directory to the specified destination *)
let decompress (file_name : string) (dest_path : string) : unit =
  try begin
    let ic = open_in file_name in
    let oc = open_out dest_path in
    let uncompress = Cryptokit.Zlib.uncompress () in
    Cryptokit.transform_channel uncompress ic oc; close_in ic; close_out oc
  end with
    | _ -> raise (Fatal ("Failure uncompressing '"^file_name^"'"))

(* returns the decompressed [str] *)
let decompress_string str =
    Cryptokit.transform_string (Cryptokit.Zlib.uncompress ()) str

(* returns the string list of lines in the decompressed file given the
 * file_name of a compressed file *)
let decompress_contents file_name =
  try begin
    let decomp_name = file_name^"uncmlpressed" in
      decompress file_name decomp_name;
    let file_lines = parse_lines decomp_name in
      Sys.remove decomp_name; file_lines
  end with
  | _ -> raise (Fatal ("Failure decompressing file '"^file_name^"'"))

(* [copy filename destination] creates exact copy of filename at destination *)
let copy (file_name : string) (dest_path : string) : unit =
  let rec loop ic oc =
    try Printf.fprintf oc ("%s\n") (input_line ic); loop ic oc
    with End_of_file -> close_in ic; close_out oc
  in try
    let ic = open_in file_name in
    let oc = open_out dest_path in loop ic oc
  with
    Sys_error _ -> raise (Fatal "utils.copy, file not found")
