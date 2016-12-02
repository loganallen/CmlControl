open Common

(* prints a single diff line with color *)
let print_diff (dif : Odiff.diff) : unit =
  match dif with
  | Odiff.Add _ -> Print.print_color (Odiff.string_of_diff dif) "g"
  | Odiff.Delete _ -> Print.print_color (Odiff.string_of_diff dif) "r"
  | Odiff.Change _ -> Print.print_color (Odiff.string_of_diff dif) "y"

(* prints the diffs made from two files *)
let diff_file (new_file : string) (old_file : string) : unit =
  let diffs = Odiff.files_diffs old_file new_file in
  if diffs = [] then () else
    Print.print_color ("diff --cml " ^ new_file) "";
    List.iter print_diff diffs

(* prints a diff between one decompressed file and one compressed file *)
let diff_vs_blob (normal : string) (blob : string) : unit =
  let temp = ".cml/objects/temp_blob" in
  Crypto.decompress blob temp;
  diff_file normal temp;
  Sys.remove temp

(* prints a diff between two compressed files *)
let diff_blobs (blob1 : string) (blob2 : string) : unit =
  let temp1 = ".cml/objects/temp_blob1" in
  let temp2 = ".cml/objects/temp_blob2" in
  Crypto.decompress blob1 temp1; Crypto.decompress blob2 temp2;
  diff_file temp1 temp2;
  Sys.remove temp1; Sys.remove temp2

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
let diff (file1, c1) (file2, c2) : unit =
  match (c1, c2) with
    | (false, false) -> diff_file file1 file2
    | (false, true) -> diff_vs_blob file1 file2
    | (true, false) -> diff_vs_blob file2 file1
    | (true, true) -> diff_blobs file1 file2

(* prints diffs for all pairs of files in a list [lst] *)
let rec diff_mult (lst : ((string * bool) * (string * bool)) list) : unit =
  match lst with
    | [] -> ()
    | (new_file,old_file)::[] -> diff new_file old_file
    | (new_file,old_file)::t  -> begin
        diff new_file old_file;
        Print.print_newline (); Print.print "--------------------";
        Print.print_newline (); diff_mult t
      end

