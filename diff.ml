open Universal

type file_data = {
  file_path : string;
  compressed : bool;
}

type diff_input = {
  name : string;
  old_file_data : file_data option;
  new_file_data : file_data option;
}

(* prints a single diff line with color *)
let print_diff (dif : Odiff.diff) : unit =
  match dif with
  | Odiff.Add _ -> Print.print_color (Odiff.string_of_diff dif) "g"
  | Odiff.Delete _ -> Print.print_color (Odiff.string_of_diff dif) "r"
  | Odiff.Change _ -> Print.print_color (Odiff.string_of_diff dif) "y"

(* prints a diff between one decompressed file and one compressed file *)
let diff_vs_blob (normal : string) (blob : string) : Odiff.diffs =
  let temp = ".cml/objects/temp_blob" in
  Crypto.decompress blob temp;
  let file_diff = Odiff.files_diffs normal temp in
  Sys.remove temp;
  file_diff

(* prints a diff between one decompressed file and one compressed file *)
let blob_vs_diff (blob : string) (normal : string) : Odiff.diffs =
  let temp = ".cml/objects/temp_blob" in
  Crypto.decompress blob temp;
  let file_diff = Odiff.files_diffs temp normal in
  Sys.remove temp;
  file_diff

(* prints a diff between two compressed files *)
let diff_blobs (blob1 : string) (blob2 : string) : Odiff.diffs =
  let temp1 = ".cml/objects/temp_blob1" in
  let temp2 = ".cml/objects/temp_blob2" in
  Crypto.decompress blob1 temp1; Crypto.decompress blob2 temp2;
  let file_diffs = Odiff.files_diffs temp1 temp2 in
  Sys.remove temp1; Sys.remove temp2;
  file_diffs

(* [diff (file1, iscompressed1) (file2, iscompressed2)] prints a diff between
 * two files that can be compressed or decompressed *)
let diff old_file new_file : Odiff.diffs =
  match (old_file.compressed, new_file.compressed) with
    | (false, false) -> Odiff.files_diffs old_file.file_path new_file.file_path
    | (false, true) -> diff_vs_blob old_file.file_path new_file.file_path
    | (true, false) -> blob_vs_diff old_file.file_path new_file.file_path
    | (true, true) -> diff_blobs old_file.file_path new_file.file_path

let print_separator do_print =
  if do_print then begin
    Print.print "--------------------";
  end else
    ()

(* prints diffs for all pairs of files in a list [lst] *)
let rec diff_mult (lst : diff_input list) : unit =
  let acc_diff acc input = begin
    match input with
    | { name; old_file_data = None; } -> begin
      print_separator acc;
      Print.print_color ("added:   "^name) "green";
      true
    end
    | { name; new_file_data = None; } -> begin
      print_separator acc;
      Print.print_color ("deleted: "^name) "red";
      true
    end
    | { name; old_file_data = Some old_file; new_file_data = Some new_file; } -> begin
      let file_diffs = diff old_file new_file in
      if file_diffs = [] then
        acc
      else begin
        print_separator acc;
        Print.print_color ("diff --cml "^name) "";
        List.iter print_diff file_diffs;
        true
      end
    end
  end in
  let _ = List.fold_left acc_diff false lst in ()

(* Convert an index into an associate list with file_data *)
let index_to_diff_index compressed idx =
  idx |> List.map (fun (file,hash) ->
    let file_path = if compressed then get_object_path hash else file in
    (file, { file_path = file_path; compressed = compressed }))

(* Print the diff of two diff input lists *)
let diff_indexes old_diff_idx new_diff_idx =
  let acc_diff_inputs acc (new_filename, new_file_data) =
    if List.mem_assoc new_filename old_diff_idx then
      let old_file_data = List.assoc new_filename old_diff_idx in
      { name = new_filename;
        old_file_data = Some old_file_data;
        new_file_data = Some new_file_data;
      } :: acc
    else
      { name = new_filename;
        old_file_data = None;
        new_file_data = Some new_file_data;
      } :: acc
  in
  let acc_removed_diff_inputs acc (old_filename, old_file_data) =
    if List.mem_assoc old_filename new_diff_idx then
      acc
    else
      { name = old_filename;
        old_file_data = Some old_file_data;
        new_file_data = None;
      } :: acc
  in
  let new_diff_inputs = List.fold_left acc_diff_inputs [] new_diff_idx in
  List.fold_left acc_removed_diff_inputs new_diff_inputs old_diff_idx
  |> diff_mult
