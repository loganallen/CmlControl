let print_diff (diff : Odiff.diff) : unit =
  match diff with
  | Odiff.Add _ -> Print.print_color (Odiff.string_of_diff diff) "g"
  | Odiff.Delete _ -> Print.print_color (Odiff.string_of_diff diff) "r"
  | Odiff.Change _ -> Print.print_color (Odiff.string_of_diff diff) "y"

(* prints a diff between two files to console *)
let diff_file (new_file : string) (old_file : string) : unit =
  let diffs = Odiff.files_diffs old_file new_file in
  if diffs = [] then () else
    Print.print_color ("diff --cml " ^ new_file) "";
    List.iter print_diff diffs

(* prints diffs all pairs of files in a list *)
let rec diff_mult (lst : (string * string) list) : unit =
  match lst with
    | [] -> ()
    | (old_file,new_file)::[] -> diff_file old_file new_file
    | (old_file,new_file)::t  -> begin
        diff_file new_file old_file;
        Print.print_newline (); Print.print "--------------------";
        Print.print_newline (); diff_mult t
      end
