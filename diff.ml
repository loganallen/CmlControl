(* prints a diff between two files to console *)
let print_diff_files (mine : string) (other : string) : unit =
  let diffs = Odiff.files_diffs mine other in
  if diffs = [] then () else
    Print.print_color ("--" ^ mine) "b";
    Odiff.print_diffs Pervasives.stdout diffs

(* prints diffs all pairs of files in a list *)
let rec print_diff_files_mult (lst : (string * string) list) : unit =
  match lst with
    | [] -> ()
    | (mine, other)::[] -> print_diff_files mine other
    | (mine, other)::t -> print_diff_files mine other;
                          Print.print_newline (); 
                          Print.print_color "--------------------" "r";
                          Print.print_newline (); print_diff_files_mult t
