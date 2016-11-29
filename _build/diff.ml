let diff_file (old_file : string) (new_file : string) : unit =
  let diffs = Odiff.files_diffs old_file new_file in
  if diffs = [] then () else
    Print.print_color ("--" ^ old_file) "b";
    Odiff.print_diffs Pervasives.stdout diffs

let rec diff_mult (lst : (string * string) list) : unit =
  match lst with
    | [] -> ()
    | (old_file,new_file)::[] -> diff_file old_file new_file
    | (old_file,new_file)::t  -> begin
        diff_file old_file new_file;
        Print.print_newline (); Print.print_color "--------------------" "r";
        Print.print_newline (); diff_mult t
      end
