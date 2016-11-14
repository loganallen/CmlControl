open Command
open Print

let () =
  print_endline "cml started - below are the passed in args";
  for i = 1 to Array.length Sys.argv - 1 do
    print_color Sys.argv.(i) "yellow"
  done;;
