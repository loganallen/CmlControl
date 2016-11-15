open Command
open Print

let () =
  print_endline "cml started - below are the passed in args";
 	let cmd = parse_input Sys.argv in
  execute cmd
