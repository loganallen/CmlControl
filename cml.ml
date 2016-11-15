open Command
open Print

let () =
 	let cmd = parse_input (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
  execute cmd
