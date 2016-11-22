open Command
open Print

(* parse Cml command line request and execute the respective command *)
let () =
 	parse_input (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) |> execute
