(* Cml main file *)
open Command
open Print

(* parse [cml] command line request and execute the respective command *)
let () =
  try
 	  parse_input (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) |> execute
  with
  | Parsing cmd -> print ("cml: '"^cmd^"' is not a cml command. See cml help.")
