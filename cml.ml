let () =
  print_endline "cml started - below are the passed in args";
  for i = 1 to Array.length Sys.argv - 1 do
    print_endline Sys.argv.(i)
  done;;
