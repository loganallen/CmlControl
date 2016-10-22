######To Compile Ocaml

ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o v.byte v.ml

######To Compile JS
js_of_ocaml v.byte
