install:
	ocamlbuild -pkgs unix,ANSITerminal cml.byte
	cp cml /usr/local/bin
	cp cml.byte /usr/local/bin

clean:
	ocamlbuild -clean
