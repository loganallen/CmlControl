install:
	ocamlbuild -pkgs diff,unix,ANSITerminal,cryptokit,str cml.byte
	sudo cp cml /usr/local/bin
	sudo cp cml.byte /usr/local/bin

uninstall:
	sudo rm /usr/local/bin/cml
	sudo rm /usr/local/bin/cml.byte

clean:
	ocamlbuild -clean
