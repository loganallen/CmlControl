install:
	ocamlbuild -pkgs unix,ANSITerminal,cryptokit,camlzip,str cml.byte
	cp cml /usr/local/bin
	cp cml.byte /usr/local/bin

uninstall:
	rm /usr/local/bin/cml
	rm /usr/local/bin/cml.byte

clean:
	ocamlbuild -clean
