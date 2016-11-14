install:
	ocamlbuild -pkgs unix cml.byte
	cp cml /usr/local/bin
	cp cml.byte /usr/local/bin

clean:
	ocamlbuild -clean
