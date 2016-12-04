How to compile and run CmlControl by Andrew Grossfeld, Logan Allen, Brandon Walker, and Michael Gingras

For compilation, you need to install the following dependecies.

 ~ cryptokit

	Install cryptokit: opam install cryptokit
	It is possible and even likely that you will receive an error.
	To fix run: opam depext conf-zlib.1
	Answer yes to all of the prompts and then after completion, rerun: opam install cryptokit


 ~ ANSITerminal

 	Install ANSITerminal: opam install ANSITerminal


 ~ ocamldiff

 	Install ocamldiff: opam install ocamldiff


Now you have all of the depenecies you need. Open up the src directory.

	run: make

	-- this will compile the program and attempt to copy the cml bash script and
	   cml byte code to the /usr/bin/local folder so that CmlControl can be run 
	   from any directory on your system.

	To uninstall CmlControl from your system. run: make uninstall



Now you have successfully compiled CmlControl. You can now run our project.

To start:
	
	- open a directory on you machine and run: cml init
		--> this will initialize your directory as a cml repository

	- to run more commands, type cml or cml help for more information, or read our design document.
