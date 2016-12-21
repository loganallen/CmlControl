# Cml Control

*An OCaml-based management tool for local version control.*

*Authors:* <br>
Logan Allen, Andrew Grossfeld, Brandon Walker, and Michael Gingras

## Installation
Download source code from this repo.  
For compilation, you need to install the following dependecies:

#### cryptokit

Install cryptokit: `opam install cryptokit`  
- It is possible and even likely that you will receive an error.  
To fix this issue run: `opam depext conf-zlib.1`  
- Answer yes to all of the prompts and then after completion  
Re-run: `opam install cryptokit`  


#### ANSITerminal

Install ANSITerminal: `opam install ANSITerminal`  


#### ocamldiff

Install ocamldiff: `opam install ocamldiff`  

## Compilation

Navigate to the CmlControl src directory in terminal.

Run: `make` <br>
- This will compile the program and attempt to copy the cml bash script and
cml byte code to the /usr/bin/local folder so that CmlControl can be run 
from any directory on your system.

To *uninstall* CmlControl from your system. run: `make uninstall`

You have now successfully compiled our project and are able to experience the wonders of CmlControl!

#### To begin:

Open a directory on you machine and run: `cml init`  
- This will initialize your directory as a cml repository

Run `cml` or `cml help` for more information, or read our design document.

## Development

This project is Open Source! Feel free to explore the source code and contribute via pull requests.

Adhere to the following guidelines when developing CmlControl:

Run: `make clean` to remove compiled OCaml files before committing to the development repository.

Run: `make uninstall` to remove `cml` from your path entirely.

**Run:** `rm -rf .cml/` **before committing if you were testing** `cml` **in the local repo.**
