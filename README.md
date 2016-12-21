# Cml Control

*An OCaml-based management tool for local version control.*

### Installation
Download source code from this repo.  
For compilation, you need to install the following dependecies:

#### cryptokit

Install cryptokit: `opam install cryptokit`  
It is possible and even likely that you will receive an error.  
To fix run: `opam depext conf-zlib.1`  
Answer yes to all of the prompts and then after completion  
Rerun: `opam install cryptokit`  


#### ANSITerminal

Install ANSITerminal: `opam install ANSITerminal`  


#### ocamldiff

Install ocamldiff: `opam install ocamldiff`  


### Compilation

Open the src directory.

Run: `make`

--this will compile the program and attempt to copy the cml bash script and
cml byte code to the /usr/bin/local folder so that CmlControl can be run 
from any directory on your system.

To uninstall CmlControl from your system. run: `make uninstall`



You have now successfully compiled our project and are able to experience the wonders of CmlControl.

#### To begin:

Open a directory on you machine and run: `cml init`  
-- this will initialize your directory as a cml repository

Run `cml` or `cml help` for more information, or read our design document.

### Development

Run: `make clean` to remove compiled OCaml files before committing to the development repository.

Run: `make uninstall` to remove `cml` from your path entirely.

**Run:** `rm -rf .cml/` **before committing if you were testing** `cml` **in the local repo.**

### How To Use Git!!

When working on a feature use the following protocol:

1. `git checkout master` and `git pull` to update your local master
2. `git checkout -b name/feature` will create a branch off of master and move you to this branch
3. work on your shit: `git add`, `git commit`, and `git push origin name/feature` to push the code to the remote branch
4. when you're DONE with your feature, you're almost ready for a PR (pull request). Master may have been updated since you branched, so we need to account for this...
5. `git checkout master` -> `git pull` -> `git checkout name/feature` -> `git merge master`
6. Resolve any conflicts, then `git push origin name/feature` to push up to github
8. Go to github.com, to your branch, and click `make pull request` and **assign us to take a look at it/let us know.**
9. After comments have been made, make necessary changes locally (`git add`, `git commit`) then `git push origin name/feature` one last time. Go to github and it should say "no merge conflicts, ready to merge" or something, or else you'll have to pull master again and merge into your dev branch, resolve, and push to origin name/feature.
10. Now you can merge: `git checkout master` and `git merge name/feature` will merge your code into master, then `git push` will push to origin/master. Or simply click the green button on your pull request on github.com. Done!
