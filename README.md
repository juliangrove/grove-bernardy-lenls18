# grove-bernardy-lenls18

Code for [Grove and Bernardy, 2021](https://ling.auf.net/lingbuzz/006284). You
can do:

	git clone --recurse-submodules https://github.com/juliangrove/grove-bernardy-lenls18
	cd grove-bernardy-lenls18
	cabal v2-repl

If you have Nix, but not Cabal, you can do:

	nix-shell --run "cabal v2-update && cabal v2-repl"

The two concrete examples from the paper can be found in
[Fragment.hs](https://github.com/juliangrove/grove-bernardy-lenls18/blob/main/src/Fragment/Fragment.hs). 
