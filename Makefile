/usr/bin/did: did doing
	sudo cp doing `stack path --local-bin`
	stack install

did: Main.hs Makefile
	cabal build


