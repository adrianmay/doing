/usr/bin/did: did doing
	sudo cp did doing /usr/bin
	

did: did.hs Makefile
	stack ghc \
		--package split \
	  --package iso8601-time \
		--package groupBy \
	  -- did.hs


