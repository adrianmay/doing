/usr/bin/did: did doing
	sudo cp did doing /usr/bin

did: did.hs Makefile
	stack --resolver lts-22.37 ghc \
		--package split \
		--package iso8601-time \
		--package groupBy \
		-- did.hs


