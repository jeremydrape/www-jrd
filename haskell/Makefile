f:
	ghc --make jrd-f
	jrd-f
	#ghci -Wall jrd-f.hs -e main

a:
	ghci -Wall jrd-a.hs -e main

clean:
	rm -f *.hi *.o jrd-f
	rm -R ../f/[0-9]*
