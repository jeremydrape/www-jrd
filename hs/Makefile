all:
	ghc -Wall -fwarn-tabs --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi

clean:
	rm -f jrd-cgi *.hi *.o

ln-local:
	rm -f WWW
	ln -s $(HOME)/sw/www-minus/WWW WWW
