all:
	ghc -Wall -O2 --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi

clean:
	rm -f jrd-cgi *.hi *.o

ln-local:
	rm -f Text WWW
	ln -s $(HOME)/sw/html-minus/Text Text
	ln -s $(HOME)/sw/www-minus/WWW WWW
