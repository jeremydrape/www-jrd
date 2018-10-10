ln-local:
	rm -f Text WWW
	ln -s $(HOME)/sw/html-minus/Text Text
	ln -s $(HOME)/sw/www-minus/WWW WWW

mk-cgi:
	ghc -Wall -O2 --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi


all:	ln-local mk-cgi

clean:
	rm -f jrd-cgi *.hi *.o
