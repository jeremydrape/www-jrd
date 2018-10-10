ln-local:
	rm -f Text/CSV Text/HTML Text/XML WWW
	mkdir -p Text
	ln -s $(HOME)/sw/html-minus/Text/HTML Text/HTML
	ln -s $(HOME)/opt/src/lazy-csv-0.5.1/Text/CSV Text/CSV
	ln -s $(HOME)/opt/src/xml-1.3.9/Text/XML Text/XML
	ln -s $(HOME)/sw/www-minus/WWW WWW

mk-cgi:
	ghc -Wall -O2 --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi


all:	ln-local mk-cgi

clean:
	rm -f jrd-cgi *.hi *.o
