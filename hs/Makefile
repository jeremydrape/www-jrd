ln-local:
	rm -f Text/CSV Text/Html Text/XML Www
	mkdir -p Text
	ln -s $(HOME)/opt/src/lazy-csv-0.5.1/Text/CSV Text/CSV
	ln -s $(HOME)/opt/src/xml-1.3.9/Text/XML Text/XML
	ln -s $(HOME)/sw/html-minus/Text/Html Text/Html
	ln -s $(HOME)/sw/www-minus/Www Www

mk-cgi:
	ghc -Wall -O2 --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi


all:	ln-local mk-cgi

clean:
	rm -f jrd-cgi *.hi *.o
