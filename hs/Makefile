ln-local:
	rm -f Text/HTML Text/CSV WWW
	mkdir -p Text
	ln -s $(HOME)/sw/html-minus/Text/HTML Text/HTML
	ln -s $(HOME)/opt/src/lazy-csv-0.5.1/Text/CSV Text/CSV
	ln -s $(HOME)/sw/www-minus/WWW WWW

mk-cgi:
	ghc -Wall -O2 --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi


all:	ln-local mk-cgi

clean:
	rm -f jrd-cgi *.hi *.o
