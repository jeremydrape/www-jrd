all:
	ghc -Wall -fwarn-tabs --make jrd-cgi.hs
	strip -s jrd-cgi
	cp jrd-cgi ../index.cgi

clean:
	rm -f jrd-cgi *.hi *.o

ln-local:
	rm -f HTML WWW
	ln -s $(HOME)/sw/html-minus/HTML HTML
	ln -s $(HOME)/sw/www-minus/WWW WWW
