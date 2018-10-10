mk-hs:
	(cd hs; make all)

mk-convert:
	(cd data/jpeg; make all)

mk-cmark:
	(rm -f bin/cmark ; mkdir -p bin ; ln -s ~/opt/bin/cmark bin/cmark)

mk-editor:
	mkdir -p e u
	rm -f e/index.cgi u/index.cgi
	cp $(HOME)/sw/www-minus/py/editor.py e/index.cgi
	cp $(HOME)/sw/www-minus/py/upload.py u/index.cgi


all:	mk-hs mk-convert mk-cmark mk-editor

clean:
	(cd hs; make clean)
	(cd data/jpeg; make clean)

push-rd:
	git push ssh://rd@rohandrape.net/~rd/ut/www-jrd.git master

pull-rd:
	git pull ssh://rd@rohandrape.net/~rd/ut/www-jrd.git master

pull-rd-http:
	git pull http://rohandrape.net/ut/www-jrd.git/ master

pull-jrd:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com; git push ../www-jrd.git master)"
	git pull http://jeremydrape.com/www-jrd.git/ master

remote-update:
	ssh rd@rohandrape.net "(cd ut/www-jrd; git pull ../www-jrd.git)"

remote-update-jrd:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;make pull-rd-http;make all)"

remote-commit-jrd:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;git commit -a -m `date +%F/%T`)"
	make pull-jrd push-rd remote-update
