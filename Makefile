all:
	(cd hs; make all)
	(cd data/jpeg; make all)
	(rm -f bin/cmark ; mkdir -p bin ; ln -s ~/opt/bin/cmark bin/cmark)
	(make setup-editor)

clean:
	(cd hs; make clean)
	(cd data/jpeg; make clean)

setup-editor:
	mkdir -p e
	rm -f e/index.cgi
	cp $(HOME)/sw/www-minus/py/editor.py e/index.cgi

push-sp:
	git push ssh://rd@slavepianos.org/~rd/ut/www-jrd.git master

pull-sp:
	git pull ssh://rd@slavepianos.org/~rd/ut/www-jrd.git master

pull-sp-http:
	git pull http://rd.slavepianos.org/ut/www-jrd.git/ master

pull-jrd:
	git pull http://jeremydrape.com/www-jrd.git/ master

remote-update:
	ssh rd@slavepianos.org "(cd ut/www-jrd; git pull ../www-jrd.git)"

remote-update-jrd:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;make pull-sp-http;make all)"

remote-commit-jrd:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;git commit -a -m `date +%F/%T`; git push ../www-jrd.git master)"
	make pull-jrd push-sp remote-update
