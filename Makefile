all:
	(cd hs; make all)
	(cd data/jpeg; make all)

clean:
	(cd hs; make clean)
	(cd data/jpeg; make clean)

push-sp:
	git push sp

pull-sp:
	git pull sp master

pull-sp-http:
	git pull http://rd.slavepianos.org/ut/www-jrd.git/ master

pull-jrd:
	git pull http://jeremydrape.com/www-jrd.git/ master

add-remote-sp:
	git remote add sp ssh://rd@slavepianos.org/~rd/ut/www-jrd.git

remote-sp:
	ssh rd@slavepianos.org "(cd ut/www-jrd; make pull-jrd)"

remote-update:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;make pull-sp-http;make all)"

remote-commit:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;git commit -a -m `date +%F/%T`; git push ../www-jrd.git master)"
