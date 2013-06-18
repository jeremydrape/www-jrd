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

add-remote-sp:
	git remote remove sp
	git remote add sp ssh://rd@slavepianos.org/~rd/ut/www-jrd.git

remote-update:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;make pull-sp;make all)"
