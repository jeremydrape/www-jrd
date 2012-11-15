all:
	(cd hs; make all)
	(cd data/jpeg; make all)

clean:
	(cd hs; make clean)
	(cd data/jpeg; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:ut/www-jrd

remote-update:
	ssh jeremydrape@jeremydrape.com "(cd jeremydrape.com;darcs pull -a;make all)"
