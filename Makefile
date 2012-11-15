all:
	(cd hs; make all)
	(cd data/jpeg; make all)

clean:
	(cd hs; make clean)
	(cd data/jpeg; make clean)
