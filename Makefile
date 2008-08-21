send_f: 
	r.ftp jrd-f.css f/index.html f/*/index.html

all:
	make send_indexes

send_indexes:
	r.ftp jrd.css \
		index.html \
		*/index.html \
		*/*/index.html \
		*/*/*/index.html
