all:
	make send_indexes

send_indexes:
	r.ftp jrd.css \
		index.html \
		*/index.html \
		*/*/index.html \
		*/*/*/index.html
