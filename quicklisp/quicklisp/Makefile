all:
	git archive --format=tar --prefix=quicklisp/ HEAD > quicklisp.tar
	gzip -c quicklisp.tar > quicklisp-`cat version.txt`.tgz

