/^exeformat/c\
exeformat = coff

/^alsdir:[ 	]/ i\
alsdir:\
	mkdir alsdir \
	mkdir alsdir\\builtins \
	mkdir alsdir\\library \
	cp @srcdir@/builtins/*.pro alsdir/builtins \
	cp @srcdir@/library/*.pro alsdir/library \


s/^alsdir:[ 	]/not_alsdir: /
s/^	.\//	go32 /

s/pi_cfg\.h\.proto/pi_cfg/

/-o prebldtok/ a\
	go32 prebldtok > prebldtok.out
s/`.\/prebldtok`/@prebldtok.out/
s/`date`/YY\/MM\/DD/g
s/echo >>/echo. >>/

/cp alsmics/ a\
	coff2exe alsdir/als-mics alsmics
