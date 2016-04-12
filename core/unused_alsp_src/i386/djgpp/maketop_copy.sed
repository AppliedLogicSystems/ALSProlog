s/@srcdir@/pathsrc/g
s/@CC@/gcc -I..\/i386\/djgpp/
s/@MACH_OS@/djgpp/
s/@OS@/djgpp/
s/@LIBS@/..\/i386\/djgpp\/libregex.a -lm/
s/@TARGET@/djgpp/
s/@X_CFLAGS@/-DX_DISPLAY_MISSING/
s/@X_LIBS@//
s/@X_EXTRA_LIBS@//
s/@LIBOBJS@/fileblocks.o/

s/cd \(.*\); $(MAKE)/$(MAKE) -C \1/g
s/^	(\(.*\))$/	\1/g
s/^	-(\(.*\))$/	\1/g
s/include $(srcdir)\/generic\/mh-generic/include ..\/generic.dos/

/VPATH.*=/ s/:/;/g

s/^	.\//	go32 /
s/^	alspro/	go32 alspro/
s/.h.proto//g
