

all	Ä	acsyd3iinterf.ppc.lib acsyd3izinterf.ppc.lib acsyd3iinterf.68k.lib acsyd3izinterf.68k.lib

GUSIIncludes =   {MPW}Interfaces:GUSIIncludes:

ALSIncludes	= mailbox3:builds:all_macos7.5:

CC68k =   MWC68K

LINK68k =   MWLINK68K

CCPPC =   MWCPPC

LINKPPC =   MWLINKPPC

# we should use -requireprotos, but the interface generator isn't producing prototypes.
CFLAGS =   -opt all -mpw_chars -w off -nosyspath -sym full

CFLAGS68K =   -model farData -model codesmart

CFLAGSPPC =   

# all the paths with  bld-port and {srcdir} are unecessary - the interface generator is incorrectly
# include defs.h, so I had to do it.
srcdir =   mailbox3:alsp_src:
ARCH =   port
SOS =   macos
CPPFLAGS =  -i :::src: -i :::gen: -i {ALSIncludes} -i {ALSIncludes}bld-port: -i {srcdir}generic: -i {srcdir}generic:generate:tok.h: -i {srcdir}{ARCH}:{SOS} -i {srcdir}{ARCH}  -i {GUSIIncludes}

:	Ä	:::gen:

.68k.o Ä .c
	{CC68k} -o {TargDir}{Default}.68k.o {CPPFLAGS} {CFLAGS} {CFLAGS68K} {DepDir}{Default}.c


.ppc.o Ä .c
	{CCPPC} -o {TargDir}{Default}.ppc.o {CPPFLAGS} {CFLAGS} {CFLAGSPPC} {DepDir}{Default}.c


INTFOBJPPC =	acsyd3i0.ppc.o ¶
				acsyd3i1.ppc.o ¶
				acsyd3i2.ppc.o ¶
				acsyd3i3.ppc.o

ZINTFOBJPPC =	acsyd3iz0.ppc.o ¶
				acsyd3iz1.ppc.o ¶
				acsyd3iz2.ppc.o ¶
				acsyd3iz3.ppc.o

INTFOBJ68K =	acsyd3i0.68k.o ¶
				acsyd3i1.68k.o ¶
				acsyd3i2.68k.o ¶
				acsyd3i3.68k.o

ZINTFOBJ68K =	acsyd3iz0.68k.o ¶
				acsyd3iz1.68k.o ¶
				acsyd3iz2.68k.o ¶
				acsyd3iz3.68k.o


acsyd3iinterf.ppc.lib	Ä	{INTFOBJPPC}
	{LINKPPC} -library -o acsyd3iinterf.ppc.lib {INTFOBJPPC}
	
acsyd3izinterf.ppc.lib	Ä	{ZINTFOBJPPC}
	{LINKPPC} -library -o acsyd3izinterf.ppc.lib {ZINTFOBJPPC}
	
acsyd3iinterf.68k.lib	Ä	{INTFOBJ68K}
	{LINK68K} -library -o acsyd3iinterf.68k.lib {INTFOBJ68K}
	
acsyd3izinterf.68k.lib	Ä	{ZINTFOBJ68K}
	{LINK68K} -library -o acsyd3izinterf.68k.lib {ZINTFOBJ68K}
	
	
clean	Ä
	Delete -i {INTFOBJPPC} {ZINTFOBJPPC} {INTFOBJ68K} {ZINTFOBJ68K}

super_clean	Ä	clean
	Delete -i acsyd3iinterf.ppc.lib acsyd3izinterf.ppc.lib acsyd3iinterf.68k.lib acsyd3izinterf.68k.lib


