#
#   Uses any alspro version: 1.74c or later
#

A2M = ../generate/abs2make

ALSPRO = alspro

all: all-generic all-port all-maketop

#
#	All versions of ...
#
all-generic: generic.unix generic.dj1 generic.dj2 generic.w31 \
			generic.mac generic.os2

all-port: port.unix port.dj1 port.dj2 port.w31 port.mac port.os2

all-maketop: maketop.unix maketop.dj1 maketop.dj2 maketop.w31 \
			maketop.mac maketop.os2

#
#	All makefiles for version ...
#
w31: generic.w31 port.w31 maketop.w31

unix: generic.unix port.unix maketop.unix

djgpp1: generic.dj1 port.dj1 maketop.dj1

djgpp2: generic.dj2 port.dj2 maketop.dj2

os2: generic.os2 port.os2 maketop.os2

mac: generic.mac port.mac maketop.mac



#---------------------------------------------------------
#	generic.mkf
#
generic.unix: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.unix -ctx unix



generic.dj1: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl \
		-p -src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.dj -ctx djgpp1

generic.dj2: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  \
		-b -g abs2make_cl \
		-p -src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.dj2 -ctx djgpp2


generic.os2: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl \
		-p -src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.os2 -ctx os2

generic.w31: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.w31 -ctx w31

generic.mac: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.mac -ctx mac
	unix2mac ../generic/generate/generic.mac

#---------------------------------------------------------
#	bld-port/makefile.in
#
port.unix: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.unix -ctx unix
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.unix -ctx unix


port.dj1: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.dj1 -ctx djgpp1
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.dj1 -ctx djgpp1

port.dj2: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.dj2 -ctx djgpp2
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.dj2 -ctx djgpp2


port.os2: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.os2 -ctx os2
	$(ALSPRO) $(A2M)  -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.os2 -ctx os2

port.w31: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.w31 -ctx w31
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.w31 -ctx w31


port.mac: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.mac -ctx mac
	unix2mac ../port/port.mac
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.mac -ctx mac
	unix2mac ../bld-port/makefile.mac

#---------------------------------------------------------
#	top level makefile.in
#

maketop.unix: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.unix -ctx unix

maketop.dj1: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.dj1 -ctx djgpp1

maketop.dj2: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.dj2 -ctx djgpp2

maketop.os2: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.os2 -ctx os2

maketop.w31: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.w31 -ctx w31 

maketop.mac: maketop.abs
	$(ALSPRO) $(A2M) -b -g abs2make_cl -p \
		-p -src maketop.abs -tgt maketop.mac -ctx mac
	unix2mac maketop.mac

#---------------------------------------------------------

install.unix:
	cp maketop.unix ../makefile.in
	cp ../generic/generate/generic.unix ../generic/generic.mkf 
	cp ../port/port.unix ../port/port.mkf
	cp ../bld-port/makefile.unix ../bld-port/makefile.in
