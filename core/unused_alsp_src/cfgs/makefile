#
#   Uses any alspro version: 1.74c or later
#

A2M = ../generate/abs2make

#ALSPRO = /mailbox3/bldtests/sp1.74B/bld-port/alspro_b
#ALSPRO = /mailbox3/bldtests/hp1.74B/bld-port/alspro_b
ALSPRO = alspro_b

all: all-generic all-port all-maketop

#
#	All versions of ...
#
all-generic: generic.unix generic.djgpp generic.msw31 generic.mac generic.os2

all-port: port.unix port.djgpp port.msw31 port.mac port.os2

all-maketop: maketop.unix maketop.djgpp maketop.msw31 maketop.mac maketop.os2

#
#	All makefiles for version ...
#
msw31: generic.msw31 port.msw31 maketop.msw31

unix: generic.unix port.unix maketop.unix

djgpp: generic.djgpp port.djgpp maketop.djgpp

os2: generic.os2 port.os2 maketop.os2

mac: generic.mac port.mac maketop.mac



#---------------------------------------------------------
#	generic.mkf
#
generic.unix: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.unix -ctx unix



generic.dj generic.djgpp: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl \
		-p -src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.dj -ctx djgpp


generic.os2: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl \
		-p -src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.os2 -ctx os2

generic.msw31: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.w31 -ctx msw31

generic.mac: ../generic/generate/generic.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
		-src ../generic/generate/generic.abs \
		-tgt ../generic/generate/generic.mac -ctx mac

#---------------------------------------------------------
#	bld-port/makefile.in
#
port.unix: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.unix -ctx unix
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.unix -ctx unix


port.djgpp: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.dj -ctx djgpp
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.dj -ctx djgpp


port.os2: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.os2 -ctx os2
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.os2 -ctx os2

port.msw31: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.w31 -ctx msw31
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.w31 -ctx msw31


port.mac: ../bld-port/makefile.abs ../port/port.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../port/port.abs \
                -tgt ../port/port.mac -ctx mac
	$(ALSPRO) $(A2M)  -g abs2make_cl -p \
                -src ../bld-port/makefile.abs \
                -tgt ../bld-port/makefile.mac -ctx mac

#---------------------------------------------------------
#	top level makefile.in
#

maketop.unix: maketop.abs
	$(ALSPRO) $(A2M) -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.unix -ctx unix

maketop.djgpp: maketop.abs
	$(ALSPRO) $(A2M) -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.dj -ctx djgpp

maketop.os2: maketop.abs
	$(ALSPRO) $(A2M) -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.os2 -ctx os2

maketop.msw31: maketop.abs
	$(ALSPRO) $(A2M)  -g abs2make_cl \
		-p -src maketop.abs -tgt maketop.w31 -ctx msw31 

maketop.mac: maketop.abs
	$(ALSPRO) $(A2M) -g abs2make_cl -p \
		-p -src maketop.abs -tgt maketop.mac -ctx mac

#---------------------------------------------------------

install.unix:
	cp maketop.unix ../makefile.in
	cp ../generic/generate/generic.unix ../generic/generic.mkf 
	cp ../port/port.unix ../port/port.mkf
	cp ../bld-port/makefile.unix ../bld-port/makefile.in
