#
# Makefile.spec 
# 	Note : Makefile.tail is excluded for DOSPHAR and DOSERGO
# 	Note : Makefile.tail is excluded for MSWins
#
# 	Software Versions:
#		3.02 HighC Compiler
#		4.0  PharLap Software
#

AIMAGE	=	alsproi.exp

ALIB	=	alspro.a

#
# How to put the system together.
#
#

all	: image

image	: $(AIMAGE)

library	: $(ALIB)

#
# Inference rules for the DOS system.
#

CFLAGS= -c -Hon=Quiet -Hoff=Warn -Hoff=Char_default_unsigned

.c.o:
	hc386 $*.c $(CFLAGS) -o $*.o 

.s.o:
	386asm $*.s -nolist -o $*.o


#
# How to make ALS library
#

LIBOBJS = ${GLIBOBJS} ${SLIBOBJS}

alspro.a: $(LIBOBJS)  $(GPCKGOBJS)
	386lib <@<
$(ALIB)
-c $(LIBOBJS) $(GPCKGOBJS)
<



#
# How to make ALS Prolog image 
#
 
HIGHCLIBDIR=c:\highc\small
 
HIGHCLIB1=$(HIGHCLIBDIR)\hc386.lib
HIGHCLIB2=$(HIGHCLIBDIR)\hcloc.lib
 
alsproi.exp:	$(ALIB) $(GFOROBJS)
	386link <@< 
$(GFOROBJS)
-lib $(ALIB)
-lib $(HIGHCLIB1)
-lib $(HIGHCLIB2)
-pack
-maxreal 0ffffh
-maxdata 0
-nomap 
-exe $(AIMAGE)
<


 
#
# How to make ALS Prolog executable image in PharLap environment
#
 
RUN386B=c:\phar386\bin\run386b.exe
#VMMDRVB=c:\phar386\bin\vmmdrvb.exp
VMMDRVB=
#RUN386SWTCH=-maxreal 0ffffh
RUN386SWTCH=-maxreal 0ffffh -vm c:\phar386\bin\vmmdrv.exp
 
alspro.exe:  alsproi.exp
    bind386 $(RUN386B) $(VMMDRVB) alsproi.exp -exe alspro.exe
    cfig386 alspro.exe $(RUN386SWTCH)




#
# How to link foreign routines with ALS Prolog library
#

FOROBJS=tfor.o

alsfor.exp:	$(ALIB) $(FOROBJS)
	386link <@< 
$(FOROBJS) 
-lib $(ALIB)
-lib $(HIGHCLIB1)
-lib $(HIGHCLIB2)
-pack
-maxreal 0ffffh
-maxdata 0 
-nomap
-exe alsfor.exp
<



#
# How to make an application package
#

PCKGLIB=alspckg.a
PFOROBJS=pimain.o
PCKGS=tblt.o tblttok.o

alspckg.a:	$(LIBOBJS)
	386lib <@<
$(PCKGLIB) 
-c $(LIBOBJS)
<


alspckg.exp:	$(PCKGLIB) $(PFOROBJS) $(PCKGS) 
	386link <@<
$(PFOROBJS) $(PCKGS) 
-lib $(PCKGLIB) 
-lib $(HIGHCLIB1) 
-lib $(HIGHCLIB2) 
-pack
-maxreal 0ffffh
-maxdata 0 
-nomap 
-exe alspckg.exp
<


alspckg.exe:  alspckg.exp
    bind386 $(RUN386B) $(VMMDRVB) alspckg.exp -exe alspckg.exe
    cfig386 alspckg.exe $(RUN386SWTCH)

#
# Dependencies of assembler files used in DOS system
#

int86.o: int86.s

inthand.o:	inthand.s sconfig.h

rts.o: sconfig.h


# 
# Makefile to make 386 System in DOS Environment with CMS libraries
# 

#
# Delete modules malloc and calloc from original HighC library
# (Those modules contains memory management functions such as
# malloc and free)
# 
VMHIGHCLIB1=vmhc386.lib 

$(VMHIGHCLIB1):
	copy $(HIGHCLIB1) $(VMHIGHCLIB1)
	386lib $(VMHIGHCLIB1) -delete malloc calloc


#
# Create CMS Virtual Memory Library
#
CMSVMLIB=libta.lib
VMLIB=vmlib.a

$(VMLIB): vminit.o
	copy $(CMSVMLIB) $(VMLIB)
	386lib $(VMLIB) -add vminit.o


#
# Real mode code part of CMS Virtual Memory library
#
VMRMOBJ=vmrmc.o

#$(VMRMOBJ): vmrmc.s
#	386asm vmrmc.s -386P -80387 -twocase -nolist -o $(VMRMOBJ)


#
# How to make ALS Prolog image 
# in CMS Virtual Memory Environment
#
VMFOROBJS=vmpimain.o
#VMFOROBJS=cmsmain.o

alsprovm.exp: $(VMFOROBJS) $(VMRMOBJ) $(VMLIB) $(VMHIGHCLIB1) 
	386link <@< 
$(VMRMOBJ)
$(VMFOROBJS) 
-lib $(ALIB) 
-lib $(VMLIB) 
-lib $(VMHIGHCLIB1) 
-lib $(HIGHCLIB2) 
-pack
-unpriv
-maxreal 0ffffh
-nomap
-realbreak pmbase
-exe alsprovm.exp
<

 
#
# How to make ALS Prolog executable image 
# in CMS Virtual Memory Environment
#
PHRUN386B=c:\phar386\bin\run386b.exe
 
alsprovm.exe:  alsprovm.exp
    bind386 $(RUN386B) alsprovm.exp -exe alsprovm.exe



#
# How to make an application package 
# in CMS Virtual Memory Environment
#
alspvm.exp: $(VMFOROBJS) $(VMRMOBJ) $(VMLIB) $(VMHIGHCLIB1) 
	386link <@<
$(VMRMOBJ)
$(VMFOROBJS) 
$(PCKGS) 
-lib $(PCKGLIB) 
-lib $(VMLIB) 
-lib $(VMHIGHCLIB1) 
-lib $(HIGHCLIB2) 
-pack
-unpriv
-maxreal 0ffffh
-nomap
-realbreak pmbase
-exe alspvm.exp
<


#
# Dependencies for files used 
# in CMS Virtual Memory Environment
#
vminit.o: vminit.c config.h pckg.h

cmsmain.o: cmsmain.c



 
#
# How to make set.exp
# (This program sets expiration date and/or number of usage for alsdemo
#  on Protech memory key)
#
 
setexp.exp: setexp.o cle.obj
    386link <@<
setexp.o cle.obj 
-lib $(HIGHCLIB1) 
-lib(HIGHCLIB2) 
-privileged 
-exe setexp.exp
<
 
setexp.exe:  setexp.exp
    bind386 $(RUN386B) setexp.exp -exe setexp.exe
 
NUMOFDAYS=30
NUMOFUSAGE=10000
 
setexp: setexp.exe
    setexp $(NUMOFDAYS) $(NUMOFUSAGE)
 
setexp.o: setexp.c
 
 
 
#
# How to make ALS Prolog demo image
# in PharLap environment and in CMS Virtual Memory Environment
#
 
#
# Flags to determine the kind of checks done by alsdemo
#
DFLAGS= -DEXP_DATE=1 -DCHECK_LAST_USAGE=1 -DCHECK_NUMOF_USAGE=1
 
dpaction:
    hc386 <@<
paction.c $(CFLAGS) $(DFLAGS) -o paction.o
<
 
dpaction2:
    hc386 <@<
paction2.c $(CFLAGS) $(DFLAGS) -o paction2.o
<
 
alsdemo.a: $(LIBOBJS)  $(GPCKGOBJS) dpaction dpaction2
    386lib <@<
$(DEMOLIB)
-c $(LIBOBJS) paction2.o $(GPCKGOBJS)
<
    del paction.o
 
 
alsdemo.exp:    $(DEMOLIB) $(GFOROBJS)
    386link <@<
$(GFOROBJS)
cle.obj
-lib $(DEMOLIB)
-lib $(HIGHCLIB1)
-lib $(HIGHCLIB2)
-pack
-privileged
-maxreal 0ffffh
-maxdata 0 -nomap
-exe alsdemo.exp
<
 
 
#
# Note:
# 	alsdemov.exp currently doesn't work since CMS virtual memory manager
#	wants to run at a privilege level grater than zero but the Protechkey
# 	software wants to run at the privilege level zero.
#
# 	Ilyas 8/10/1992
#
alsdemov.exp: $(VMFOROBJS) $(VMRMOBJ) $(VMLIB) $(VMHIGHCLIB1) $(DEMOLIB)
    386link <@<
$(VMRMOBJ)
$(VMFOROBJS)
cle.obj
-lib $(DEMOLIB)
-lib $(VMLIB)
-lib $(VMHIGHCLIB1)
-lib $(HIGHCLIB2)
-pack
-privileged
-maxreal 0ffffh
-realbreak pmbase
-exe alsdemov.exp
<


