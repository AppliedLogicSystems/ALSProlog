
#
# Makefile.spec 
# 	Note : Makefile.tail is excluded for DOSPHAR and DOSERGO
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
	hc386 $*.c $(CFLAGS) -Hobject=$*.o 

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

alspro3.a: $(LIBOBJS)  $(GPCKGOBJS)
	f:\pharlap\ver30\bin\386lib <@<
$(ALIB)
-c $(LIBOBJS) $(GPCKGOBJS)
<


#
# How to make ALS Prolog image 
#

# We have to use version 1.71 HighC library 
#(Version 1.73 doesn't work under Ergo environment)
#
#HIGHCLIB=c:\highc\small\hce.lib
HIGHCLIB=f:\highc\small\hce.lib

alsproi.exp:	$(ALIB) $(GFOROBJS)
	386link <@< 
$(GFOROBJS) 
-lib $(ALIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
-exe $(AIMAGE)
<

alsproi3.exp:	alspro3.a $(GFOROBJS)
	f:\pharlap\ver30\bin\386link <@< 
$(GFOROBJS) 
-lib $(ALIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
-exe $(AIMAGE)
<


#
# How to make ALS Prolog executable image in Ergo environment
#

OS386=f:\os386\v2105\nonvm\os386\os386.exe
TINYUP=f:\os386\v21vm\tinyup.exe

aialspro.exe:	alsproi.exp
	bind -o aialspro.exe -i alsproi.exp -k $(OS386) -l $(TINYUP) 


VMOS386=f:\os386\v2105\vm\os386\os386vm.exe
VMTINYUP=f:\os386\v21vm\tinyup.exe

alspro.exe:	alsproi.exp
	bind -o alspro.exe -i alsproi.exp -k $(VMOS386) -l $(VMTINYUP) 


#
# How to link foreign routines with ALS Prolog library
#

FOROBJS=tfor.o

alsfor.exp:	$(ALIB) $(FOROBJS)
	386link <@< 
$(FOROBJS) 
-lib $(ALIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
-exe alsfor.exp
<

alsfor3.exp:	alspro3.a $(FOROBJS)
	f:\pharlap\ver30\bin\386link <@< 
$(FOROBJS) 
-lib $(ALIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
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

alspckg3.a:	$(LIBOBJS)
	f:\pharlap\ver30\bin\386lib <@<
$(PCKGLIB) 
-c $(LIBOBJS)
<


alspckg.exp:	$(PCKGLIB) $(PFOROBJS) $(PCKGS) 
	386link <@<
$(PFOROBJS) $(PCKGS) 
-lib $(PCKGLIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
-exe alspckg.exp
<

alspckg3.exp:	alspckg3.a $(PFOROBJS) $(PCKGS) 
	f:\pharlap\ver30\bin\386link <@<
$(PFOROBJS) $(PCKGS) 
-lib $(PCKGLIB) -lib $(HIGHCLIB) 
-maxdata 0 -nomap 
-exe alspckg.exp
<

alspckg.exe:	alspckg.exp
	bind -o alspckg.exe -i alspckg.exp -k $(VMOS386) -l $(VMTINYUP) 

#
# Dependencies of assembler files used in DOS system
#

int86.o: int86.s

inthand.o:	inthand.s sconfig.h

rts.o: sconfig.h

