#
# Makefile for the interfaces to the accsys library
#

ALSSRC=/mailbox3/alsp_src

#
#	Cinterf:
#
CINTDIR	= /mailbox3/alsp_src/cinterf
C2PDIR	= $(CINTDIR)/c2pro
P2IDIR	= $(CINTDIR)/pro2intf

PROLOG = alspro

#
#	General accsys stuff
#
FPREFIX		= acc
srcdir		= /mailbox3/alsp_src
itfsrcdir	= ../src
#VPATH		= $(itfsrcdir)
SPECFILE	= $(itfsrcdir)/accsys.spc
HFILES		= $(itfsrcdir)/accsys.h
SYSCFG		= ../syscfg

INTF_CFLAGS	= 
INTF_C2PFLAGS	= 
INTF_P2IFLAGS	= 
INTF_INCLUDES	= 
INTF_DEFINES	= 

#
#	Prolog flags
#

PROFLAGS = -b

#
#	Create .src from .spc
#

TRFLAGS = -debug $(XTRFLAGS)
C2PFiles = c2p.pro c2pro cexp cfiles cmacro cout cparse ctoken
C2PFS = $(addprefix $(CINTDIR)/c2pro/,$(C2PFiles))

FILTXPR=$(INTF)
FILTFILE=../xref/$(INTF).flt

c2prolcl.pro: $(C2PFS)
	cat $(C2PFS) > c2prolcl.pro

C2PDB		= debug
C2PFLAGS 	= -$(C2PDB) $(INTF_TRFLAGS)


$(INTF).src : $(SPECFILE) $(HFILES) c2prolcl.pro
	$(PROLOG) c2prolcl.pro \
		$(PROFLAGS) -g c2pro \
		-p -srcpath $(itfsrcdir) $(INTF) $(C2PFLAGS) \
		$(INTF_INCLUDES) -master accsys $(INTF_DEFINES) \
		-filter $(FILTXPR) -filterFile $(FILTFILE)


#
#	Create .c files from .src files
#

P2IFiles= p2i.pro pro2intf intfout
P2IFS = $(addprefix $(CINTDIR)/pro2intf/,$(P2IFiles))

p2intlcl.pro: $(P2IFS)
	cat $(P2IFS) > p2intlcl.pro

TRANSFCN="mytrans(I,O) :- name(O,[0'a ,0'_ |I])"

P2IFLAGS = -Ddebug -T $(TRANSFCN) $(INTF_P2IFLAGS) -hinc '[accsys]'

cfiles: $(INTF)0.c

$(INTF)0.c $(INTF)1.c $(INTF)2.c $(INTF)3.c : $(INTF).src p2intlcl.pro
	$(PROLOG) $(PROFLAGS) p2intlcl.pro \
	-g pro2intf \
	-p $(INTF) -fpre $(FPREFIX) $(P2IFLAGS)


#
#	Making individual versions:
#

d3i d3x d3z d3t d3n:
	echo trying tgt=acsy$@
	make cfiles INTF=acsy$@ INTFVAR=$@ PROTYPE=$(PROTYPE)
	
d3iz d3in d3ix d3inz d3ixz d3it d3int d3ixt d3intz d3ixtz:
	echo trying tgt=acsy$@
	make cfiles INTF=acsy$@ INTFVAR=$@ PROTYPE=$(PROTYPE)

