#
#	Makefile for generating ctrans files
#
#

# Just just the local working ALS Prolog:

ALSPRO=alspro

TYPFILES= c2p.typ

all: c2prolcl.pro

types: $(TYPFILES)
	$(ALSPRO) -g comptype_cl -p $(TYPFILES)

c2p.pro: c2p.typ
	$(ALSPRO) -g comptype_cl -p c2p.typ

C2PFiles=c2p.pro c2pro cexp cfiles cmacro cout cparse ctoken

c2prolcl.pro c2pro.pro: $(C2PFiles)
	cat $(C2PFiles) > c2prolcl.pro


