#
#	Makefile for generating pro2intf files
#
#
TYPFILES= p2i.typ

all: tpro2intf.pro

types: $(TYPFILES)
	alspro -g comptype_cl -p $(TYPFILES)

p2i.pro: p2i.typ
	alspro -g comptype_cl -p p2i.typ

P2IFiles=p2i.pro pro2intf intfout

tpro2intf.pro: $(P2IFiles)
	cat $(P2IFiles) > tpro2intf.pro



