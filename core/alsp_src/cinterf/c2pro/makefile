#
#	Makefile for generating ctrans files
#
#
PRODIR=../..
ARCH=SPARC
MACH=Sun

ARCHMACH=$(ARCH)/$(MACH)

#ALSPRO=$(PRODIR)/$(ARCHMACH)/obj/alspro
ALSPRO=alspro

TYPFILES= c2p.typ

all: trans_c2pro_unix.pro trans_c2pro_dos.pro trans_c2pro_mac.pro trans_c2pro_vms.pro

types: $(TYPFILES)
	$(ALSPRO) -g comptype_cl -p $(TYPFILES)

c2p.pro: c2p.typ
	$(ALSPRO) -g comptype_cl -p c2p.typ

TC2PFiles=c2p.pro c2pro cexp cfiles cmacro cout cparse ctoken

OS_TC2PFiles=cunix cdos cvms cmac

trans_c2pro_unix.pro: $(TC2PFiles) cunix
	cat $(TC2PFiles) cunix > trans_c2pro_unix.pro

trans_c2pro_dos.pro: $(TC2PFiles) cdos
	cat $(TC2PFiles) cdos  > trans_c2pro_dos.pro

trans_c2pro_mac.pro: $(TC2PFiles) cmac
	cat $(TC2PFiles) cmac  > trans_c2pro_mac.pro

trans_c2pro_vms.pro: $(TC2PFiles) cvms
	cat $(TC2PFiles) cvms  > trans_c2pro_vms.pro

