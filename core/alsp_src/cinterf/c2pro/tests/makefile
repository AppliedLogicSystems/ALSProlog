


PROLOG=alspro

WIN=x
GOS=unix
CINTDIR=../..
XINCLUDES = -I /usr/include

WSRCD=../examples
SYSPFX=/dvx


C2PFILT=all
TRFLAGS = -debug $(XTRFLAGS)
C2PFiles = c2p.pro c2pro cexp cfiles cmacro cout cparse ctoken
C2PFS = $(addprefix $(CINTDIR)/c2pro/,$(C2PFiles))

c2prolcl.pro: $(C2PFS)
	cat $(C2PFS) > c2prolcl.pro

t $(WIN).src : ../examples/$(WIN).spc ../examples/$(WIN).h c2prolcl.pro
	$(PROLOG) $(PROFLAGS) c2prolcl.pro \
	-p -os $(GOS) -filter $(C2PFILT) -srcpath $(WSRCD) $(WIN) \
	-sys_pfx /dvx  -info x_info\
	$(TRFLAGS) $(XINCLUDES) $(XDEFINES) $(X_CFLAGS)

