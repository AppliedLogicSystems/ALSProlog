#
#	Makefile for creating *.pro from *.typ:
#
ALSPRO=alspro

TYPFILES	=clogintp.typ cscmntyp.typ job_rectp.typ sktclntp.typ sktsrvtp.typ

PROFILES	=admnclnt.pro cmnclint.pro cscmntyp.pro csp_int.pro job_rectp.pro \
			 job_utls.pro jobserve.pro localintf.pro sktclint.pro sktclntp.pro \
			 sktserve.pro sktsrvtp.pro sktsvlib.pro sktsvutl.pro usradmn.pro \
			 wkr_mgmt.pro

DOCFILES	=csp_int.frame ProCS.frame ProCS.html

types: $(TYPFILES)
	$(ALSPRO) -g comptype_cl -p $(TYPFILES)

c_s.tar: $(TYPEFILES) $(PROFILES) $(DOCFILES)
	tar -cvf ./c_s.tar *.pro *.typ *.frame *.html

c_s.tar.gz: c_s.tar
	gzip c_s.tar

packup: c_s.tar.gz

