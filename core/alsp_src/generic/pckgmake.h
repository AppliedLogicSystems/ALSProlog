/* 
 * pckgmake.h  -- Include file for pckgmake.c
 *
 * Copyright (c) 1991-1993 by Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 4/23/1991
 */


#ifdef PACKAGE 


#define CHANGE_FLAGS(f,nf) 	(((f) & ~NMSK_USAGE) | nf)

#define isPrologProc(ent)													\
		(((ent->flags & NMSK_USAGE) == NFLG_SINGLE)  	||     				\
		 ((ent->flags & NMSK_USAGE) == NFLG_MULTIPLE) 	||					\
		 ((ent->flags & NMSK_USAGE) == NFLG_SWITCH) ) 




#define COFF_PCKG_OVERFLOW(ent) \
  if (coff_pckg_overflow(ent) < 0) return(-1);

#define COFF_PCKG_CALL_ENTRY(ent) \
  if (coff_pckg_call_entry(ent) < 0) return(-1);

#define COFF_PCKG_EXEC_ENTRY(ent) \
  if (coff_pckg_exec_entry(ent) < 0) return(-1);

#define COFF_PCKG_CODE_BUILTIN(ent) \
  if (coff_pckg_code_builtin(ent) < 0 ) return(-1);

#define COFF_PCKG_CODE_MODCLOSURE(ent) \
  if (coff_pckg_code_modclosure(ent) < 0) return(-1);

#define COFF_PCKG_CODE_JMP(ent) \
  if (coff_pckg_code_jmp(ent) < 0) return(-1);

#define COFF_PCKG_CODE_SINGLE(ent,procname) \
  if (coff_pckg_code_single(ent,procname) < 0) return(-1);

#define COFF_PCKG_CODE_MULTIPLE(ent,procname) \
  if (coff_pckg_code_multiple(ent,procname) < 0) return(-1);

#define	COFF_PCKG_LIBBREAK(ent) \
  if (coff_pckg_libbreak(ent) < 0 ) return(-1);

#define COFF_PCKG_RESOLVE_REF(ent) \
  if (coff_pckg_resolve_ref(ent) < 0) return(-1);

#ifdef arch_i386

#define COFF_PCKG_CODE_CALL(ent)    fatal_error(FE_IN_PCKG1,0);

#define COFF_PCKG_CODE_EQUAL(ent)   fatal_error(FE_IN_PCKG2,0);

#define COFF_PCKG_CODE_TRUE(ent)    fatal_error(FE_IN_PCKG3,0);


#endif 	/* arch_i386 */


#ifdef arch_sparc

#define COFF_PCKG_CODE_CALL(ent) \
  if (coff_pckg_code_call(ent) < 0) return(-1);

#define COFF_PCKG_CODE_EQUAL(ent) \
  if (coff_pckg_code_equal(ent) < 0) return(-1);

#define COFF_PCKG_CODE_TRUE(ent) \
  if (coff_pckg_code_true(ent) < 0) return(-1);

#endif /* arch_sparc */


#ifdef arch_m68k 

#define PROC_OVERFLOW_ENTRY(cp,ent)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+2) 					\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_overflow),P_R_VIR32) \
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+6,((char *)(cp))+8) 				\
		}
		
#define PROC_CALL_ENTRY(cp)													\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+4) 					\
		}

#define PROC_EXEC_ENTRY(cp)													\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+6) 					\
		}

#define PROC_CODE_BUILTIN_BUILTIN(cp,ent) 									\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+4) 					\
			COFF_RELOC_SYMBOL(												\
				builtin_name(*(long *)((char *)(cp)+4)),P_R_VIR32)			\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+8,((char *)(cp))+10) 				\
			COFF_RELOC_SYMBOL_IDX(											\
				COFF_SYMIDX(symidx_wm_execute_builtin),P_R_VIR32)			\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+14,((char *)(cp))+58) 				\
		}

#define PROC_CODE_BUILTIN_MODCLOSURE(cp,ent)								\
		{																	\
			Code *codeentry;												\
			ntbl_entry *toent;												\
			char buf[64];													\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+28) 					\
			codeentry = (Code *)(*(long *)((char *)(cp)+28));				\
			toent = (ntbl_entry *)(codeentry - NTBL_ENTRYSIZE);				\
			ntbl_entry_name(toent,"code",buf);								\
			COFF_RELOC_SYMBOL(buf,P_R_VIR32)								\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+32,((char *)(cp))+58) 				\
		}

#define PROC_CODE_BUILTIN_JMP(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+4) 					\
			COFF_RELOC_SYMBOL(												\
				builtin_name(*(long *)((char *)(cp)+4)),P_R_VIR32)			\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+8,((char *)(cp))+58) 				\
		}

#define PROC_CODE_BUILTIN_CALL(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+14) 					\
			COFF_RELOC_SYMBOL(												\
				builtin_name(*(long *)((char *)(cp)+14)),P_R_VIR32)			\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+18,((char *)(cp))+58) 				\
		}

#define PROC_CODE_BUILTIN_EQUAL(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+14) 					\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_unify),P_R_VIR32)	\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+18,((char *)(cp))+58) 				\
		}

#define PROC_CODE_BUILTIN_TRUE(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+58) 					\
		}


#define PROC_CODE_SINGLE(cp,ent,procname)									\
		{																	\
			char buf[64];													\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+4) 					\
    		sprintf(buf,"%s_%d_%s", 										\
				procname,clauseId(ent->first_clause),"dstart");				\
			COFF_RELOC_SYMBOL(buf,P_R_VIR32)								\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+8,((char *)(cp))+58) 				\
		}

#define PROC_CODE_MULTIPLE(cp,ent,procname)									\
		{																	\
			char buf[64];													\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+8) 					\
    		sprintf(buf,"%s_%d_%s",procname,								\
				clauseId(nextClauseAddr(ent->first_clause)),"choice");		\
			COFF_RELOC_SYMBOL(buf,P_R_VIR32)								\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+12,((char *)(cp))+20) 				\
    		sprintf(buf,"%s_%d_%s",procname,								\
				clauseId(ent->first_clause),"code");						\
			COFF_RELOC_SYMBOL(buf,P_R_VIR32)								\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+24,((char *)(cp))+58) 				\
		}

#define PROC_CODE_UNDEFINED(cp,ent)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+8) 					\
			COFF_RELOC_SYMBOL_IDX(											\
				COFF_SYMIDX(symidx_wm_resolve_ref),P_R_VIR32)				\
			COFF_LONG_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+12,((char *)(cp))+58) 				\
		}



#define CLAUSE_RETRY_ME_INST(procname,ca,ent)								\
		{																	\
			COFF_RAWDATA((char *)(choiceEntry(ca)),							\
						 (((char *)(choiceEntry(ca)))+20))					\
        	sprintf(buf,"%s_%d_%s",											\
					procname,clauseId(nextClauseAddr(ca)),"choice"); 		\
			COFF_RELOC_SYMBOL(buf,P_R_VIR32)								\
			COFF_LONG_RAWDATA(0)											\
		}

#define CLAUSE_TRUST_ME_INST(ca,ent)										\
		{																	\
			COFF_RAWDATA((char *)(choiceEntry(ca)),							\
						 (((char *)(choiceEntry(ca)))+24))					\
		}

#endif 	/* arch_m68k */



#ifdef arch_m88k 

#define MASK26 	0x03FFFFFF


#define PROC_OVERFLOW_ENTRY(cp,ent)											\
		{																	\
			long ov_symidx;													\
			ov_symidx =														\
			  ovtab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs]; 	\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(ov_symidx),P_R_REL26)			\
			COFF_LONG_RAWDATA(((*(unsigned long *)(cp)) & ~MASK26))			\
			COFF_LONG_RAWDATA(*(((unsigned long *)(cp))+1))					\
		}
		
#define PROC_CALL_ENTRY(cp)													\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+8) 					\
		}

#define PROC_EXEC_ENTRY(cp)													\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+16) 					\
		}

#define PROC_CODE_BUILTIN_BUILTIN(cp,ent)									\
		{																	\
			unsigned long bltaddr;											\
			char *bltname;													\
			long eb_symidx;													\
			eb_symidx =														\
			  ebtab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs];  	\
			bltaddr = ( (unsigned long) *(((unsigned short *)(cp))+1) ) |	\
				( ((unsigned long) *(((unsigned short *)(cp))+5)) << 16 );	\
			bltname = (char *) builtin_name(bltaddr); 						\
			COFF_SHORT_RAWDATA(*(unsigned short *)(cp))						\
			COFF_RELOC_SYMBOL(bltname,P_R_RELL16)							\
			COFF_SHORT_RAWDATA(0)											\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(eb_symidx),P_R_REL26)			\
			COFF_LONG_RAWDATA(((*(((unsigned long *)(cp))+1)) & ~MASK26))	\
			COFF_SHORT_RAWDATA(*(unsigned short *)((char *)(cp)+8))			\
			COFF_RELOC_SYMBOL(bltname,P_R_RELH16)							\
			COFF_SHORT_RAWDATA(0)											\
			COFF_RAWDATA(((char *)(cp))+12,((char *)(cp))+76) 				\
		}

#ifdef silicon_filter
#define SLICON_FILTER_CODE(p) 												\
		{ 																	\
			COFF_LONG_RAWDATA(*p++)											\
		}
#else 	/* silicon_filter */
#define SLICON_FILTER_CODE(p) 	{ }
#endif 	/* silicon_filter */

#define PROC_CODE_BUILTIN_MODCLOSURE(cp,ent)								\
		{																	\
			unsigned long *p;												\
			char buf[64];													\
			Code *paddr;													\
			long offs;														\
			ntbl_entry *toent;												\
			p = (unsigned long *) (cp);										\
			COFF_RAWDATA((char *)(p),(char *)(p+3))							\
			p += 3;															\
			if (ent->nargs >= 3) {											\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
			COFF_RAWDATA((char *)(p),(char *)(p+5))							\
			p += 5;															\
         	offs = (long)(*p & MASK26); 									\
			if (offs & 0x02000000) 											\
				offs |= ~MASK26;											\
         	paddr = (Code *) (p + offs);				 					\
			toent = (ntbl_entry *)(paddr - NTBL_ENTRYSIZE);					\
			ntbl_entry_name(toent,"code",buf);								\
			COFF_RELOC_SYMBOL(buf,P_R_REL26)								\
			COFF_LONG_RAWDATA(*p++ & ~MASK26) 								\
			COFF_RAWDATA((char *)p,((char *)(cp))+76) 						\
		}

#define PROC_CODE_BUILTIN_JMP(cp)											\
		{																	\
			Code *paddr;													\
			long offs;														\
         	offs = (long)(*((long *)(cp)) & MASK26); 						\
			if (offs & 0x02000000) 											\
				offs |= ~MASK26;											\
         	paddr = (Code *) (((long *)(cp)) + offs);	 					\
			COFF_RELOC_SYMBOL(builtin_name(paddr),P_R_REL26)				\
			COFF_LONG_RAWDATA(*(unsigned long *)(cp) & ~MASK26) 			\
			COFF_RAWDATA(((char *)(cp))+4,((char *)(cp))+76) 				\
		}

#define PROC_CODE_BUILTIN_CALL(cp)											\
		{																	\
			Code *paddr;													\
			long offs;														\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+8) 					\
         	offs = (long)(*(((long *)(cp))+2) & MASK26); 					\
			if (offs & 0x02000000) 											\
				offs |= ~MASK26;											\
         	paddr = (Code *) ((((long *)(cp))+2) + offs);					\
			COFF_RELOC_SYMBOL(builtin_name(paddr),P_R_REL26)				\
			COFF_LONG_RAWDATA(*((unsigned long *)(cp)+2) & ~MASK26)			\
			COFF_RAWDATA(((char *)(cp))+12,((char *)(cp))+76) 				\
		}

#define PROC_CODE_BUILTIN_EQUAL(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+12) 					\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_unify),P_R_REL26)	\
			COFF_LONG_RAWDATA(*((unsigned long *)(cp)+3) & ~MASK26)			\
			COFF_RAWDATA(((char *)(cp))+16,((char *)(cp))+76) 				\
		}

#define PROC_CODE_BUILTIN_TRUE(cp)											\
		{																	\
			COFF_RAWDATA((char *)(cp),((char *)(cp))+76) 					\
		}


#define PROC_CODE_SINGLE(cp,ent,procname)									\
		{																	\
			int  emask;														\
			unsigned long *p;												\
			char buf[64];													\
			p = (unsigned long *) (cp);										\
			emask = emaskCode(ent->first_clause);							\
			if (emask & EMSK_OLDE) {										\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
			if (emask & EMSK_CP) {											\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
			if (emask & EMSK_A1) {											\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
			if (emask & EMSK_A2) {											\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
			if (emask & EMSK_A3) {											\
				SLICON_FILTER_CODE(p)										\
				COFF_LONG_RAWDATA(*p++)										\
			}																\
    		sprintf(buf,"%s_%d_%s", 										\
				procname,clauseId(ent->first_clause),"dstart");				\
			COFF_RELOC_SYMBOL(buf,P_R_REL26)								\
			COFF_LONG_RAWDATA(*p++ & ~MASK26) 								\
			COFF_RAWDATA((char *)p,((char *)(cp))+76)		 				\
		}

#define PROC_CODE_MULTIPLE(cp,ent,procname)									\
		{																	\
			unsigned long *p;												\
			long try_symidx;												\
			char buf1[64];													\
			char buf2[64];													\
			p = (unsigned long *) (cp);										\
			try_symidx =													\
			  trytab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs]; 	\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(try_symidx),P_R_REL26)		\
			COFF_LONG_RAWDATA(*p++ & ~MASK26)			 					\
			COFF_LONG_RAWDATA(*p++)						 					\
    		sprintf(buf1,"%s_%d_%s",procname,								\
				clauseId(nextClauseAddr(ent->first_clause)),"choice");		\
    		sprintf(buf2,"%s_%d_%s",procname,								\
				clauseId(ent->first_clause),"code");						\
			COFF_SHORT_RAWDATA(*(unsigned short *)(p))						\
			COFF_RELOC_SYMBOL(buf1,P_R_RELL16)								\
			COFF_SHORT_RAWDATA(0)											\
			p++;															\
			COFF_RELOC_SYMBOL(buf2,P_R_REL26)								\
			COFF_LONG_RAWDATA(*p++ & ~MASK26) 								\
			COFF_SHORT_RAWDATA(*(unsigned short *)(p))						\
			COFF_RELOC_SYMBOL(buf1,P_R_RELH16)								\
			COFF_SHORT_RAWDATA(0)											\
			p++;															\
			COFF_RAWDATA((char *)p,((char *)(cp))+76)		 				\
		}

#define PROC_CODE_UNDEFINED(cp,ent)											\
		{																	\
			unsigned long *p;												\
			long rr_symidx;													\
			p = (unsigned long *) (cp);										\
			rr_symidx =														\
			  rrtab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs];  	\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(rr_symidx),P_R_REL26)			\
			COFF_LONG_RAWDATA(*p++ & ~MASK26)			 					\
			COFF_RAWDATA((char *)p,((char *)(cp))+76)		 				\
		}



#define CLAUSE_RETRY_ME_INST(procname,ca,ent)								\
		{																	\
			int  n;															\
			unsigned long *p;												\
			long retry_symidx;												\
			char buf[64];													\
			p = (unsigned long *) (choiceEntry(ca));						\
			n = (ent->nargs > NAREGS) ? NAREGS : ent->nargs;  				\
			if (emaskCode(ca) & EMSK_CP) 									\
				retry_symidx =  retry_tab_symidx[n];						\
			else 															\
				retry_symidx =  retry_u_tab_symidx[n];						\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(retry_symidx),P_R_REL26)		\
			COFF_LONG_RAWDATA(*p++ & ~MASK26)			 					\
			COFF_LONG_RAWDATA(*p++)						 					\
        	sprintf(buf,"%s_%d_%s",											\
					procname,clauseId(nextClauseAddr(ca)),"choice"); 		\
			COFF_SHORT_RAWDATA(*(unsigned short *)(p))						\
			COFF_RELOC_SYMBOL(buf,P_R_RELL16)								\
			COFF_SHORT_RAWDATA(0)											\
			p++;															\
			COFF_SHORT_RAWDATA(*(unsigned short *)(p))						\
			COFF_RELOC_SYMBOL(buf,P_R_RELH16)								\
			COFF_SHORT_RAWDATA(0)											\
		}

#define CLAUSE_TRUST_ME_INST(ca,ent)										\
		{																	\
			int  n;															\
			unsigned long *p;												\
			long trust_symidx;												\
			p = (unsigned long *) (choiceEntry(ca));						\
			n = (ent->nargs > NAREGS) ? NAREGS : ent->nargs;  				\
			if (emaskCode(ca) & EMSK_CP) 									\
				trust_symidx =  trust_tab_symidx[n];						\
			else 															\
				trust_symidx =  trust_u_tab_symidx[n];						\
			COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(trust_symidx),P_R_REL26)		\
			COFF_LONG_RAWDATA(*p++ & ~MASK26)			 					\
			COFF_LONG_RAWDATA(*p++)						 					\
			COFF_LONG_RAWDATA(*p++)						 					\
			COFF_LONG_RAWDATA(*p++)						 					\
		}

#endif 	/* arch_m88k */






#endif /* PACKAGE */

