/*
 * machinst.h			-- include file for 68020 code generator
 *	Copyright (c) 1987-1993 by Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation Date: 3/22/87
 * Revision History:
 * 10/26/94 - C. Houpt -- Removed unnecessary extern definition of ic_ptr.
 *						Fixed ic_putl() to use union ic_uptr to put longs.
 */

#include "wintcode.h"
#include "icodegen.h"

#define SP	7		/* address register assignments */
#define E	6

#ifdef MacOS
#define H   	2
#else
#define H	5
#endif

#define TR	4
#define B	3

#ifndef MacOS
#define Fail 2
#endif

#define S	1
#define A0	0

#define OV	7		/* data register assignments	*/
#define SPB	6
#define HB	5
#define	D0	0
#define	D1	1
#define D2	2

#define DDIRECT 0		/* some of the addressing modes */
#define ADIRECT	1
#define INDIRECT 2
#define POSTINCR 3
#define PREDECR 4
#define DISPL	5
#define MEMINDIRECT 6
#define ABSADDR 7		/* other modes not utilized here are 7 too */

#define UNIFY ((int) wm_unify)
#define DOCUT ((int) wm_docut)
#define OVERFLOW ((int) wm_overflow)

#define ic_put(data) *ic_ptr++ = (data)
#define ic_putl(data) *(ic_uptr.long_ptr)++ = (long) (data)

/*
 * The following label manipulation macros are only good for short 
 * displacements
 */
#define PATCHDISP(v) *((v)-1) |= ((ic_ptr-(v))*2);
#define LDISP(v) (((v)-ic_ptr-1)*2)	/* used for backward refs */
#define LABEL(v) v = ic_ptr;	

/*
 * Labels - for icmath.c  (backwards and forwards):
 * NOTE: for 68k, we have some ugliness.  Whereas with a RISC, we always place
 * the displacement in the low bits of the branch instruction, this is only
 * true on the 68k for 8-bit disp.  Otherwise, 1 or 2 extension words are
 * used.  This complicates things when trying to handle long forward branches.
 * To avoid making this look like a 2-pass assembler, we don't implement
 * a solution to this - it turns out we _shouldn't_ need more than 8 bits
 * here on conditional branches and the like.  We do need more than 8 bits
 * for bsrs, and these are implemented as below, but notice patching isn't
 * supported here (must branch to an already known label).
 *  
 *
 *      A label may occur before it appears in a branch instruction of after.
 *      If it appears before, it is easy to compute the displacement when the
 *      instruction is emitted.  If it appears after, we must patch the
 *      displacement field at the time the label is defined.  To this end,
 *      we will distinguish between forward branches and backward branches by
 *      using different label declarations.
 *
 *      A label which occurs before its use in an instruction is declared with
 *
 *              BLABDCL(label)
 *
 *      When it is used in a branch instruction, it would be used as follows
 *
 *                      BNE(BLAB(label))
 *
 *      It is permissible to have more than one branch back to a label since
 *      the label variable is fixed and referes to the position to branch back
 *      to.  A forward label variable on the other hand refers to the position
 *      to fix up.  Therefore, multiple forward branches to the same location
 *      should use different label variables.  A forward label is used in a
 *      branch instruction as follows:
 *
 *                      BNE(FLAB(label))
 *
 *      FLABDCL actually records the location to fix up in the label variable.
 *      The displacement is patched with a forward label declaration which
 *      will appear as follows:
 *
 *              FLABDCL(label)
 *
 *      FLAB and FLABDCL may be only be used for 8-bit displacements.
 *     
 *
 *
 */

#define BLABDCL(label) label = ic_ptr;
#define BLAB(label) ((((Code *)(label))-ic_ptr) * 2)
#define FLAB(label) ((label=ic_ptr),0)
#define FLABDCL(label) *label |= (((ic_ptr-(label) - 1)*2) & 0x000000ff);

typedef Code * LAB;





#define ADDDE(dreg,reg,mode,disp) \
	{ ic_put(0150600 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define ADDED(reg,mode,disp,dreg) \
	{ ic_put(0150200 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define ADDQ(num,reg,mode,disp) \
	{ ic_put(0x5080 | (((num)&7)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp);		}
#define ADDIL(num,dreg,dmode,ddisp) \
	{ ic_put(003200 | ((dmode)<<3) | (dreg)); \
	  ic_putl(num); \
	  if (dmode == DISPL) ic_put(ddisp); }
#define ADDIAW(num,areg) \
	{ ic_put(0150374 | ((areg) << 9)); ic_put(num); }
#define ANDDD(sreg,dreg) ic_put(0xc080 | ((dreg)<<9) | (sreg));
#define ANDIW(data,reg,mode,disp) \
	{ ic_put(001100 | ((mode)<<3) | (reg)); ic_put(data); \
	  if (mode == DISPL) ic_put(disp); }

#define ANDDE(dreg,reg,mode,disp) \
	{ ic_put(0140600 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define ANDED(reg,mode,disp,dreg) \
	{ ic_put(0140200 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }

#define ASLDD(sreg,dreg) ic_put(0160640 | ((sreg)<<9) | (dreg));
#define ASRDD(sreg,dreg) ic_put(0160240 | ((sreg)<<9) | (dreg));

#define BCC(disp) ic_put(0x6400 | ((disp)&0xff));
#define BCS(disp) ic_put(0x6500 | ((disp)&0xff));
#define BEQ(disp) ic_put(0x6700 | ((disp)&0xff));
#define BGE(disp) ic_put(0x6c00 | ((disp)&0xff));
#define BGT(disp) ic_put(0x6e00 | ((disp)&0xff));
#define BHI(disp) ic_put(0x6200 | ((disp)&0xff));
#define BHS(disp) ic_put(0x6400 | ((disp)&0xff));
#define BLE(disp) ic_put(0x6f00 | ((disp)&0xff));
#define BLO(disp) ic_put(0x6500 | ((disp)&0xff));
#define BLS(disp) ic_put(0x6300 | ((disp)&0xff));
#define BLT(disp) ic_put(0x6d00 | ((disp)&0xff));
#define BNE(disp) ic_put(0x6600 | ((disp)&0xff));
#define BPL(disp) ic_put(0x6a00 | ((disp)&0xff));
#define BRA(disp) ic_put(0x6000 | ((disp)&0xff));
#define BVC(disp) ic_put(0x6800 | ((disp)&0xff));
#define BVS(disp) ic_put(0x6900 | ((disp)&0xff));
#define BSR(disp) { \
	  if ((disp) > 32767 || (disp) < -32767)  {  \
      		ic_put(0x61ff); \
		ic_putl((disp)); } \
	  else if ((disp) > 127 || (disp) < -127) {  \
    		ic_put(0x6100); \
		ic_put((disp)); } \
       	  else  \
	 	ic_put(0x6100 | ((disp)&0xff)); }
#define BSRL(disp) { \
    ic_put(0x61ff); \
    ic_putl((disp)); }
#define CLRD(dreg) ic_put(041200 | (dreg));
#define CMP(reg,mode,disp,dreg) \
	{ ic_put(0xb080 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define CMPAbs(addr,dreg) \
	{ ic_put(0xb0b9 | ((dreg)<<9)); ic_putl(addr); }
#define CMPAAbs(addr,areg) \
	{ ic_put(0xb1f9 | ((areg)<<9)); ic_putl(addr); }
#define CMPA(reg,mode,disp,areg) \
	{ ic_put(0xb1c0 | ((areg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define CMPAD(areg,dreg) ic_put(0xb088 | ((dreg)<<9) | (areg));
#define CMPDD(dreg1,dreg2) ic_put(0xb080 | ((dreg2)<<9) | (dreg1));
#define CMPI(const,dreg,dmode,ddisp) \
	{ ic_put(0x0c80 | ((dmode)<<3) | (dreg)); ic_putl(const); \
	  if (dmode == DISPL) ic_put(ddisp); }
#define DIVS(reg,mode,disp,qreg,rreg) \
	{ ic_put(046100 | ((mode)<<3) | (reg)); \
	  ic_put(004000 | ((qreg)<<12) | (rreg)); \
	  if (mode == DISPL) ic_put(disp); }

#define EOR(dreg,reg,mode,disp) \
	{ ic_put(0130600 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }

#define EXGDA(dreg,areg) ic_put(0x140610 | ((dreg)<<9) | (areg));

#ifdef MacOS
#define DoFail {extern long *Fail; \
              {MOVAbs((long) &Fail,0,ADIRECT,0); \
			  JMPR(0);}}
#else
#define DoFail {JMPR(Fail);}
#endif

#define LEA(sreg,smode,sdisp,areg) \
	{ if (smode == ABSADDR) { \
	    ic_put(0x41c1 | ((areg)<<9) | ((smode)<<3)); \
	    ic_putl((sdisp)); \
	  } \
	  else { \
	      ic_put(0x41c0 | ((areg)<<9) | ((smode)<<3) | (sreg)); \
	      if (smode == DISPL) ic_put(sdisp); \
	    } \
	}
#define LEA_PCREL(reg) ic_put(040772 | ((reg)<<9));
#define LINK(reg,data) {ic_put(047120 | (reg)); ic_put(data);}
#define LSLND(n,d) ic_put(0160610 | (((n)&7)<<9) | (d));
#define LSRND(n,d) ic_put(0160210 | (((n)&7)<<9) | (d));
#define MOVE(sreg,smode,sdisp,dreg,dmode,ddisp) \
      { ic_put(0x2000|((dreg)<<9)|((dmode)<<6)|((smode)<<3)|(sreg)); \
	if (smode == DISPL) ic_put(sdisp); \
	if (dmode == DISPL) ic_put(ddisp); }
#define MOVAbs(addr,dreg,dmode,ddisp) \
	{ ic_put(0x2039 | ((dreg)<<9) | ((dmode)<<6)); \
	  ic_putl(addr); }
#define MOVAddr2Addr(srcaddr,dstaddr) \
    { ic_put(0x23fc);  \
	  ic_putl(srcaddr); \
	  ic_putl(dstaddr);}
#define MOVReg2Addr(sreg,smode,dstaddr) \
    { ic_put(0x23c0 | ((smode << 3) | (sreg)) ); \
      ic_putl(dstaddr);}
#define MOVI2Addr(immed,addr) \
	{ ic_put(021774); ic_putl(immed); ic_putl(addr); }
/* I think that MOVtoAbs and MOVReg2Addr perform the same operation,
 * but the arguments are different.  Scott defined MOVtoAbs for icmath.c
 * At some point in the future these should be consolidated.
 */
#define MOVtoAbs(addr,sreg,smode,sdisp) \
	{ ic_put(0x23c0 | ((smode) <<3) | (sreg)); \
	  ic_putl(addr); }
#define MOVI(lconst,dreg,dmode,ddisp) \
	{ ic_put(0x203c | ((dreg)<<9) | ((dmode)<<6)); \
	  ic_putl(lconst);	\
	  if (dmode == DISPL) ic_put(ddisp); }
#define MOVIW(wconst,reg,mode,disp) \
	{ ic_put(0x303c | ((reg)<<9) | ((mode)<<6));	\
	  ic_put(wconst);				\
	  if (mode == DISPL) ic_put(disp); }
#define MOVEQ(sconst,dreg) ic_put(0x7000 | ((dreg)<<9) | ((sconst) & 0xff));
#define MOVEIMMTOD(const,dreg) \
/* do a MOVEQ if the signed constant is small enough, else use MOVE */ \
  { if ((const) > 127 || (const) < -127)  /* 16/32 bit displacement necessary */ \
	MOVI(((long) (const)),dreg,DDIRECT,0) \
    else MOVEQ((const),dreg); }
#define MULS(reg,mode,disp,dreg) \
	{ ic_put(046000 | ((mode) << 3) | (reg)); \
	  ic_put(004000 | ((dreg)<<12)); \
	  if (mode == DISPL) ic_put(disp); }

#define NEG(reg,mode,disp) \
	{ ic_put(042200 | ((mode) << 3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }

#define NOP ic_put(0x4e71);

#define NOT(reg,mode,disp) \
	{ ic_put(043200 | ((mode) << 3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }

#define ORDE(dreg,reg,mode,disp) \
	{ ic_put(0100600 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define ORED(reg,mode,disp,dreg) \
	{ ic_put(0100200 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }

#define ORIW(data,reg,mode,disp) \
	{ ic_put(000100 | ((mode)<<3) | (reg)); ic_put(data); \
	  if (mode == DISPL) ic_put(disp); }
#define JSR(addr) {ic_put(047271);ic_putl((int)(addr));}
#define JSRW(addr) {ic_put(047270); ic_put(((int)(addr))&0xffff); }
#define JSRX(addr) {if ((unsigned long)(addr)<32768) JSRW(addr) else JSR(addr);}
#define JSRI(reg,mode,disp) \
  if (mode == INDIRECT)  /* reg indirect */  \
  	ic_put(047220 | (reg)); \
  else if (mode == MEMINDIRECT) { /* simple memory indirect ==  ([disp,Areg]) */ \
      		ic_put(047260 | (reg)); \
		ic_put(000541);  /* emit as mem indir. preindexed but suppress index reg */ \
		ic_put((disp)); /* assume word disp */ }
#define JMP(addr) {ic_put(0x4ef9);ic_putl((long)(addr));}
#define JMPR(reg) {ic_put(047320 | (reg));}
#define RTS ic_put(0x4e75);
#define SUBAD(areg,dreg) ic_put(0x9088 | ((dreg)<<9) | (areg));
#define SUBDE(dreg,reg,mode,disp) \
	{ ic_put(0110600 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define SUBED(reg,mode,disp,dreg) \
	{ ic_put(0110200 | ((dreg)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp); }
#define ADDQ(num,reg,mode,disp) \
	{ ic_put(0x5080 | (((num)&7)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp);		}
#define SUBQ(num,reg,mode,disp) \
	{ ic_put(0x5180 | (((num)&7)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp);		}
#define SUBQAbs(num,disp) \
	{ ic_put(0x51b9 | (((num)&7)<<9)); ic_putl(disp); }
#define SUBQW(num,reg,mode,disp) \
	{ ic_put(0x5140 | (((num)&7)<<9) | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp);		}
#define TST(reg,mode,disp) \
	{ ic_put(045200 | ((mode)<<3) | (reg)); \
	  if (mode == DISPL) ic_put(disp);		}

#define UNLK(reg) ic_put(047130 | (reg));

#define COMPUTE_MODE(base,disp,reg,mode) \
   {  if (base == REGS) {	\
	 reg = disp;		\
	 mode = 0;		\
      }				\
      else {			\
	 disp *= 4;		\
	 reg = base;\
	 mode = disp ? 5 : 2;	\
      } }

extern Code *ic_macropatch1;
extern Code *ic_macropatch2;

/* Do not call these as functions... */
extern	void	wm_unify	PARAMS(( void ));
extern	void	wm_docut	PARAMS(( void ));
extern	void	wm_g_uia	PARAMS(( void ));
extern	void	wm_p_uia	PARAMS(( void ));
extern	void	wm_g_sym	PARAMS(( void ));
extern	void	wm_g_dbl	PARAMS(( void ));
extern	void	wm_u_lval	PARAMS(( void ));
extern	void	wm_p_unsafe	PARAMS(( void ));
extern	void	wm_u_sym	PARAMS(( void ));
extern	void	wm_u_int	PARAMS(( void ));
extern	void	wm_g_int	PARAMS(( void ));
extern	void	cmp_sym_uia	PARAMS(( void ));
extern	void	wm_overflow	PARAMS(( void ));
