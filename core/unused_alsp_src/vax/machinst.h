/*
 * machinst.h		-- Code Generation macros for VAX
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 33/10/90
 *
 */

#include "icodegen.h"
/*
 * ic_put and variations
 */

#define ic_put(bdat) *ic_ptr++ = ((Code) (bdat))
#define ic_putw(wdat) *((short *) ic_ptr)++ = ((short) (wdat))
#define ic_putl(ldat) *((long *) ic_ptr)++ = ((long) (ldat))


/*
 * Registers
 */

#define r0	0
#define r1	1
#define r2	2
#define T1	3
#define S	4
#define SAFETY	5
#define H	6
#define TR	7
#define FAIL	8
#define HB	9
#define SPB	10
#define B	11
#define E	12
#define SP	14


/*
 * Labels:
 *
 *	A label may occur before it appears in a branch instruction or after.
 *	If it appears before, it is easy to compute the displacement when the
 *	instruction is emitted.  If it appears after, we must patch the
 *	displacement field at the time the label is defined.  To this end,
 *	we will distingish between forward branches and backward branches by
 *	using different label declarations.
 *
 *	A label which occurs before its use in an instruction is declared with
 *
 *		BLABDCL(label)
 *
 *	When it is used in a branch instruction it would be used as follows
 *
 *			bneq	BLAB(label)
 *
 *	It is permissible to have more than one branch back to a label since
 *	the label variable is fixed and refers to the position to branch back
 *	to.  A forward label variable on the other hand refers to the position
 *	to fix up.  Therefore, multiple forward branches to the same location
 *	should use different label variables.  A forward label is used in a
 *	branch instructions as follows:
 *
 *			bneq	FLAB(label)
 *	
 *
 *	FLABDCL actually records the location to fix up in the label variable.
 *	The displacement is patched with a forward label declaration which
 *	will appear as follows:
 *
 *		FLABDCL(label)
 *
 *
 */

#define BLABDCL(label) label = ic_ptr;
#define BLAB(label) { register int disp = (label)-ic_ptr-1; *ic_ptr++ = disp; }
#define BLABW(label) { register int disp = (label)-ic_ptr-2; *(short *)ic_ptr++ = disp; }

	    /* in FLAB, we create a branch to itself to aid error detection */
#define FLAB(label) label = ic_ptr; *ic_ptr++ = -2; 
#define FLABDCL(label) *label = ic_ptr-label-1;

typedef Code * LABEL;


/*
 * Instructions
 */

#define halt	ic_put(iHALT);
#define nop	ic_put(iNOP);
#define rei	ic_put(iREI);
#define bpt	ic_put(iBPT);
#define ret	ic_put(iRET);
#define rsb	ic_put(iRSB);
#define ldpctx	ic_put(iLDPCTX);
#define svpctx	ic_put(iSVPCTX);
#define cvtps	ic_put(iCVTPS);
#define cvtsp	ic_put(iCVTSP);
#define index	ic_put(iINDEX);
#define crc	ic_put(iCRC);
#define prober	ic_put(iPROBER);
#define probew	ic_put(iPROBEW);
#define insque	ic_put(iINSQUE);
#define remque	ic_put(iREMQUE);
#define bsbb	ic_put(iBSBB);
#define brb	ic_put(iBRB);
#define bneq	ic_put(iBNEQ);
#define beql	ic_put(iBEQL);
#define bgtr	ic_put(iBGTR);
#define bleq	ic_put(iBLEQ);
#define jsb	ic_put(iJSB);
#define jmp	ic_put(iJMP);
#define bgeq	ic_put(iBGEQ);
#define blss	ic_put(iBLSS);
#define bgtru	ic_put(iBGTRU);
#define blequ	ic_put(iBLEQU);
#define bvc	ic_put(iBVC);
#define bvs	ic_put(iBVS);
#define bgequ	ic_put(iBGEQU);
#define blssu	ic_put(iBLSSU);
#define addp4	ic_put(iADDP4);
#define addp6	ic_put(iADDP6);
#define subp4	ic_put(iSUBP4);
#define subp6	ic_put(iSUBP6);
#define cvtpt	ic_put(iCVTPT);
#define mulp	ic_put(iMULP);
#define cvttp	ic_put(iCVTTP);
#define divp	ic_put(iDIVP);
#define movc3	ic_put(iMOVC3);
#define cmpc3	ic_put(iCMPC3);
#define scanc	ic_put(iSCANC);
#define spanc	ic_put(iSPANC);
#define movc5	ic_put(iMOVC5);
#define cmpc5	ic_put(iCMPC5);
#define movtc	ic_put(iMOVTC);
#define movtuc	ic_put(iMOVTUC);
#define bsbw	ic_put(iBSBW);
#define brw	ic_put(iBRW);
#define cvtwl	ic_put(iCVTWL);
#define cvtwb	ic_put(iCVTWB);
#define movp	ic_put(iMOVP);
#define cmpp3	ic_put(iCMPP3);
#define cvtpl	ic_put(iCVTPL);
#define cmpp4	ic_put(iCMPP4);
#define editpc	ic_put(iEDITPC);
#define matchc	ic_put(iMATCHC);
#define locc	ic_put(iLOCC);
#define skpc	ic_put(iSKPC);
#define movzwl	ic_put(iMOVZWL);
#define acbw	ic_put(iACBW);
#define movaw	ic_put(iMOVAW);
#define pushaw	ic_put(iPUSHAW);
#define addf2	ic_put(iADDF2);
#define addf3	ic_put(iADDF3);
#define subf2	ic_put(iSUBF2);
#define subf3	ic_put(iSUBF3);
#define mullf2	ic_put(iMULLF2);
#define mullf3	ic_put(iMULLF3);
#define divf2	ic_put(iDIVF2);
#define divf3	ic_put(iDIVF3);
#define cvtfb	ic_put(iCVTFB);
#define cvtfw	ic_put(iCVTFW);
#define cvtfl	ic_put(iCVTFL);
#define cvtrfl	ic_put(iCVTRFL);
#define cvtbf	ic_put(iCVTBF);
#define cvtwf	ic_put(iCVTWF);
#define cvtlf	ic_put(iCVTLF);
#define acbf	ic_put(iACBF);
#define movf	ic_put(iMOVF);
#define cmpf	ic_put(iCMPF);
#define mnegf	ic_put(iMNEGF);
#define tstf	ic_put(iTSTF);
#define emodf	ic_put(iEMODF);
#define polyf	ic_put(iPOLYF);
#define cvtfd	ic_put(iCVTFD);
#define reserved_to_digital57	ic_put(iRESERVED_TO_DIGITAL57);
#define adawi	ic_put(iADAWI);
#define reserved_to_digital59	ic_put(iRESERVED_TO_DIGITAL59);
#define reserved_to_digital5a	ic_put(iRESERVED_TO_DIGITAL5a);
#define reserved_to_digital5b	ic_put(iRESERVED_TO_DIGITAL5b);
#define insqhi	ic_put(iINSQHI);
#define insqti	ic_put(iINSQTI);
#define remqhi	ic_put(iREMQHI);
#define remqti	ic_put(iREMQTI);
#define addd2	ic_put(iADDD2);
#define addd3	ic_put(iADDD3);
#define subd2	ic_put(iSUBD2);
#define subd3	ic_put(iSUBD3);
#define muld2	ic_put(iMULD2);
#define muld3	ic_put(iMULD3);
#define divd2	ic_put(iDIVD2);
#define divd3	ic_put(iDIVD3);
#define cvtdb	ic_put(iCVTDB);
#define cvtdw	ic_put(iCVTDW);
#define cvtdl	ic_put(iCVTDL);
#define cvtrdl	ic_put(iCVTRDL);
#define cvtbd	ic_put(iCVTBD);
#define cvtwd	ic_put(iCVTWD);
#define cvtld	ic_put(iCVTLD);
#define acbd	ic_put(iACBD);
#define movd	ic_put(iMOVD);
#define cmpd	ic_put(iCMPD);
#define mnegd	ic_put(iMNEGD);
#define tstd	ic_put(iTSTD);
#define emodd	ic_put(iEMODD);
#define polyd	ic_put(iPOLYD);
#define cvtdf	ic_put(iCVTDF);
#define reserved_to_digital77	ic_put(iRESERVED_TO_DIGITAL77);
#define ashl	ic_put(iASHL);
#define ashq	ic_put(iASHQ);
#define emul	ic_put(iEMUL);
#define ediv	ic_put(iEDIV);
#define clrq	ic_put(iCLRQ);
#define movq	ic_put(iMOVQ);
#define movaq	ic_put(iMOVAQ);
#define pushaq	ic_put(iPUSHAQ);
#define addb2	ic_put(iADDB2);
#define addb3	ic_put(iADDB3);
#define subb2	ic_put(iSUBB2);
#define subb3	ic_put(iSUBB3);
#define mulb2	ic_put(iMULB2);
#define mulb3	ic_put(iMULB3);
#define divb2	ic_put(iDIVB2);
#define divb3	ic_put(iDIVB3);
#define bisb2	ic_put(iBISB2);
#define bisb3	ic_put(iBISB3);
#define bicb2	ic_put(iBICB2);
#define bicb3	ic_put(iBICB3);
#define xorb2	ic_put(iXORB2);
#define xorb3	ic_put(iXORB3);
#define mnegb	ic_put(iMNEGB);
#define caseb	ic_put(iCASEB);
#define movb	ic_put(iMOVB);
#define cmpb	ic_put(iCMPB);
#define mcomb	ic_put(iMCOMB);
#define bitb	ic_put(iBITB);
#define clrb	ic_put(iCLRB);
#define tstb	ic_put(iTSTB);
#define incb	ic_put(iINCB);
#define decb	ic_put(iDECB);
#define cvtbl	ic_put(iCVTBL);
#define cvtbw	ic_put(iCVTBW);
#define movzbl	ic_put(iMOVZBL);
#define movzbw	ic_put(iMOVZBW);
#define rotl	ic_put(iROTL);
#define acbb	ic_put(iACBB);
#define movab	ic_put(iMOVAB);
#define pushab	ic_put(iPUSHAB);
#define addw2	ic_put(iADDW2);
#define addw3	ic_put(iADDW3);
#define subw2	ic_put(iSUBW2);
#define subw3	ic_put(iSUBW3);
#define mulw2	ic_put(iMULW2);
#define mulw3	ic_put(iMULW3);
#define divw2	ic_put(iDIVW2);
#define divw3	ic_put(iDIVW3);
#define bisw2	ic_put(iBISW2);
#define bisw3	ic_put(iBISW3);
#define bicw2	ic_put(iBICW2);
#define bicw3	ic_put(iBICW3);
#define xorw2	ic_put(iXORW2);
#define xorw3	ic_put(iXORW3);
#define mnegw	ic_put(iMNEGW);
#define casew	ic_put(iCASEW);
#define movw	ic_put(iMOVW);
#define cmpw	ic_put(iCMPW);
#define mcomw	ic_put(iMCOMW);
#define bitw	ic_put(iBITW);
#define clrw	ic_put(iCLRW);
#define tstw	ic_put(iTSTW);
#define incw	ic_put(iINCW);
#define decw	ic_put(iDECW);
#define bispsw	ic_put(iBISPSW);
#define bicpsw	ic_put(iBICPSW);
#define popr	ic_put(iPOPR);
#define pushr	ic_put(iPUSHR);
#define chmk	ic_put(iCHMK);
#define chme	ic_put(iCHME);
#define chms	ic_put(iCHMS);
#define chmu	ic_put(iCHMU);
#define addl2	ic_put(iADDL2);
#define addl3	ic_put(iADDL3);
#define subl2	ic_put(iSUBL2);
#define subl3	ic_put(iSUBL3);
#define mull2	ic_put(iMULL2);
#define mull3	ic_put(iMULL3);
#define divl2	ic_put(iDIVL2);
#define divl3	ic_put(iDIVL3);
#define bisl2	ic_put(iBISL2);
#define bisl3	ic_put(iBISL3);
#define bicl2	ic_put(iBICL2);
#define bicl3	ic_put(iBICL3);
#define xorl2	ic_put(iXORL2);
#define xorl3	ic_put(iXORL3);
#define mnegl	ic_put(iMNEGL);
#define casel	ic_put(iCASEL);
#define movl	ic_put(iMOVL);
#define cmpl	ic_put(iCMPL);
#define mcoml	ic_put(iMCOML);
#define bitl	ic_put(iBITL);
#define clrl	ic_put(iCLRL);
#define tstl	ic_put(iTSTL);
#define incl	ic_put(iINCL);
#define decl	ic_put(iDECL);
#define adwc	ic_put(iADWC);
#define sbwc	ic_put(iSBWC);
#define mtpr	ic_put(iMTPR);
#define mfpr	ic_put(iMFPR);
#define movpsl	ic_put(iMOVPSL);
#define pushl	ic_put(iPUSHL);
#define moval	ic_put(iMOVAL);
#define pushal	ic_put(iPUSHAL);
#define bbs	ic_put(iBBS);
#define bbc	ic_put(iBBC);
#define bbss	ic_put(iBBSS);
#define bbcs	ic_put(iBBCS);
#define bbsc	ic_put(iBBSC);
#define bbcc	ic_put(iBBCC);
#define bbssi	ic_put(iBBSSI);
#define bbcci	ic_put(iBBCCI);
#define blbs	ic_put(iBLBS);
#define blbc	ic_put(iBLBC);
#define ffs	ic_put(iFFS);
#define ffc	ic_put(iFFC);
#define cmpv	ic_put(iCMPV);
#define cmpzv	ic_put(iCMPZV);
#define extv	ic_put(iEXTV);
#define extzv	ic_put(iEXTZV);
#define insv	ic_put(iINSV);
#define acbl	ic_put(iACBL);
#define aoblss	ic_put(iAOBLSS);
#define aobleq	ic_put(iAOBLEQ);
#define sobgeq	ic_put(iSOBGEQ);
#define sobgtr	ic_put(iSOBGTR);
#define cvtlb	ic_put(iCVTLB);
#define cvtlw	ic_put(iCVTLW);
#define ashp	ic_put(iASHP);
#define cvtlp	ic_put(iCVTLP);
#define callg	ic_put(iCALLG);
#define calls	ic_put(iCALLS);
#define xfc	ic_put(iXFC);
#define escd_to_digitalfd	ic_put(iESCD_TO_DIGITALfd);
#define esce_to_digitalfe	ic_put(iESCE_TO_DIGITALfe);
#define escf_to_digitalff	ic_put(iESCF_TO_DIGITALff);


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Addressing Modes
 */

/*
 * Short Immediate: These constants may range from 0 thru 63.  They use
 *	the VAX literal mode.
 */

#define SIMM(i)	ic_put(i);

/*
 * Byte Immediate: These constants may take up a byte of storage.  Care
 *	should be taken when using BIMM, WIMM, or LIMM as the one to use
 *	depends on the context.  For example,
 *		movl	#3, r0
 *
 *	would be encoded as
 *		movl	LIMM(3), REG(R0)
 *	or
 *		movl	SIMM(3), REG(R0)
 *	but not
 *		movl	BIMM(3), REG(R0)
 *
 * Each of these addressing modes are the VAX immediate mode which is a
 * PC relative addressing mode.
 */

#define BIMM(i) ic_put(0x8f); ic_put(i);

/* Word Immediate */
#define WIMM(i) ic_put(0x8f); ic_putw(i);

/* Long word Immediate */
#define LIMM(i) ic_put(0x8f); ic_putl(i);


/*
 * Register:  The paremeter r is the register from 0 thru 14.  Theoretically,
 *	r15 is permissible also, but the PC relative modes handle r15 in a
 *	more convenient and managable manner.
 */

#define REG(r)	ic_put((0x50 | (r)));


/*
 * Register Deferred:
 *
 *
 */

#define ATREG(r) ic_put((0x60 | (r)));

/*
 * Autoincrement:
 */

#define AUTOINCR(r) ic_put((0x80 | (r)));

/*
 * Autoincrement deferred:
 */

#define AUTOINCR_DEFERRED(r) ic_put((0x90 | (r)));

/*
 * Autodecrement:
 */

#define AUTODECR(r) ic_put((0x70 | (r)));

/*
 * Displacement Mode:
 *
 *	BDISP(r,d)	is byte displacement
 *	WDISP(r,d) 	is word displacement
 *	LDISP(r,d)	is longword displacment
 *
 *	DISP(r,d)	will generate either register deferred, byte
 *			displacement, or word displacement depending on the
 *			value of d
 *
 */

#define BDISP(r,d) ic_put((0xa0 | (r))); ic_put(d);
#define WDISP(r,d) ic_put((0xc0 | (r))); ic_putw(d);
#define LDISP(r,d) ic_put((0xe0 | (r))); ic_putl(d);

#define DISP(r,d)	if ((d)==0) ic_put((0x60 | (r))); \
			else if (-128 <= (d) && (d) < 128) { BDISP(r,d) } \
			else { WDISP(r,d) }

/*
 * Displacement Deferred Mode:
 */

#define ATDISP(r,d)	if (-128 <= (d) && (d) < 128) { \
				ic_put(0xb0 | (r)); ic_put(d); \
			} else { \
				ic_put(0xd0 | (r)); ic_put(d); \
			}

/*
 * Index mode:
 */

#define INDEX(r) ic_put(0x40 | (r));

/*
 * Absolute mode:
 *
 *	This is a pc-relative mode.  The normal assembler syntax is
 *
 *		@#address
 *
 */

#define ABSADDR(addr) ic_put(0x9f); ic_putl(((long) addr));

/*
 * Relative Mode:
 *
 *	This is a pc-relative mode.  The actual address is supplied to the
 *	macro.  The displacement is computed from this address.
 *
 */

#define RELADDR(addr) 	{   int d = ((Code *) addr) - ic_ptr;	\
			    if (-126 <= d && d <= 129) {	\
				ic_put(0xaf); ic_put(d-2);	\
			    } else if (-32765 <= d && d <= 32770) { \
				ic_put(0xcf); ic_putw(d-3);	\
			    } else {				\
				ic_put(0xef); ic_putl(d-5);	\
			    } }

/*
 * Relative Deferred Mode:
 *
 *	This is a pc-relative mode.  The actual address is supplied to the
 *	macro.  The displacement is computed from this address.
 *
 */

#define ATRELADDR(addr)	{   int d = ((Code *) addr) - ic_ptr;	\
			    if (-126 <= d && d <= 129) {	\
				ic_put(0xbf); ic_put(d-2);	\
			    } else if (-32765 <= d && d <= 32770) { \
				ic_put(0xdf); ic_putw(d-3);	\
			    } else {				\
				ic_put(0xff); ic_putl(d-5);	\
			    } }

/*
 * LOC Mode:  The code generator will often pass a base register
 * and displacement to various procedures.  The base register really
 * is the base register unless it is equal to REGS in which case the
 * displacement is the register number to use.  The displacement
 * is otherwise a valid longword displacement.  It must be converted
 * to a byte displacement in order to be valid.  
 *
 * All of this stuff would be doable with direct code, but it would
 * be ugly and irritating to look at.
 */

#define LOC(base,disp) 			\
	{ 				\
	    if (base == REGS) {		\
		REG(disp)		\
	    }				\
	    else {			\
		register int rdisp = disp<<2; \
		DISP(base,rdisp)	\
	    }				\
	}
