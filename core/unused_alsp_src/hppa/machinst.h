/*
 * machinst.h
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/8/93
 * Revision History:
 */

#include "icodegen.h"

/*
 * ic_put
 *
 *	This macro is responsible for writing the instruction in the code
 *	stream.  Note that the increment is done after the assignment, thus
 *	permitting the value of ic_ptr ot be used in the expression on the
 *	right hand side of the assignment operator.
 *
 *	This was a problem in previous implementations in that the C compiler
 *	would do different things on different machines.  It is not clear
 *	whether the value taken for ic_ptr on the right hand side of the
 *	assignment is before or after the increment of ic_ptr.  It actually
 *	did different things with different compilers.
 */

#define ic_put(dat) (*((long *) ic_ptr) = ((long) (dat))),ic_ptr++

/*
 * Labels:
 *
 *	A label may occur before it appears in a branch instruction of after.
 *	If it appears before, it is easy to compute the displacement when the
 *	instruction is emitted.  If it appears after, we must patch the
 *	displacement field at the time the label is defined.  To this end,
 *	we will distinguish between forward branches and backward branches by
 *	using different label declarations.
 *
 *	A label which occurs before its use in an instruction is declared with
 *
 *		BLABDCL(label)
 *
 *	When it is used in a branch instruction, it would be used as follows
 *
 *			BL(cn_,BLAB(label),CP)
 *
 *	It is permissible to have more than one branch back to a label since
 *	the label variable is fixed and referes to the position to branch back
 *	to.  A forward label variable on the other hand refers to the position
 *	to fix up.  Therefore, multiple forward branches to the same location
 *	should use different label variables.  A forward label is used in a
 *	branch instruction as follows:
 *
 *			BL(cn_,FLAB(label),CP)
 *
 *
 *	FLAB actually records the location to fix up in the label variable.
 *	The displacement is patched with a forward label declaration which
 *	will appear as follows:
 *
 *		FLABDCL12(label)
 *	or
 *		FLABDCL17(label)
 *
 *	
 *	FLABDCL12 is used to fix up an instruction requiring a twelve bit
 *	offset. FLABDCL17 is used to fix up an instruction requiring a 17
 *	bit offset.
 *
 */

#define BLABDCL(label) label = ic_ptr;
#define BLAB(label) (((Code *)(label))-ic_ptr-2)
#define FLAB(label) ((label=ic_ptr),0)
#define FLABDCL12(label) *label |= (CG_im12(ic_ptr+2-(label)));
#define FLABDCL17(label) *label |= (CG_im17(ic_ptr+2-(label)));

#define is22BitDispl(d) ((d) <= 0x1ffff && (d) >= -0x20000)

typedef Code * LABEL;


/*
 * Immediates:
 *
 * 	The HPPA requires at most two instructions to load a 32 bit value
 *	The LDIL instruction will load the high twenty-one bits into a register
 *	An LDO instruction may then be used to set the low eleven bits
 *	appropriately.
 */

#define hi21(val)	(((int) (val)) >> 11)
#define lo11(val)	(((int) (val)) & 0x07ff)


/*
 * Registers:
 *
 */

#define ZERO	0
#define Scratch	1
#define cRet	2
#define H	3
#define HB	4
#define B	5
#define TR	6
#define SPB	7
#define E	8
#define HeapBase 9
#define StkBot	10
#define OldE	11
#define CP	12
#define Fail	13
#define Safety	14
#define SP	15
#define S	16
#define tmp1	17
#define tmp2	18
#define A1	19
#define A2	20
#define A3	21
#define T1	22
#define T2	23
#define T3	24
#define UArg1	25
#define UArg2	26
#define cDP	27
#define cSP	30


/*
 * Pseudo Instructions:
 */

#define MOVE(src,dst)	OR(cl_,src,ZERO,dst)
#define NOP		OR(cl_,ZERO,ZERO,ZERO)



/*
 * Instructions:
 */

/* LDW		-- Load Word */
#define LDW(d,s,b,t) ic_put(iLDW(d,s,b,t));

/* LDH		-- Load Halfword */
#define LDH(d,s,b,t) ic_put(iLDH(d,s,b,t));

/* LDB		-- Load Byte */
#define LDB(d,s,b,t) ic_put(iLDB(d,s,b,t));

/* STW		-- Store Word */
#define STW(r,d,s,b) ic_put(iSTW(r,d,s,b));

/* STH		-- Store Halfword */
#define STH(r,d,s,b) ic_put(iSTH(r,d,s,b));

/* STB		-- Store Byte */
#define STB(r,d,s,b) ic_put(iSTB(r,d,s,b));

/* LDWM		-- Load Word And Modify */
#define LDWM(d,s,b,t) ic_put(iLDWM(d,s,b,t));

/* STWM		-- Store Word And Modify */
#define STWM(r,d,s,b) ic_put(iSTWM(r,d,s,b));

/* LDWX		-- Load Word Indexed */
#define LDWX(cmplt,x,s,b,t) ic_put(iLDWX(cmplt,x,s,b,t));

/* LDHX		-- Load Halfword Indexed */
#define LDHX(cmplt,x,s,b,t) ic_put(iLDHX(cmplt,x,s,b,t));

/* LDBX		-- Load Byte Indexed */
#define LDBX(cmplt,x,s,b,t) ic_put(iLDBX(cmplt,x,s,b,t));

/* LDWAX	-- Load Word Absolute Indexed */
#define LDWAX(cmplt,x,b,t) ic_put(iLDWAX(cmplt,x,b,t));

/* LDCWX	-- Load And Clear Word Indexed */
#define LDCWX(cmplt,x,s,b,t) ic_put(iLDCWX(cmplt,x,s,b,t));

/* LDWS		-- Load Word Short */
#define LDWS(cmplt,d,s,b,t) ic_put(iLDWS(cmplt,d,s,b,t));

/* LDHS		-- Load Halfword Short */
#define LDHS(cmplt,d,s,b,t) ic_put(iLDHS(cmplt,d,s,b,t));

/* LDBS		-- Load Byte Short */
#define LDBS(cmplt,d,s,b,t) ic_put(iLDBS(cmplt,d,s,b,t));

/* LDWAS	-- Load Word Absolute Short */
#define LDWAS(cmplt,d,b,t) ic_put(iLDWAS(cmplt,d,b,t));

/* LDCWS	-- Load And Clear Word Short */
#define LDCWS(cmplt,d,s,b,t) ic_put(iLDCWS(cmplt,d,s,b,t));

/* STWS		-- Store Word Short */
#define STWS(cmplt,r,d,s,b) ic_put(iSTWS(cmplt,r,d,s,b));

/* STHS		-- Store Halfword Short */
#define STHS(cmplt,r,d,s,b) ic_put(iSTHS(cmplt,r,d,s,b));

/* STBS		-- Store Byte Short */
#define STBS(cmplt,r,d,s,b) ic_put(iSTBS(cmplt,r,d,s,b));

/* STWAS	-- Store Word Absolute Short */
#define STWAS(cmplt,r,d,b) ic_put(iSTWAS(cmplt,r,d,b));

/* STBYS	-- Store Bytes Short */
#define STBYS(cmplt,r,d,s,b) ic_put(iSTBYS(cmplt,r,d,s,b));

/* LDO		-- Load Offset */
#define LDO(d,b,t) ic_put(iLDO(d,b,t));

/* LDIL		-- Load Immediate Left */
#define LDIL(i,t) ic_put(iLDIL(i,t));

/* ADDIL	-- Add Immediate Left */
#define ADDIL(i,r) ic_put(iADDIL(i,r));

/* BL		-- Branch And Link */
#define BL(n,target,t) ic_put(iBL(n,target,t));

/* GATE		-- Gateway */
#define Gate(n,target,t) ic_put(iGate(n,target,t));

/* BLR		-- Branch And Link Register */
#define BLR(n,x,t) ic_put(iBLR(n,x,t));

/* BV		-- Branch Vectored */
#define BV(n,x,b) ic_put(iBV(n,x,b));

/* BE		-- Branch External */
#define BE(n,wd,sr,b) ic_put(iBE(n,wd,sr,b));

/* BLE		-- Branch And Link External */
#define BLE(n,wd,sr,b) ic_put(iBLE(n,wd,sr,b));

/* MOVB		-- Move And Branch */
#define MOVB(cond,n,r1,r2,target) ic_put(iMOVB(cond,n,r1,r2,target));

/* MOVIB	-- Move Immediate And Branch */
#define MOVIB(cond,n,i,r2,target) ic_put(iMOVIB(cond,n,i,r2,target));

/* COMBT	-- Compare And Branch If True */
#define COMBT(cond,n,r1,r2,target) ic_put(iCOMBT(cond,n,r1,r2,target));

/* COMBF	-- Compare And Branch If False */
#define COMBF(cond,n,r1,r2,target) ic_put(iCOMBF(cond,n,r1,r2,target));

/* COMIBT	-- Compare Immediate And Branch If True */
#define COMIBT(cond,n,i,r2,target) ic_put(iCOMIBT(cond,n,i,r2,target));

/* COMIBF	-- Compare Immediate And Branch If False */
#define COMIBF(cond,n,i,r2,target) ic_put(iCOMIBF(cond,n,i,r2,target));

/* ADDBT	-- Add And Branch If True */
#define ADDBT(cond,n,r1,r2,target) ic_put(iADDBT(cond,n,r1,r2,target));

/* ADDBF	-- Add And Branch If False */
#define ADDBF(cond,n,r1,r2,target) ic_put(iADDBF(cond,n,r1,r2,target));

/* ADDIBT	-- Add Immediate And Branch If True */
#define ADDIBT(cond,n,i,r2,target) ic_put(iADDIBT(cond,n,i,r2,target));

/* ADDIBF	-- Add Immediate And Branch If False */
#define ADDIBF(cond,n,i,r2,target) ic_put(iADDIBF(cond,n,i,r2,target));

/* BVB		-- Branch On Variable Bit */
#define BVB(cond,n,r1,target) ic_put(iBVB(cond,n,r1,target));

/* BB		-- Branch On Bit */
#define BB(cond,n,r1,p,target) ic_put(iBB(cond,n,r1,p,target));

/* ADD		-- Add */
#define ADD(cond,r1,r2,t) ic_put(iADD(cond,r1,r2,t));

/* ADDL		-- Add Logical */
#define ADDL(cond,r1,r2,t) ic_put(iADDL(cond,r1,r2,t));

/* ADDO		-- Add And Trap On Overflow */
#define ADDO(cond,r1,r2,t) ic_put(iADDO(cond,r1,r2,t));

/* ADDC		-- Add With Carry */
#define ADDC(cond,r1,r2,t) ic_put(iADDC(cond,r1,r2,t));

/* ADDCO	-- Add With Carry And Trap On Overflow */
#define ADDCO(cond,r1,r2,t) ic_put(iADDCO(cond,r1,r2,t));

/* SH1ADD	-- Shift One And Add */
#define SH1ADD(cond,r1,r2,t) ic_put(iSH1ADD(cond,r1,r2,t));

/* SH1ADDL	-- Shift One And Add Logical */
#define SH1ADDL(cond,r1,r2,t) ic_put(iSH1ADDL(cond,r1,r2,t));

/* SH1ADDO	-- Shift One, Add And Trap On Overflow */
#define SH1ADDO(cond,r1,r2,t) ic_put(iSH1ADDO(cond,r1,r2,t));

/* SH2ADD	-- Shift Two And Add */
#define SH2ADD(cond,r1,r2,t) ic_put(iSH2ADD(cond,r1,r2,t));

/* SH2ADDL	-- Shift Two And Add Logical */
#define SH2ADDL(cond,r1,r2,t) ic_put(iSH2ADDL(cond,r1,r2,t));

/* SH2ADDO	-- Shift Two, Add And Trap On Overflow */
#define SH2ADDO(cond,r1,r2,t) ic_put(iSH2ADDO(cond,r1,r2,t));

/* SH3ADD	-- Shift Three And Add */
#define SH3ADD(cond,r1,r2,t) ic_put(iSH3ADD(cond,r1,r2,t));

/* SH3ADDL	-- Shift Three And Add Logical */
#define SH3ADDL(cond,r1,r2,t) ic_put(iSH3ADDL(cond,r1,r2,t));

/* SH3ADDO	-- Shift Three, Add And Trap On Overflow */
#define SH2ADDO(cond,r1,r2,t) ic_put(iSH2ADDO(cond,r1,r2,t));

/* SUB		-- Subtract */
#define SUB(cond,r1,r2,t) ic_put(iSUB(cond,r1,r2,t));

/* SUBO		-- Subtract And Trap On Overflow */
#define SUBO(cond,r1,r2,t) ic_put(iSUBO(cond,r1,r2,t));

/* SUBB		-- Subtract With Borrow */
#define SUBB(cond,r1,r2,t) ic_put(iSUBB(cond,r1,r2,t));

/* SUBBO	-- Subtract With Borrow And Trap On Overflow */
#define SUBBO(cond,r1,r2,t) ic_put(iSUBBO(cond,r1,r2,t));

/* SUBT		-- Subtract And Trap On Condition */
#define SUBT(cond,r1,r2,t) ic_put(iSUBT(cond,r1,r2,t));

/* SUBTO	-- Subtract And Trap On Condition Or Overflow */
#define SUBTO(cond,r1,r2,t) ic_put(iSUBTO(cond,r1,r2,t));

/* DS		-- Divide Step */
#define DS(cond,r1,r2,t) ic_put(iDS(cond,r1,r2,t));

/* COMCLR	-- Compare And Clear */
#define COMCLR(cond,r1,r2,t) ic_put(iCOMCLR(cond,r1,r2,t));

/* OR		-- Inclusive Or */
#define OR(cond,r1,r2,t) ic_put(iOR(cond,r1,r2,t));

/* XOR		-- Exclusive Or */
#define XOR(cond,r1,r2,t) ic_put(iXOR(cond,r1,r2,t));

/* AND		-- And */
#define AND(cond,r1,r2,t) ic_put(iAND(cond,r1,r2,t));

/* ANDCM	-- And Complement */
#define ANDCM(cond,r1,r2,t) ic_put(iANDCM(cond,r1,r2,t));

/* UXOR		-- Unit XOR */
#define UXOR(cond,r1,r2,t) ic_put(iUXOR(cond,r1,r2,t));

/* UADDCM	-- Unit Add Complement */
#define UADD(cond,r1,r2,t) ic_put(iUADD(cond,r1,r2,t));

/* UADDCMT	-- Unit Add Complement And Trap On Condition */
#define UADDCMT(cond,r1,r2,t) ic_put(iUADDCMT(cond,r1,r2,t));

/* DCOR		-- Decimal Correct */
#define DCOR(cond,r,t) ic_put(iDCOR(cond,r,t));

/* IDCOR	-- Intermediate Decimal Correct */
#define IDCOR(cond,r,t) ic_put(iIDCOR(cond,r,t));

/* ADDI		-- Add To Immediate */
#define ADDI(cond,i,r,t) ic_put(iADDI(cond,i,r,t));

/* ADDIO	-- Add to Immediate And Trap On Overflow */
#define ADDIO(cond,i,r,t) ic_put(iADDIO(cond,i,r,t));

/* ADDIT	-- Add To Immediate And Trap On Condition */
#define ADDIT(cond,i,r,t) ic_put(iADDIT(cond,i,r,t));

/* ADDITO	-- Add To Immediate And Trap On Condition Or Overflow */
#define ADDITO(cond,i,r,t) ic_put(iADDITO(cond,i,r,t));

/* SUBI		-- Subtract From Immediate */
#define SUBI(cond,i,r,t) ic_put(iSUBI(cond,i,r,t));

/* SUBIO	-- Subtract From Immediate And Trap On Overflow */
#define SUBIO(cond,i,r,t) ic_put(iSUBIO(cond,i,r,t));

/* COMICLR	-- Compare Immediate And Clear */
#define COMICLR(cond,i,r,t) ic_put(iCOMICLR(cond,i,r,t));

/* VSHD		-- Variable Shift Double */
#define VSHD(cond,r1,r2,t) ic_put(iVSHD(cond,r1,r2,t));

/* SHD		-- Shift Double */
#define SHD(cond,r1,r2,p,t) ic_put(iSHD(cond,r1,r2,p,t));

/* VEXTRU	-- Variable Extract Unsigned */
#define VEXTRU(cond,r,len,t) ic_put(iVEXTRU(cond,r,len,t));

/* VEXTRS	-- Variable Extract Signed */
#define VEXTRS(cond,r,len,t) ic_put(iVEXTRS(cond,r,len,t));

/* EXTRU	-- Extract Unsigned */
#define EXTRU(cond,r,p,len,t) ic_put(iEXTRU(cond,r,p,len,t));

/* EXTRS	-- Extract Signed */
#define EXTRS(cond,r,p,len,t) ic_put(iEXTRS(cond,r,p,len,t));

/* VDEP		-- Variable Deposit */
#define VDEP(cond,r,len,t) ic_put(iVDEP(cond,r,len,t));

/* DEP		-- Deposit */
#define DEP(cond,r,p,len,t) ic_put(iDEP(cond,r,p,len,t));

/* VDEPI	-- Variable Deposit Immediate */
#define VDEPI(cond,i,len,t) ic_put(iVDEPI(cond,i,len,t));

/* DEPI		-- Deposit Immediate */
#define DEPI(cond,i,p,len,t) ic_put(iDEPI(cond,i,p,len,t));

/* ZVDEP	-- Zero And Variable Deposit */
#define ZVDEP(cond,r,len,t) ic_put(iZVDEP(cond,r,len,t));

/* ZDEP		-- Zero And Deposit */
#define ZDEP(cond,r,p,len,t) ic_put(iZDEP(cond,r,p,len,t));

/* ZVDEPI	-- Zero And Variable Deposit Immediate */
#define ZVDEPI(cond,i,len,t) ic_put(iZVDEPI(cond,i,len,t));

/* ZDEPI	-- Zero And Deposit Immediate */
#define ZDEPI(cond,i,p,len,t) ic_put(iZDEPI(cond,i,p,len,t));

/* BREAK	-- Break */
#define BREAK(im5,im13) ic_put(iBREAK(im5,im13));

/* RFI		-- Return From Interruption */
#define RFI ic_put(iRFI);

/* SSM		-- Set System Mask */
#define SSM(i,t) ic_put(iSSM(i,t));

/* RSM		-- Reset System Mask */
#define RSM(i,t) ic_put(iRSM(i,t));

/* MTSM		-- Move To System Mask */
#define MTSM(r) ic_put(iMTSM(r));

/* LDSID	-- Load Space Identifier */
#define LDSID(s,b,t) ic_put(iLDSID(s,b,t));

/* MTSP		-- Move To Space Register */
#define MTSP(r,sr) ic_put(iMTSP(r,sr));

/* MTCTL	-- Move To Control Register */
#define MTCTL(r,t) ic_put(iMTCTL(r,t));

/* MFSP		-- Move From Space Register */
#define MFSP(sr,t) ic_put(iMFSP(sr,t));

/* MFCTL	-- Move From Control Register */
#define MFCTL(r,t) ic_put(iMFCTL(r,t));

/* SYNC		-- Synchronize Caches */
#define SYNC ic_put(iSYNC);

/* PROBER	-- Probe Read Access */
#define PROBER(s,b,r,t) ic_put(iPROBER(s,b,r,t));

/* PROBERI	-- Probe Read Access Immediate */
#define PROBERI(s,b,i,t) ic_put(iPROBERI(s,b,i,t));

/* PROBEW	-- Probe Write Access */
#define PROBEW(s,b,r,t) ic_put(iPROBEW(s,b,r,t));

/* PROBEWI	-- Probe Write Access Immediate */
#define PROBEWI(s,b,i,t) ic_put(iPROBEWI(s,b,i,t));

/* LPA		-- Load Physical Address */
#define LPA(cmplt,x,s,b,t) ic_put(iLPA(cmplt,x,s,b,t));

/* LHA		-- Load Hash Address */
#define LHA(cmplt,x,s,b,t) ic_put(iLHA(cmplt,x,s,b,t));

/* PDTLB	-- Purge Date TLB */
#define PDTLB(cmplt,x,s,b) ic_put(iPDTLB(cmplt,x,s,b));

/* PITLB	-- Purge Instruction TLB */
#define PITLB(cmplt,x,sr,b) ic_put(iPITLB(cmplt,x,sr,b));

/* PDTLBE	-- Purge Data TLB Entry */
#define PDTLBE(cmplt,x,s,b) ic_put(iPDTLBE(cmplt,x,s,b));

/* PITLBE	-- Purge Instruction TLB Entry */
#define PITLBE(cmplt,x,sr,b) ic_put(iPITLBE(cmplt,x,sr,b));

/* IDTLBA	-- Insert Data TLB Address */
#define IDTLBA(r,s,b) ic_put(iIDTLBA(r,s,b));

/* IITLBA	-- Insert Instruction TLB Address */
#define IITLBA(r,sr,b) ic_put(iIITLBA(r,sr,b));

/* IDTLBP	-- Insert Data TLB Protection */
#define IDTLBP(r,s,b) ic_put(iIDTLBP(r,s,b));

/* IITLBP	-- Insert Instruction TLB Protection */
#define IITLBP(r,sr,b) ic_put(iIITLBP(r,sr,b));

/* PDC		-- Purge Data Cache */
#define PDC(cmplt,x,s,b) ic_put(iPDC(cmplt,x,s,b));

/* FDC		-- Flush Data Cache */
#define FDC(cmplt,x,s,b) ic_put(iFDC(cmplt,x,s,b));

/* FIC		-- Flush Instruction Cache */
#define FIC(cmplt,x,sr,b) ic_put(iFIC(cmplt,x,sr,b));

/* FDCE		-- Flush Data Cache Entry */
#define FDCE(cmplt,x,s,b) ic_put(iFDCE(cmplt,x,s,b));

/* FICE		-- Flush Instruction Cache Entry */
#define FICE(cmplt,x,sr,b) ic_put(iFICE(cmplt,x,sr,b));

/* DIAG		-- Diagnose */
#define DIAG(i) ic_put(iDIAG(i));

/*
 * Temporarily skipping Coprocessor and Floating Point instructions...
 */
