/*
 * codegen.h
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/5/93
 * Revision History:
 *
 * Description:
 *	This file contains some of the macros for code generation.  In
 *	particular it contains defines of the form i[A-Z0-9]* .  For example,
 *	the iADD macro has four parameters.  When used in code, this macro
 *	(with properly instantiated parameters) will "return" the (32 bit) word
 *	which is the HPPA's ADD instruction.
 *
 *	The macros in this file are seldom called directly.  Instead, they are
 *	mostly called from the macros which are defined in machinst.h which
 *	are responsible for putting the instructions down in a buffer.
 *
 */

/*
 * Macros for building other stuff internally
 *
 * See codegen.c for functions defining operations a bit too complicated
 * for macros.
 */

#define CG_reg16(r) ((r)<<16)
#define CG_reg21(r) ((r)<<21)
#define CG_spaceid(r) ((r)<<14)
#define CG_reg0(r) (r)

/*
 * Instruction Completers
 */

/*
 * ,S ,M ,SM for LDWX, LDHX, LDBX, LDWAX, and LDCWX
 */

#define cx_S	0x00002000
#define cx_M	0x00000020
#define cx_SM	0x00002020
#define cx_none 0x00000000

/*
 * ,MB ,MA for Load and Store Short
 */

#define cs_MB	0x00002020
#define cs_MA	0x00000020
#define cs_none	0x00000000

/*
 * ,B ,E ,B,M ,E,M for STBYS instruction
 */

#define cs_B	0x00000000
#define cs_E	0x00002000
#define cs_BM	0x00000020
#define cs_EM	0x00002020

/*
 * ,n (nullify) for branch and other instructions
 */

#define cn_n	0x2	/* nullify next instruction */
#define cn_	0	/* no nullification */

/*
 * Logical Instruction Completers
 */

#define cl_	0x0000	/* never */
#define cl_EQ	0x2000	/* all bits are 0 */
#define cl_LT	0x4000	/* leftmost bit is 1 */
#define cl_LE	0x6000	/* leftmost bit is 1 or all bits are 0 */
#define cl_OD	0xe000	/* rightmost bit is 1 */
#define cl_TR	0x1000	/* always */
#define cl_NE	0x3000	/* some bits are 1 */
#define cl_GE	0x5000	/* leftmost bit is 0 */
#define cl_GT	0x7000	/* leftmost bit is 0 some bits are 1 */
#define cl_EV	0xf000	/* rightmost bit is 0 */



/* LDW		-- Load Word */
#define iLDW(d,s,b,t) \
	(0x48000000 | CG_reg21(b) | CG_reg16(t) | CG_spaceid(s) | CG_im14(d))

/* LDH		-- Load Halfword */
#define iLDH(d,s,b,t) \
	(0x44000000 | CG_reg21(b) | CG_reg16(t) | CG_spaceid(s) | CG_im14(d))

/* LDB		-- Load Byte */
#define iLDB(d,s,b,t) \
	(0x40000000 | CG_reg21(b) | CG_reg16(t) | CG_spaceid(s) | CG_im14(d))

/* STW		-- Store Word */
#define iSTW(r,d,s,b) \
	(0x68000000 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im14(d))

/* STH		-- Store Halfword */
#define iSTH(r,d,s,b) \
	(0x64000000 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im14(d))

/* STB		-- Store Byte */
#define iSTB(r,d,s,b) \
	(0x60000000 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im14(d))

/* LDWM		-- Load Word And Modify */
#define iLDWM(d,s,b,t) \
	(0x4c000000 | CG_reg21(b) | CG_reg16(t) | CG_spaceid(s) | CG_im14(d))

/* STWM		-- Store Word And Modify */
#define iSTWM(r,d,s,b) \
	(0x6c000000 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im14(d))

/* LDWX		-- Load Word Indexed */
#define iLDWX(cmplt,x,s,b,t) \
	(0x0c000080 | (cmplt) | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | CG_reg0(t))
/* LDHX		-- Load Halfword Indexed */
#define iLDHX(cmplt,x,s,b,t) \
	(0x0c000040 | (cmplt) | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | CG_reg0(t))

/* LDBX		-- Load Byte Indexed */
#define iLDBX(cmplt,x,s,b,t) \
	(0x0c000000 | (cmplt) | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | CG_reg0(t))

/* LDWAX	-- Load Word Absolute Indexed */
#define iLDWAX(cmplt,x,b,t) \
	(0x0c000180 | (cmplt) | CG_reg21(b) | CG_reg16(x) | CG_reg0(t))

/* LDCWX	-- Load And Clear Word Indexed */
#define iLDCWX(cmplt,x,s,b,t) \
	(0x0c0001c0 | (cmplt) | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | CG_reg0(t))

/* LDWS		-- Load Word Short */
#define iLDWS(cmplt,d,s,b,t) \
	(0x0c001080 | (cmplt) | CG_reg21(b) | (CG_im5(d)<<16) | CG_spaceid(s) | CG_reg0(t))


/* LDHS		-- Load Halfword Short */
#define iLDHS(cmplt,d,s,b,t) \
	(0x0c001040 | (cmplt) | CG_reg21(b) | (CG_im5(d)<<16) | CG_spaceid(s) | CG_reg0(t))

/* LDBS		-- Load Byte Short */
#define iLDBS(cmplt,d,s,b,t) \
	(0x0c001000 | (cmplt) | CG_reg21(b) | (CG_im5(d)<<16) | CG_spaceid(s) | CG_reg0(t))

/* LDWAS	-- Load Word Absolute Short */
#define iLDWAS(cmplt,d,b,t) \
	(0x0c001180 | (cmplt) | CG_reg21(b) | (CG_im5(d)<<16) | CG_reg0(t))

/* LDCWS	-- Load And Clear Word Short */
#define iLDCWS(cmplt,d,s,b,t) \
	(0x0c0011c0 | (cmplt) | CG_reg21(b) | (CG_im5(d)<<16) | CG_spaceid(s) | CG_reg0(t))

/* STWS		-- Store Word Short */
#define iSTWS(cmplt,r,d,s,b) \
	(0x0c001280 | (cmplt) | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im5(d))

/* STHS		-- Store Halfword Short */
#define iSTHS(cmplt,r,d,s,b) \
	(0x0c001240 | (cmplt) | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im5(d))

/* STBS		-- Store Byte Short */
#define iSTBS(cmplt,r,d,s,b) \
	(0x0c001200 | (cmplt) | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im5(d))

/* STWAS	-- Store Word Absolute Short */
#define iSTWAS(cmplt,r,d,b) \
	(0x0c001380 | (cmplt) | CG_reg21(b) | CG_reg16(r) | CG_im5(d))

/* STBYS	-- Store Bytes Short */
#define iSTBYS(cmplt,r,d,s,b) \
	(0x0c001300 | (cmplt) | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_im5(d))

/* LDO		-- Load Offset */
#define iLDO(d,b,t) \
	(0x34000000 | CG_reg21(b) | CG_reg16(t) | CG_im14(d))

/* LDIL		-- Load Immediate Left */
#define iLDIL(i,t) \
	(0x20000000 | CG_reg21(t) | CG_im21(i))

/* ADDIL	-- Add Immediate Left */
#define iADDIL(i,r) \
	(0x28000000 | CG_reg21(r) | CG_im21(i))

/* BL		-- Branch And Link */
#define iBL(n,target,t) \
	(0xe8000000 | CG_reg21(t) | CG_im17(target) | (n))

/* GATE		-- Gateway */
#define iGATE(n,target,t) \
	(0xe8002000 | CG_reg21(t) | CG_im17(target) | (n))

/* BLR		-- Branch And Link Register */
#define iBLR(n,x,t) \
	(0xe8004000 | CG_reg21(t) | CG_reg16(x) | (n))

/* BV		-- Branch Vectored */
#define iBV(n,x,b) \
	(0xe800c000 | CG_reg21(b) | CG_reg16(x) | (n))

/* BE		-- Branch External */
#define iBE(n,wd,sr,b) \
	(0xe0000000 | CG_reg21(b) | CG_im17(wd) | CG_spacereg(sr) | (n))

/* BLE		-- Branch And Link External */
#define iBLE(n,wd,sr,b) \
	(0xe4000000 | CG_reg21(b) | CG_im17(wd) | CG_spacereg(sr) | (n))

/* MOVB		-- Move And Branch */
#define iMOVB(cond,n,r1,r2,target) \
	(0xc8000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* MOVIB	-- Move Immediate And Branch */
#define iMOVIB(cond,n,i,r2,target) \
	(0xcc000000 | CG_reg21(r2) | (CG_im5(i)<<16) | (cond) | CG_im12(target) | (n))

/* COMBT	-- Compare And Branch If True */
#define iCOMBT(cond,n,r1,r2,target) \
	(0x80000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* COMBF	-- Compare And Branch If False */
#define iCOMBF(cond,n,r1,r2,target) \
	(0x88000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* COMIBT	-- Compare Immediate And Branch If True */
#define iCOMIBT(cond,n,i,r2,target) \
	(0x84000000 | CG_reg21(r2) | (CG_im5(i)<<16) | (cond) | CG_im12(target) | (n))

/* COMIBF	-- Compare Immediate And Branch If False */
#define iCOMIBF(cond,n,i,r2,target) \
	(0x8c000000 | CG_reg21(r2) | (CG_im5(i)<<16) | (cond) | CG_im12(target) | (n))

/* ADDBT	-- Add And Branch If True */
#define iADDBT(cond,n,r1,r2,target) \
	(0xa0000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* ADDBF	-- Add And Branch If False */
#define iADDBF(cond,n,r1,r2,target) \
	(0xa8000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* ADDIBT	-- Add Immediate And Branch If True */
#define iADDIBT(cond,n,i,r2,target) \
	(0xa4000000 | CG_reg21(r2) | (CG_im5(i)<<16) | (cond) | CG_im12(target) | (n))

/* ADDIBF	-- Add Immediate And Branch If False */
#define iADDIBF(cond,n,i,r2,target) \
	(0xac000000 | CG_reg21(r2) | (CG_im5(i)<<16) | (cond) | CG_im12(target) | (n))

/* BVB		-- Branch On Variable Bit */
#define iBVB(cond,n,r1,target) \
	(0xc0000000 | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* BB		-- Branch On Bit */
#define iBB(cond,n,r1,p,target) \
	(0xc4000000 | ((p)<<21) | CG_reg16(r1) | (cond) | CG_im12(target) | (n))

/* ADD		-- Add */
#define iADD(cond,r1,r2,t) \
	(0x08000600 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* ADDL		-- Add Logical */
#define iADDL(cond,r1,r2,t) \
	(0x08000a00 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* ADDO		-- Add And Trap On Overflow */
#define iADDO(cond,r1,r2,t) \
	(0x08000e00 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* ADDC		-- Add With Carry */
#define iADDC(cond,r1,r2,t) \
	(0x08000700 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* ADDCO	-- Add With Carry And Trap On Overflow */
#define iADDCO(cond,r1,r2,t) \
	(0x08000f00 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH1ADD	-- Shift One And Add */
#define iSH1ADD(cond,r1,r2,t) \
	(0x08000640 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH1ADDL	-- Shift One And Add Logical */
#define iSH1ADDL(cond,r1,r2,t) \
	(0x08000a40 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH1ADDO	-- Shift One, Add And Trap On Overflow */
#define iSH1ADDO(cond,r1,r2,t) \
	(0x08000e40 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH2ADD	-- Shift Two And Add */
#define iSH2ADD(cond,r1,r2,t) \
	(0x08000680 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH2ADDL	-- Shift Two And Add Logical */
#define iSH2ADDL(cond,r1,r2,t) \
	(0x08000a80 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH2ADDO	-- Shift Two, Add And Trap On Overflow */
#define iSH2ADDO(cond,r1,r2,t) \
	(0x08000e80 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH3ADD	-- Shift Three And Add */
#define iSH3ADD(cond,r1,r2,t) \
	(0x080006c0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH3ADDL	-- Shift Three And Add Logical */
#define iSH3ADDL(cond,r1,r2,t) \
	(0x08000ac0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SH3ADDO	-- Shift Three, Add And Trap On Overflow */
#define iSH2ADDO(cond,r1,r2,t) \
	(0x08000ec0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUB		-- Subtract */
#define iSUB(cond,r1,r2,t) \
	(0x08000400 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUBO		-- Subtract And Trap On Overflow */
#define iSUBO(cond,r1,r2,t) \
	(0x08000c00 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUBB		-- Subtract With Borrow */
#define iSUBB(cond,r1,r2,t) \
	(0x08000500 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUBBO	-- Subtract With Borrow And Trap On Overflow */
#define iSUBBO(cond,r1,r2,t) \
	(0x08000d00 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUBT		-- Subtract And Trap On Condition */
#define iSUBT(cond,r1,r2,t) \
	(0x080004c0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SUBTO	-- Subtract And Trap On Condition Or Overflow */
#define iSUBTO(cond,r1,r2,t) \
	(0x08000cc0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* DS		-- Divide Step */
#define iDS(cond,r1,r2,t) \
	(0x08000440 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* COMCLR	-- Compare And Clear */
#define iCOMCLR(cond,r1,r2,t) \
	(0x08000880 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* OR		-- Inclusive Or */
#define iOR(cond,r1,r2,t) \
	(0x08000240 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* XOR		-- Exclusive Or */
#define iXOR(cond,r1,r2,t) \
	(0x08000280 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* AND		-- And */
#define iAND(cond,r1,r2,t) \
	(0x08000200 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* ANDCM	-- And Complement */
#define iANDCM(cond,r1,r2,t) \
	(0x08000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* UXOR		-- Unit XOR */
#define iUXOR(cond,r1,r2,t) \
	(0x08000380 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* UADDCM	-- Unit Add Complement */
#define iUADD(cond,r1,r2,t) \
	(0x08000980 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* UADDCMT	-- Unit Add Complement And Trap On Condition */
#define iUADDCMT(cond,r1,r2,t) \
	(0x080009c0 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* DCOR		-- Decimal Correct */
#define iDCOR(cond,r,t) \
	(0x08000b80 | CG_reg21(r) | (cond) | CG_reg0(t))

/* IDCOR	-- Intermediate Decimal Correct */
#define iIDCOR(cond,r,t) \
	(0x08000bc0 | CG_reg21(r) | (cond) | CG_reg0(t))

/* ADDI		-- Add To Immediate */
#define iADDI(cond,i,r,t) \
	(0xb4000000 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* ADDIO	-- Add to Immediate And Trap On Overflow */
#define iADDIO(cond,i,r,t) \
	(0xb4000800 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* ADDIT	-- Add To Immediate And Trap On Condition */
#define iADDIT(cond,i,r,t) \
	(0xb0000000 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* ADDITO	-- Add To Immediate And Trap On Condition Or Overflow */
#define iADDITO(cond,i,r,t) \
	(0xb0000800 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* SUBI		-- Subtract From Immediate */
#define iSUBI(cond,i,r,t) \
	(0x94000000 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* SUBIO	-- Subtract From Immediate And Trap On Overflow */
#define iSUBIO(cond,i,r,t) \
	(0x94000800 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* COMICLR	-- Compare Immediate And Clear */
#define iCOMICLR(cond,i,r,t) \
	(0x90000000 | CG_reg21(r) | CG_reg16(t) | (cond) | CG_im11(i))

/* VSHD		-- Variable Shift Double */
#define iVSHD(cond,r1,r2,t) \
	(0xd0000000 | CG_reg21(r2) | CG_reg16(r1) | (cond) | CG_reg0(t))

/* SHD		-- Shift Double */
#define iSHD(cond,r1,r2,p,t) \
	(0xd0000800 | CG_reg21(r2) | CG_reg16(r1) | (cond) | ((31-p)<<5) | CG_reg0(t))

/* VEXTRU	-- Variable Extract Unsigned */
#define iVEXTRU(cond,r,len,t) \
	(0xd0001000 | CG_reg21(r) | CG_reg16(t) | (cond) | (32-(len)))

/* VEXTRS	-- Variable Extract Signed */
#define iVEXTRS(cond,r,len,t) \
	(0xd0001400 | CG_reg21(r) | CG_reg16(t) | (cond) | (32-(len)))

/* EXTRU	-- Extract Unsigned */
#define iEXTRU(cond,r,p,len,t) \
	(0xd0001800 | CG_reg21(r) | CG_reg16(t) | (cond) | ((p)<<5) | (32-(len)))

/* EXTRS	-- Extract Signed */
#define iEXTRS(cond,r,p,len,t) \
	(0xd0001c00 | CG_reg21(r) | CG_reg16(t) | (cond) | ((p)<<5) | (32-(len)))

/* VDEP		-- Variable Deposit */
#define iVDEP(cond,r,len,t) \
	(0xd4000400 | CG_reg21(t) | CG_reg16(r) | (cond) | (32-(len)))

/* DEP		-- Deposit */
#define iDEP(cond,r,p,len,t) \
	(0xd4000c00 | CG_reg21(t) | CG_reg16(r) | (cond) | ((31-(p))<<5) | (32-(len)))

/* VDEPI	-- Variable Deposit Immediate */
#define iVDEPI(cond,i,len,t) \
	(0xd4001400 | CG_reg21(t) | (CG_im5(i)<<16) | (cond) | (32-(len)))

/* DEPI		-- Deposit Immediate */
#define iDEPI(cond,i,p,len,t) \
	(0xd4001c00 | CG_reg21(t) | (CG_im5(i)<<16) | (cond) | ((31-(p))<<5) | (32-(len)))

/* ZVDEP	-- Zero And Variable Deposit */
#define iZVDEP(cond,r,len,t) \
	(0xd4000000 | CG_reg21(t) | CG_reg16(r) | (cond) | (32-(len)))

/* ZDEP		-- Zero And Deposit */
#define iZDEP(cond,r,p,len,t) \
	(0xd4000800 | CG_reg21(t) | CG_reg16(r) | (cond) | ((31-(p))<<5) | (32-(len)))

/* ZVDEPI	-- Zero And Variable Deposit Immediate */
#define iZVDEPI(cond,i,len,t) \
	(0xd4001000 | CG_reg21(t) | (CG_im5(i)<<16) | (cond) | (32-(len)))

/* ZDEPI	-- Zero And Deposit Immediate */
#define iZDEPI(cond,i,p,len,t) \
	(0xd4001800 | CG_reg21(t) | (CG_im5(i)<<16) | (cond) | ((31-(p))<<5) | (32-(len)))

/* BREAK	-- Break */
#define iBREAK(im5,im13) \
	(0x00000000 | ((im13)<<13) | (im5))

/* RFI		-- Return From Interruption */
#define iRFI \
	(0x00000c00)

/* SSM		-- Set System Mask */
#define iSSM(i,t) \
	(0x00000d60 | (CG_im5(i)<<16) | CG_reg0(t))

/* RSM		-- Reset System Mask */
#define iRSM(i,t) \
	(0x00000e60 | (CG_im5(i)<<16) | CG_reg0(t))

/* MTSM		-- Move To System Mask */
#define iMTSM(r) \
	(0x00001860 | CG_reg16(r))

/* LDSID	-- Load Space Identifier */
#define iLDSID(s,b,t) \
	(0x000010a0 | CG_reg21(b) | CG_spaceid(s) | CG_reg0(t))

/* MTSP		-- Move To Space Register */
#define iMTSP(r,sr) \
	(0x00001820 | CG_reg16(r) | CG_spacereg(sr))

/* MTCTL	-- Move To Control Register */
#define iMTCTL(r,t) \
	(0x00001840 | CG_reg21(t) | CG_reg16(r))

/* MFSP		-- Move From Space Register */
#define iMFSP(sr,t) \
	(0x000004a0 | CG_spacereg(sr) | CG_reg0(t))

/* MFCTL	-- Move From Control Register */
#define iMFCTL(r,t) \
	(0x000008a0 | CG_reg21(r) | CG_reg0(t))

/* SYNC		-- Synchronize Caches */
#define iSYNC \
	(0x00000400)

/* PROBER	-- Probe Read Access */
#define iPROBER(s,b,r,t) \
	(0x04001180 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_reg0(t))

/* PROBERI	-- Probe Read Access Immediate */
#define iPROBERI(s,b,i,t) \
	(0x04003180 | CG_reg21(b) | ((i)<<16) | CG_spaceid(s) | CG_reg0(t))

/* PROBEW	-- Probe Write Access */
#define iPROBEW(s,b,r,t) \
	(0x040011c0 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s) | CG_reg0(t))

/* PROBEWI	-- Probe Write Access Immediate */
#define iPROBEWI(s,b,i,t) \
	(0x040031c0 | CG_reg21(b) | ((i)<<16) | CG_spaceid(s) | CG_reg0(t))

/* LPA		-- Load Physical Address */
#define iLPA(cmplt,x,s,b,t) \
	(0x04001340 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt) | CG_reg0(t))

/* LHA		-- Load Hash Address */
#define iLHA(cmplt,x,s,b,t) \
	(0x04001300 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt) | CG_reg0(t))

/* PDTLB	-- Purge Date TLB */
#define iPDTLB(cmplt,x,s,b) \
	(0x04001200 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt))

/* PITLB	-- Purge Instruction TLB */
#define iPITLB(cmplt,x,sr,b) \
	(0x04000200 | CG_reg21(b) | CG_reg16(x) | CG_spacereg(sr) | (cmplt))

/* PDTLBE	-- Purge Data TLB Entry */
#define iPDTLBE(cmplt,x,s,b) \
	(0x04001240 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt))

/* PITLBE	-- Purge Instruction TLB Entry */
#define iPITLBE(cmplt,x,sr,b) \
	(0x04000240 | CG_reg21(b) | CG_reg16(x) | CG_spacereg(sr) | (cmplt))

/* IDTLBA	-- Insert Data TLB Address */
#define iIDTLBA(r,s,b) \
	(0x04001040 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s))

/* IITLBA	-- Insert Instruction TLB Address */
#define iIITLBA(r,sr,b) \
	(0x04000040 | CG_reg21(b) | CG_reg16(r) | CG_spacereg(sr))

/* IDTLBP	-- Insert Data TLB Protection */
#define iIDTLBP(r,s,b) \
	(0x04001000 | CG_reg21(b) | CG_reg16(r) | CG_spaceid(s))

/* IITLBP	-- Insert Instruction TLB Protection */
#define iIITLBP(r,sr,b) \
	(0x04000000 | CG_reg21(b) | CG_reg16(r) | CG_spacereg(sr))

/* PDC		-- Purge Data Cache */
#define iPDC(cmplt,x,s,b) \
	(0x04001380 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt))

/* FDC		-- Flush Data Cache */
#define iFDC(cmplt,x,s,b) \
	(0x04001280 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt))

/* FIC		-- Flush Instruction Cache */
#define iFIC(cmplt,x,sr,b) \
	(0x04000280 | CG_reg21(b) | CG_reg16(x) | CG_spacereg(sr) | (cmplt))

/* FDCE		-- Flush Data Cache Entry */
#define iFDCE(cmplt,x,s,b) \
	(0x040012c0 | CG_reg21(b) | CG_reg16(x) | CG_spaceid(s) | (cmplt))

/* FICE		-- Flush Instruction Cache Entry */
#define iFICE(cmplt,x,sr,b) \
	(0x040002c0 | CG_reg21(b) | CG_reg16(x) | CG_spacereg(sr) | (cmplt))

/* DIAG		-- Diagnose */
#define iDIAG(i) \
	(0x14000000 | (i))

/*
 * Temporarily skipping Coprocessor and Floating Point instructions...
 */
