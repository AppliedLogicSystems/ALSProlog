/*
 * codegen.h
 *	Copyright (c) 1990-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin Buettner
 * Creation: 3/1/90
 * Revision History:
 */

/* Macros for building other stuff internally */

#define CG_RD(r) ((r)<<25)
#define CG_RS1(r) ((r)<<14)
#define CG_ASI(asi) ((asi)<<5)
#define CG_D22(disp) (((int) (disp)) & 0x3fffff)


/*
 * In the following macros, we expect the following
 *
 *	s1	-- source register, an integer in the range [0,31].
 *	s2	-- source argument, either a register number or a 13 bit
 *		   integer with bit 13 set to indicate that this argument
 *		   is an integer.  This actually gives us 14 bits.  Having
 *		   bit 13 set will force the "i" field to be one in the
 *		   appropriate instructions.
 *	d	-- destination register, an integer in the range [0,31].
 *	asi	-- address space identifier, an integer in the range [0,255].
 *	c22	-- 22 bit constant for the SETHI instruction
 *	disp22	-- 22 bit displacement for the branch instructions
 */

/*
 * B.1 Load Integer Instructions
 */

/* LDSB		-- Load Signed Byte */
#define iLDSB(s1,s2,d) (0xc0480000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDSBA	-- Load Signed Byte from Alternate space */
#define iLDSBA(s1,s2,asi,d) (0xc0c80000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* LDSH		-- Load Signed Halfword */
#define iLDSH(s1,s2,d) (0xc0500000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDSHA	-- Load Signed Halfword from Alternate space */
#define iLDSHA(s1,s2,asi,d) (0xc0d00000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* LDUB		-- Load Unsigned Byte */
#define iLDUB(s1,s2,d) (0xc0080000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDUBA	-- Load Unsigned Byte from Alternate space */
#define iLDUBA(s1,s2,asi,d) (0xc0880000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* LDUH		-- Load Unsigned Halfword */
#define iLDUH(s1,s2,d) (0xc0100000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDUHA	-- Load Unsigned Halfword from Alternate space */
#define iLDUHA(s1,s2,asi,d) (0xc0900000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* LD		-- Load Word */
#define iLD(s1,s2,d) (0xc0000000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDA		-- Load Word from Alternate space */
#define iLDA(s1,s2,asi,d) (0xc0800000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* LDD		-- Load Doubleword */
#define iLDD(s1,s2,d) (0xc0180000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDDA		-- Load Doubleword from Alternate space */
#define iLDDA(s1,s2,asi,d) (0xc0980000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/*
 * B.2 Load Floating-point Instructions
 *
 * We're skipping these for right now as I don't anticipate needing them
 * in the near future (if at all).
 */

/*
 * B.3 Load Coprocesor Instructions
 *
 * We will probably never need these so I'm skipping them too.
 */

/*
 * B.4 Store Integer Instructions
 */

/* STB		-- Store Byte */
#define iSTB(d,s1,s2) (0xc0280000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* STBA		-- Store Byte into Alternate space */
#define iSTBA(d,s1,s2,asi) (0xc0a80000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* STH		-- Store Halfword */
#define iSTH(d,s1,s2) (0xc0300000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* STHA		-- Store Halfword into Alternate space */
#define iSTHA(d,s1,s2,asi) (0xc0b00000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* ST		-- Store Word */
#define iST(d,s1,s2) (0xc0200000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* STA		-- Store Word into Alternate space */
#define iSTA(d,s1,s2,asi) (0xc0a00000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))

/* STD		-- Store Doubleword */
#define iSTD(d,s1,s2) (0xc0380000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* STDA		-- Store Doubleword into Alternate space */
#define iSTDA(d,s1,s2,asi) (0xc0b80000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))


/*
 * B.5 Store Floating-point Instructions
 *
 * We're skipping these too.
 */


/*
 * B.6 Store Coprocessor Instructions
 *
 * ...and these.
 */

/*
 * B.7 Atomic Load-Store Unsigned Byte Instructions
 */

/* LDSTUB	-- Atomic Load-Store Unsigned Byte */
#define iLDSTUB(s1,s2,d) (0xc0680000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* LDSTUBA	-- Atomic Load-Store Unsigned Byte into Alternate space */
#define iLDSTUBA(s1,s2,d,asi) (0xc0e80000|CG_RD(d)|CG_RS1(s1)|(CG_ASI(asi)|(s2))


/*
 * B.8		-- Swap r Register with Memory
 */

/* SWAP		-- SWAP r register with memory */
#define iSWAP(s1,s2,d) (0xc0780000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SWAPA	-- SWAP r register with Alternate space memory */
#define iSWAPA(s1,s2,d,asi) (0xc0f80000|CG_RD(d)|CG_RS1(s1)|CG_ASI(asi)|(s2))


/*
 * B.9		Add Instructions
 */

/* ADD		-- Add */
#define iADD(s1,s2,d) (0x80000000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ADDcc	-- Add and modify icc */
#define iADDcc(s1,s2,d) (0x80800000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ADDX		-- Add with Carry */
#define iADDX(s1,s2,d) (0x80400000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ADDXcc	-- Add with Carry and modify icc */
#define iADDXcc(s1,s2,d) (0x80c00000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.10		Tagged Add Instructions
 *
 * Although we will probably never use these, I will put them in anyway.
 */

/* TADDcc	-- Tagged Add and modify icc */
#define iTADDcc(s1,s2,d) (0x81000000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* TADDccTV	-- Tagged Add, modify icc and Trap on Overflow */
#define iTADDccTV(s1,s2,d) (0x81100000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.11		Subtract Instructions
 */

/* SUB		-- Subtract */
#define iSUB(s1,s2,d) (0x80200000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SUBcc	-- Subtract and modify icc */
#define iSUBcc(s1,s2,d) (0x80a00000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SUBX		-- Subtract with Carry */
#define iSUBX(s1,s2,d) (0x80600000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SUBXcc	-- Subtract with Carry and modify icc */
#define iSUBXcc(s1,s2,d) (0x80e00000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.12		Tagged Subtract Instructions
 */

/* TSUBcc	-- Tagged Subtract and modify icc */
#define iTSUBcc(s1,s2,d) (0x81080000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* TSUBccTV	-- Tagged Subtract, modify icc and Trap on Overflow */
#define iTSUBccTV(s1,s2,d) (0x81180000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.13		Multiply Step Instruction
 */

/* MULScc	-- Multiply Step and modify icc */
#define iMULScc(s1,s2,d) (0x81200000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.14		Logical Instructions
 */

/* AND		-- And */
#define iAND(s1,s2,d) (0x80080000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ANDcc	-- And and modify icc */
#define iANDcc(s1,s2,d) (0x80880000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ANDN		-- And Not */
#define iANDN(s1,s2,d) (0x80280000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ANDNcc	-- And Not and modify icc */
#define iANDNcc(s1,s2,d) (0x80a80000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* OR		-- Inclusive Or */
#define iOR(s1,s2,d) (0x80100000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ORcc		-- Inclusive Or and modify icc */
#define iORcc(s1,s2,d) (0x80900000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ORN		-- Inclusive Or Not */
#define iORN(s1,s2,d) (0x80300000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* ORNcc	-- Inclusive Or Not and modify icc */
#define iORNcc(s1,s2,d) (0x80b00000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* XOR		-- Exclusive Or */
#define iXOR(s1,s2,d) (0x80180000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* XORcc	-- Exclusive Or and modify icc */
#define iXORcc(s1,s2,d) (0x80980000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* XNOR		-- Exclusive Nor */
#define iXNOR(s1,s2,d) (0x80380000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* XNORcc	-- Exclusive Nor and modify icc */
#define iXNORcc(s1,s2,d) (0x80b80000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.15		Shift Instructions
 */

/* SLL		-- Shift Left Logical */
#define iSLL(s1,s2,d) (0x81280000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SRL		-- Shift Right Logical */
#define iSRL(s1,s2,d) (0x81300000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* SRA		-- Shift Right Arithmetic */
#define iSRA(s1,s2,d) (0x81380000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.16		SETHI Instruction
 */

/* SETHI	-- Set High */
#define iSETHI(c22,d) (0x01000000 | CG_RD(d) | ((c22) & 0x3fffff))


/*
 * B.17		SAVE and RESTORE instructions
 */

/* SAVE		-- Save caller's window */
#define iSAVE(s1,s2,d) (0x81e00000 | CG_RD(d) | CG_RS1(s1) | (s2))

/* RESTORE	-- Restore caller's window */
#define iRESTORE(s1,s2,d) (0x81e80000 | CG_RD(d) | CG_RS1(s1) | (s2))


/*
 * B.18		Branch on Integer Condition Instructions
 *
 *	The _a instructions represent the "annulled" version.
 */

/* BN		-- Branch Never */
#define iBN(disp22)	(0x00800000 | CG_D22(disp22))
#define iBN_a(disp22)	(0x20800000 | CG_D22(disp22))

/* BE		-- Branch on Equal */
#define iBE(disp22)	(0x02800000 | CG_D22(disp22))
#define iBE_a(disp22)	(0x22800000 | CG_D22(disp22))

/* BLE		-- Branch on Less or Equal */
#define iBLE(disp22)	(0x04800000 | CG_D22(disp22))
#define iBLE_a(disp22)	(0x24800000 | CG_D22(disp22))

/* BL		-- Branch on Less */
#define iBL(disp22)	(0x06800000 | CG_D22(disp22))
#define iBL_a(disp22)	(0x26800000 | CG_D22(disp22))

/* BLEU		-- Branch on Less or Equal Unsigned */
#define iBLEU(disp22)	(0x08800000 | CG_D22(disp22))
#define iBLEU_a(disp22)	(0x28800000 | CG_D22(disp22))

/* BCS		-- Branch on Carry Set (Less than, unsigned) */
#define iBCS(disp22)	(0x0a800000 | CG_D22(disp22))
#define iBCS_a(disp22)	(0x2a800000 | CG_D22(disp22))

/* BNEG		-- Branch on Negative */
#define iBNEG(disp22)	(0x0c800000 | CG_D22(disp22))
#define iBNEG_a(disp22)	(0x2c800000 | CG_D22(disp22))

/* BVS		-- Branch on Overflow Set */
#define iBVS(disp22)	(0x0e800000 | CG_D22(disp22))
#define iBVS_a(disp22)	(0x2e800000 | CG_D22(disp22))

/* BA		-- Branch Always */
#define iBA(disp22)	(0x10800000 | CG_D22(disp22))
#define iBA_a(disp22)	(0x30800000 | CG_D22(disp22))

/* BNE		-- Branch on Not Equal */
#define iBNE(disp22)	(0x12800000 | CG_D22(disp22))
#define iBNE_a(disp22)	(0x32800000 | CG_D22(disp22))

/* BG		-- Branch on Greater */
#define iBG(disp22)	(0x14800000 | CG_D22(disp22))
#define iBG_a(disp22)	(0x34800000 | CG_D22(disp22))

/* BGE		-- Branch on Greater or Equal */
#define iBGE(disp22)	(0x16800000 | CG_D22(disp22))
#define iBGE_a(disp22)	(0x36800000 | CG_D22(disp22))

/* BGU		-- Branch on Greater Unsigned */
#define iBGU(disp22)	(0x18800000 | CG_D22(disp22))
#define iBGU_a(disp22)	(0x38800000 | CG_D22(disp22))

/* BCC		-- Branch on Carry Clear (Greater than or Equal, Unsigned) */
#define iBCC(disp22)	(0x1a800000 | CG_D22(disp22))
#define iBCC_a(disp22)	(0x3a800000 | CG_D22(disp22))

/* BPOS		-- Branch on Positive */
#define iBPOS(disp22)	(0x1c800000 | CG_D22(disp22))
#define iBPOS_a(disp22)	(0x3c800000 | CG_D22(disp22))

/* BVC		-- Branch of Overflow Clear */
#define iBVC(disp22)	(0x1e800000 | CG_D22(disp22))
#define iBVC_a(disp22)	(0x3e800000 | CG_D22(disp22))


/*
 * B.19		Floating-point Branch on Condition Instructions
 *
 * We are skipping these.
 */


/*
 * B.20		Coprocessor Branch on Condition Instructions
 *
 * We are skipping these too.
 */


/*
 * B.21		CALL instruction
 */

/* CALL		-- Call */
#define iCALL(disp) (0x40000000 | (((int) (disp)) & 0x3fffffff))


/*
 * B.22		Jump and Link Instruction
 */

/* JMPL		-- Jump and Link */
#define iJMPL(s1,s2,d) (0x81c00000 | CG_RS1(s1) | CG_RD(d) | (s2))


/*
 * B.23		Return from Trap Instruction
 *
 * I'm putting this in even though I doubt that it will ever get used.
 */

/* RETT		-- Return from Trap */
#define iRETT(s1,s2) (0x81c80000 | CG_RS1(s1) | (s2))


/*
 * B.24		Trap on Integer Condition Instruction
 */

/* TN		-- Trap Never */
#define iTN(s1,s2)	(0x81d00000 | CG_RS1(s1) | (s2))

/* TE		-- Trap on Equal */
#define iTE(s1,s2)	(0x83d00000 | CG_RS1(s1) | (s2))

/* TLE		-- Trap on Less or Equal */
#define iTLE(s1,s2)	(0x85d00000 | CG_RS1(s1) | (s2))

/* TL		-- Trap on Less */
#define iTL(s1,s2)	(0x87d00000 | CG_RS1(s1) | (s2))

/* TLEU		-- Trap on Less or Equal Unsigned */
#define iTLEU(s1,s2)	(0x89d00000 | CG_RS1(s1) | (s2))

/* TCS		-- Trap on Carry Set (Less than, unsigned) */
#define iTCS(s1,s2)	(0x8bd00000 | CG_RS1(s1) | (s2))

/* TNEG		-- Trap on Negative */
#define iTNEG(s1,s2)	(0x8dd00000 | CG_RS1(s1) | (s2))

/* TVS		-- Trap on Overflow Set */
#define iTVS(s1,s2)	(0x8fd00000 | CG_RS1(s1) | (s2))

/* TA		-- Trap Always */
#define iTA(s1,s2)	(0x91d00000 | CG_RS1(s1) | (s2))

/* TNE		-- Trap on Not Equal */
#define iTNE(s1,s2)	(0x93d00000 | CG_RS1(s1) | (s2))

/* TG		-- Trap on Greater */
#define iTG(s1,s2)	(0x95d00000 | CG_RS1(s1) | (s2))

/* TGE		-- Trap on Greater or Equal */
#define iTGE(s1,s2)	(0x97d00000 | CG_RS1(s1) | (s2))

/* TGU		-- Trap on Greater Unsigned */
#define iTGU(s1,s2)	(0x99d00000 | CG_RS1(s1) | (s2))

/* TCC		-- Trap on Carry Clear (Greater than or Equal, Unsigned) */
#define iTCC(s1,s2)	(0x9bd00000 | CG_RS1(s1) | (s2))

/* TPOS		-- Trap on Positive */
#define iTPOS(s1,s2)	(0x9dd00000 | CG_RS1(s1) | (s2))

/* TVC		-- Trap of Overflow Clear */
#define iTVC(s1,s2)	(0x9fd00000 | CG_RS1(s1) | (s2))


/*
 * B.25		Read State Register Instructions
 */

/* RDY		-- Read Y register */
#define iRDY(d)	(0x81400000 | CG_RD(d))

/* RDPSR	-- Read Processor State Register */
#define iRDPSR(d) (0x81480000 | CG_RD(d))

/* RDWIM	-- Read Window Invalid Mask register */
#define iRDWIM(d) (0x81500000 | CG_RD(d))

/* RDTBR	-- Read Trap Base Register */
#define iRDTBR(d) (0x81580000 | CG_RD(d))


/*
 * B.26		Write State Register Instructions
 */

/* WRY		-- Write Y Register */
#define iWRY(s1,s2)	(0x81800000 | CG_RS1(s1) | (s2))

/* WRPSR	-- Write Processor State Register */
#define iWRPSR(s1,s2)	(0x81880000 | CG_RS1(s1) | (s2))

/* WRWIM	-- Write Window Invalid Mask */
#define iWRWIM(s1,s2)	(0x81900000 | CG_RS1(s1) | (s2))

/* WRTBR	-- Write Trap Base Register */
#define iWRTBR(s1,s2)	(0x81980000 | CG_RS1(s1) | (s2))


/*
 * B.27		Unimplemented Instruction
 */

/* UNIMP	-- Unimplemented */
#define iUNIMP(c22) (0x00000000 | CG_D22(c22))


/*
 * B.28		Instruction Cache Flush Instruction
 */

/* IFLUSH	-- Instruction Cache Flush */
#define iIFLUSH(s1,s2) (0x81d80000 | CG_RS1(s1) | (s2))


/*
 * B.29		Floating-point Operate (FPop) Instructions
 *
 * We probably won't need these so I won't put them in.
 */


/*
 * B.30		Coprocessor Operate Instructions
 *
 * I'm not putting these in either.
 */
