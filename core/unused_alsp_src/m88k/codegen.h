/* ************************************************************************

   codegen.h
	Copyright (c) 1988 Motorola, Inc.
	Copyright (c) 1988-90 Applied Logic Systems, Inc.

   MC88100 Code Generator Instruction Macros
   --include file for 88100 code generator

   Author:   Brian Keith Short

   Creation: April 1988

   Revision: 6/13/91,	S. Medeiros	-- fixed JSR and JSRN

   Revision: 

************************************************************************** */

/*
   General Purpose Registers
*/
#define R0     0
#define R1     1
#define R2     2
#define R3     3
#define R4     4
#define R5     5
#define R6     6
#define R7     7
#define R8     8
#define R9     9
#define R10   10
#define R11   11
#define R12   12
#define R13   13
#define R14   14
#define R15   15
#define R16   16
#define R17   17
#define R18   18
#define R19   19
#define R20   20
#define R21   21
#define R22   22
#define R23   23
#define R24   24
#define R25   25
#define R26   26
#define R27   27
#define R28   28
#define R29   29
#define R30   30
#define R31   31

/*
   CMP Return String Format
*/
#define EQ    2
#define NE    3
#define GT    4
#define LE    5
#define LT    6
#define GE    7
#define HI    8
#define LS    9
#define LO   10
#define HS   11


/*
   BCND Branch Conditions
*/
#define EQ0   2   
#define NE0   (GT0 | LT0)   
#define GT0   1   
#define LT0   4   
#define GE0   (GT0 | EQ0)   
#define LE0   (LT0 | EQ0)   


/*
   Control Registers
*/
#define CR0   0
#define CR1   1
#define CR2   2
#define CR3   3
#define CR4   4
#define CR5   5
#define CR6   6
#define CR7   7
#define CR8   8
#define CR9   9
#define CR10 10
#define CR11 11
#define CR12 12
#define CR13 13
#define CR14 14
#define CR15 15
#define CR16 16

#define PID   CR0
#define PSR   CR1
#define TPSR  CR2
#define SSB   CR3
#define SXIP  CR4
#define SNIP  CR5
#define SFIP  CR6
#define VBR   CR7
#define DMT0  CR8
#define DMD0  CR9
#define DMA0  CR10
#define DMT1  CR11
#define DMD1  CR12
#define DMA1  CR13
#define DMT2  CR14
#define DMD2  CR15
#define DMA2  CR16


/*
   ADD(rd,rs1,rs2)

   Signed Add (without carry)
*/
#define ADD(rd,rs1,rs2) (0xf4007000|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDCI(rd,rs1,rs2)

   Signed Add plus Carry
*/
#define ADDCI(rd,rs1,rs2) (0xf4007200|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDCO(rd,rs1,rs2)

   Signed Add propagate Carry Out
*/
#define ADDCO(rd,rs1,rs2) (0xf4007100|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDCIO(rd,rs1,rs2)

   Signed Add plus Carry propagate Carry Out
*/
#define ADDCIO(rd,rs1,rs2) (0xf4007300|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDI(rd,rs1,lit16)

   Signed Add with Literal (without carry)
*/
#define ADDI(rd,rs1,lit16) (0x70000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   ADDU(rd,rs1,rs2)

   Unsigned Add (without carry)
*/
#define ADDU(rd,rs1,rs2) (0xf4006000|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDUCI(rd,rs1,rs2)

   Unsigned Add plus Carry
*/
#define ADDUCI(rd,rs1,rs2) (0xf4006200|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDUCO(rd,rs1,rs2)

   Unsigned Add propagate Carry Out
*/
#define ADDUCO(rd,rs1,rs2) (0xf4006100|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDUCIO(rd,rs1,rs2)

   Unsigned Add plus Carry propagate Carry Out
*/
#define ADDUCIO(rd,rs1,rs2) (0xf4006300|(rd << 21)|(rs1 << 16)|rs2)


/*
   ADDUI(rd,rs1,lit16)

   Unsigned Add with Literal (without carry)
*/
#define ADDUI(rd,rs1,lit16) (0x60000000|(rd << 21)|(rs1 << 16)|(lit16))


/*
   AND(rd,rs1,rs2)

   Logical AND
*/
#define AND(rd,rs1,rs2) (0xf4004000|(rd << 21)|(rs1 << 16)|rs2)


/*
   ANDC(rd,rs1,rs2)

   Logical AND, complement
*/
#define ANDC(rd,rs1,rs2) (0xf4004400|(rd << 21)|(rs1 << 16)|rs2)


/*
   ANDI(rd,rs1,lit16)

   Logical AND with literal (lower half word)
*/
#define ANDI(rd,rs1,lit16) (0x40000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   ANDIU(rd,rs1,lit16)

   Logical AND with literal (upper half word)
*/
#define ANDIU(rd,rs1,lit16) (0x44000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   BB0(b5,rs1,off16)

   Branch on Bit Clear
*/
#define BB0(b5,rs1,off16) (0xd0000000|(b5 << 21)|(rs1 << 16)|((off16)&0xffff))


/*
   BB0N(b5,rs1,off16)

   Branch on Bit Clear (execute next instruction)
*/
#define BB0N(b5,rs1,off16) (0xd4000000|(b5 << 21)|(rs1 << 16)|((off16)&0xffff))


/*
   BB1(b5,rs1,off16)

   Branch on Bit Set
*/
#define BB1(b5,rs1,off16) (0xd8000000|(b5 << 21)|(rs1 << 16)|((off16)&0xffff))


/*
   BB1N(b5,rs1,off16)

   Branch on Bit Set (execute next instruction)
*/
#define BB1N(b5,rs1,off16) (0xdc000000|(b5 << 21)|(rs1 << 16)|((off16)&0xffff))


/*
   BCND(m5,rs1,off16)

   Conditional Branch
*/
#define BCND(m5,rs1,off16) (0xe8000000|((m5) << 21)|(rs1 << 16)|((off16)&0xffff))


/*
   BCNDN(b5,rs1,off16)

   Conditional Branch (execute next instruction)
*/
#define BCNDN(m5,rs1,off16) (0xec000000|(m5 << 21)|(rs1 << 16)|((off16)&0xffff))


/*
  BR(off26)

   Unconditional Branch
*/
#define BR(off26) (0xc0000000|((off26)&0x3ffffff))


/*
  BRN(off26)

   Unconditional Branch (execute next instruction)
*/
#define BRN(off26) (0xc4000000|((off26)&0x3ffffff))


/*
  BSR(off26)

   Branch to Subroutine
*/
#define BSR(off26) (0xc8000000|((off26)&0x3ffffff))


/*
  BSRN(off26)

  Branch to Subroutine (execute next instruction)
*/
#define BSRN(off26) (0xcc000000|((off26)&0x3ffffff))


/*
   CLRI(rd,rs1,w5,o5)

   Clear Bit Field (immediate offset and width)
*/
#define CLRI(rd,rs1,w5,o5) (0xf0008000|(rd << 21)|(rs1 << 16)|(w5 << 5)|o5)


/*
   CLR(rd,rs1,rs2)

   Clear Bit Field (register offset and width)
*/
#define CLR(rd,rs1,rs2) (0xf4008000|(rd << 21)|(rs1 << 16)|rs2)



/*
   CMP(rd,rs1,rs2)

   Integer Compare
*/
#define CMP(rd,rs1,rs2) (0xf4007c00|(rd << 21)|(rs1 << 16)|rs2)


/*
   CMPI(rd,rs1,lit16)

   Integer Compare with literal
*/
#define CMPI(rd,rs1,lit16) (0x7c000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   DIV(rd,rs1,rs2) 

   Signed Integer Divide
*/
#define DIV(rd,rs1,rs2) (0xf4007800|(rd << 21)|(rs1 << 16)|rs2)


/*
   DIVI(rd,rs1,lit16)

   Signed Integer Divide with literal
*/
#define DIVI(rd,rs1,lit16) (0x78000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   DIVU(rd,rs1,rs2) 

   Unsigned Integer Divide
*/
#define DIVU(rd,rs1,rs2) (0xf4006800|(rd << 21)|(rs1 << 16)|rs2)


/*
   DIVUI(rd,rs1,lit16)

   Unsigned Integer Divide with literal
*/
#define DIVUI(rd,rs1,lit16) (0x68000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   EXTI(rd,rs1,w5,o5)

   Extract Signed Bit Field (immediate width and offset)
*/
#define EXTI(rd,rs1,w5,o5) (0xf0009000|(rd << 21)|(rs1 << 16)|(w5 << 5)|o5)


/*
   EXT(rd,rs1,rs2)

   Extract Signed Bit Field (register width and offset)
*/
#define EXT(rd,rs1,rs2) (0xf4009000|(rd << 21)|(rs1 << 16)|rs2)


/*
   EXTUI(rd,rs1,w5,o5)

   Extract Unsigned Bit Field (immediate width and offset)
*/
#define EXTUI(rd,rs1,w5,o5) (0xf0009800|(rd << 21)|(rs1 << 16)|(w5 << 5)|o5)


/*
   EXTU(rd,rs1,rs2)

   Extract Unsigned Bit Field (register width and offset)
*/
#define EXTU(rd,rs1,rs2) (0xf4009800|(rd << 21)|(rs1 << 16)|rs2)


/*
   FADD/FCMP/FDIV/FLDCR/FLT/FMUL/FSTCR/FSUB/FXCR/INT/NINT/TRNC not implemented
*/


/*
   FF0(rd,rs2)

   Find First Bit Clear
*/
#define FF0(rd,rs2) (0xf400ec00|(rd << 21)|rs2)


/*
   FF1(rd,rs2)

   Find First Bit Set
*/
#define FF1(rd,rs2) (0xf400e800|(rd << 21)|rs2)


/*
   JMP(rs2)

   Unconditional Jump
*/
#define JMP(rs2) (0xf400c000|rs2)


/*
   JMPN(rs2)

   Unconditional Jump (execute next instruction)
*/
#define JMPN(rs2) (0xf400c400|rs2)


/*
   JSR(rs2)

   Jump to Subroutine
*/
#define JSR(rs2) (0xf400c800|rs2)


/*
   JSRN(rs2)

   Jump to Subroutine (execute next instruction)
*/
#define JSRN(rs2) (0xf400cc00|rs2)


/*
   LDI(rd,rs1,lit16)

   Load Register from Memory with literal (unscaled)
*/
#define LDI(rd,rs1,lit16) (0x14000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   LD(rd,rs1,rs2)

   Load Register from Memory (unscaled)
*/
#define LD(rd,rs1,rs2) (0xf4001400|(rd << 21)|(rs1 << 16)|rs2)


/*
   LDS(rd,rs1,rs2)

   Load Register from Memory (scaled)
*/
#define LDS(rd,rs1,rs2) (0xf4001600|(rd << 21)|(rs1 << 16)|rs2)


/*
   LDAI(rd,rs1,lit16)

   Load Address with literal (unscaled)
*/
#define LDAI(rd,rs1,lit16) (0x34000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   LDA(rd,rs1,rs2)

   Load Address (unscaled)
*/
#define LDA(rd,rs1,rs2)  ADDU(rd,rs1,rs2)


/*
   LDAS(rd,rs1,rs2)

   Load Address (scaled)
*/
#define LDAS(rd,rs1,rs2) (0xf4003600|(rd << 21)|(rs1 << 16)|rs2)


/*
   LDCR(rd,crs)

   Load from Control Register
*/
#define LDCR(rd,crs) (0x80004000|(rd << 21)|(crs << 5))

/*
   LDHUI

   Load Half Unsigned from Memory with literal (unscaled)
*/
#define LDHUI(rd,rs1,lit16) (0x08000000|((rd)<<21)|((rs1)<<16)|(lit16))

/*
   MAKI(rd,rs1,w5,o5)

   Make Bit Field (immediate width and offset)
*/
#define MAKI(rd,rs1,w5,o5) (0xf000a000|(rd << 21)|(rs1 << 16)|(w5 << 5)|o5)


/*
   MAK(rd,rs1,rs2)

   Make Bit Field (register width and offset)
*/
#define MAK(rd,rs1,rs2) (0xf400a000|(rd << 21)|(rs1 << 16)|rs2)


/*
   MASK(rd,rs1,lit16)

   Logical Mask Immediate (lower half word)
*/
#define MASK(rd,rs1,lit16) (0x48000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   MASKU(rd,rs1,lit16)

   Logical Mask Immediate (lower half word)
*/
#define MASKU(rd,rs1,lit16) (0x4c000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   MUL(rd,rs1,rs2)

   Integer Multiply
*/
#define MUL(rd,rs1,rs2) (0xf4006c00|(rd << 21)|(rs1 << 16)|rs2)


/*
   MULI(rd,rs1,lit16)

   Integer Multiply with literal
*/
#define MULI(rd,rs1,lit16) (0x6c000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   OR(rd,rs1,rs2)

   Logical OR
*/
#define OR(rd,rs1,rs2) (0xf4005800|(rd << 21)|(rs1 << 16)|rs2)


/*
   ORC(rd,rs1,rs2)

   Logical OR (complement source 2 operand)
*/
#define ORC(rd,rs1,rs2) (0xf4005c00|(rd << 21)|(rs1 << 16)|rs2)


/*
   ORI(rd,rs1,lit16)

   Logical OR with literal (lower half word)
*/
#define ORI(rd,rs1,lit16) (0x58000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   ORU(rd,rs1,lit16)

   Logical OR with literal (upper half word)
*/
#define ORU(rd,rs1,lit16) (0x5c000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   ROTI(rd,rs1,o5)

   Rotate Register (immediate offset)
*/
#define ROTI(rd,rs1,o5) (0xf000a800|(rd << 21)|(rs1 << 16)|o5)


/*
   ROT(rd,rs1,rs2)

   Rotate Register (register offset)
*/
#define ROT(rd,rs1,rs2) (0xf400a800|(rd << 21)|(rs1 << 16)|rs2)


/* 
   RTE

   Return from Exception
*/
#define RTE (0xf400fc00)


/*
   SETI(rd,rs1,w5,o5)

   Set Bit Field (immediate width and offset)
*/
#define SETI(rd,rs1,w5,o5) (0xf0008800|(rd << 21)|(rs1 << 16)|(w5 << 5)|o5)


/*
   SET(rd,rs1,rs2)

   Set Bit Field (register width and offset)
*/
#define SET(rd,rs1,rs2) (0xf4008800|((rd) << 21)|((rs1) << 16)|(rs2))


/*
   STI(rs,rs1,lit16)

   Store Register to Memory with literal (unscaled)
*/
#define STI(rs,rs1,lit16) (0x24000000|((rs) << 21)|((rs1) << 16)|(lit16))


/*
   ST(rs,rs1,rs2)

   Store Register to Memory (unscaled)
*/
#define ST(rs,rs1,rs2) (0xf4002400|((rs) << 21)|((rs1) << 16)|(rs2))


/*
   STS(rs,rs1,rs2)

   Store Register to Memory (scaled)
*/
#define STS(rs,rs1,rs2) (0xf4002600|(rs << 21)|(rs1 << 16)|rs2)


/*
   STCR(rs,crd)

   Store to Control Register
*/
#define STCR(rs,crd) (0x80008000|(rs << 16)|(crd << 5)|rs)



/*
   SUB(rd,rs1,rs2)

   Signed Integer Subtract (without borrow)
*/
#define SUB(rd,rs1,rs2) (0xf4007400|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBCI(rd,rs1,rs2)

   Signed Integer Subtract (propogate borrow in)
*/
#define SUBCI(rd,rs1,rs2) (0xf4007600|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBCO(rd,rs1,rs2)

   Signed Integer Subtract (propogate borrow out)
*/
#define SUBCO(rd,rs1,rs2) (0xf4007500|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBCIO(rd,rs1,rs2)

   Signed Integer Subtract (propogate borrow in and out)
*/
#define SUBCIO(rd,rs1,rs2) (0xf4007700|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBI(rd,rs1,lit16)

   Signed Integer Subtract with literal (without borrow)
*/
#define SUBI(rd,rs1,lit16) (0x74000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   SUBU(rd,rs1,rs2)

   Unsigned Integer Subtract (without borrow)
*/
#define SUBU(rd,rs1,rs2) (0xf4006400|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBCI(rd,rs1,rs2)

   Unsigned Integer Subtract (propogate borrow in)
*/
#define SUBUCI(rd,rs1,rs2) (0xf4006600|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBUCO(rd,rs1,rs2)

   Unsigned Integer Subtract (propogate borrow out)
*/
#define SUBUCO(rd,rs1,rs2) (0xf4006500|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBUCIO(rd,rs1,rs2)

   Unsigned Integer Subtract (propogate borrow in and out)
*/
#define SUBUCIO(rd,rs1,rs2) (0xf4006700|(rd << 21)|(rs1 << 16)|rs2)


/*
   SUBUI(rd,rs1,lit16)

   Unsigned Integer Subtract with literal (without borrow)
*/
#define SUBUI(rd,rs1,lit16) (0x64000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   TB0(b5,rs1,vec9)

   Trap on Bit Clear
*/
#define TB0(b5,rs1,vec9) (0xf000d000|(b5 << 21)|(rs1 << 16)|vec9)


/*
   TB1(b5,rs1,vec9)

   Trap on Bit Set
*/
#define TB1(b5,rs1,vec9) (0xf000d800|(b5 << 21)|(rs1 << 16)|vec9)


/*
   TBNDI(rs1,lit16)

   Trap on Bound Check with literal
*/
#define TBNDI(rs1,lit16) (0xf8000000|((rs1) << 16)|(lit16))


/*
   TBND(rs1,rs2)

   Trap on Bound Check 
*/
#define TBND(rs1,rs2) (0xf400f800|(rs1 << 16)|rs2)


/*
   TCND(m5,rs1,vec9)

   Conditional Trap 
*/
#define TCND(m5,rs1,vec9) (0xf000e800|(m5 << 21)|(rs1 << 16)|vec9)


/*
   XCR(rd,rs,cr)

   Exchange Control Register
*/
#define XCR(rd,rs,cr) (0x8000c000|(rd << 21)|(rs << 16)|(cr << 5)|rs)


/*
   XMEMI(rd,rs1,lit16)

   Exchange Register with Memory with literal
*/
#define XMEMI(rd,rs1,lit16) (0x04000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   XMEM(rd,rs1,rs2)

   Exchange Register with Memory (unscaled)
*/
#define XMEM(rd,rs1,rs2) (0xf4000400|(rd << 21)|(rs1 << 16)|rs2)


/*
  XMEMS(rd,rs1,rs2)

  Exchange Register with Memory (scaled)
*/
#define XMEMS(rd,rs1,rs2) (0xf4000600|(rd << 21)|(rs1 << 16)|rs2)


/*
   XOR(rd,rs1,rs2)

   Logical Exclusive OR
*/
#define XOR(rd,rs1,rs2) (0xf4005000|(rd << 21)|(rs1 << 16)|rs2)


/*
   XORI(rd,rs1,lit16)

   Logical Exclusive OR with literal (lower half word)
*/
#define XORI(rd,rs1,lit16) (0x50000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   XORIU(rd,rs1,lit16)

   Logical Exclusive OR with literal (upper half word)
*/
#define XORIU(rd,rs1,lit16) (0x54000000|((rd) << 21)|((rs1) << 16)|(lit16))


/*
   XORC(rd,rs1,rs2)

   Logical Exclusive OR, complement source 2 operand
*/
#define XORC(rd,rs1,rs2) (0xf4005400|(rd << 21)|(rs1 << 16)|rs2)



