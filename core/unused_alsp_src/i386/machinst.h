 /*
 * machinst.h --  Definitions of the machine instructions for the 386
 *     code generator
 *
 * Copyright (c) 1988 Applied Logic Systems, Inc.
 *
 * Author: Keith M. Hughes
 * Creation Date: 1/88
 * Revision History:
 *  7/17/90  Ilyas Cicekli
 *   This file is reorganized and macros for inline math are added.
 */

#include "winter.h"




/* 
 * Remember a point in the code stream by putting ic_ptr into a variable. 
 */
#define LABEL(v)  { v = ic_ptr; }

/* 
 * Calculate the offset from the current code pointer to the label v 
 */
#define OFFSET(v)  ((ic_ptr-(CodePtr)(v))*CodeSize)

/* 
 * These two macros calculate the distance from the code pointer back to 
 * a label in both SmallOffset size and BigOffset sizes 
 */
#define BDISP(v)  ((SmallOffset)(-OFFSET(v)-sizeof(SmallOffset)))
#define BLDISP(v)  ((BigOffset)(-OFFSET(v)-sizeof(BigOffset)))

/* 
 * Patch an instruction already layed down 
 */
#define PATCHDISP(v)  { *(((SmallOffset *)(v))-1) |= (SmallOffset)OFFSET(v); }
#define PATCHLDISP(v)  { *(((BigOffset *)(v))-1) |=  (BigOffset)OFFSET(v); }

/* 
 * Code has been shifted by offset code words. ANy labels found in the code
 * must be shifted also. Do so. 
 */
#define SHIFTLABEL(v,off)  { v += (off); }

/* 
 * Move the code pointer back to some label. 
 */
#define MOVETOLABEL(pos)  { ic_ptr = (CodePtr)(pos); }

/* 
 * The equivalent of . in an assembler. Give the current code position. 
 */
#define CURRENTPOSITION  ic_ptr



/*
 * No base register
 */
#define NOBASE -1



/*
 * Move Instructions
 */
#define MOVRR(r1,r2)     { ic_put(0x8b); ic_put(0xc0 | (r1<<3) | r2); }
#define MOVRM(r1,r2,disp)  { ic_put(0x8b); \
    icPutAddrMode(r1,r2,(Offset)(disp)); }
#define MOVRM_RELOC(r1,r2,disp,rtype,rval)  \
   { ic_put(0x8b);  \
     icPutAddrMode(r1,r2,(Offset)(disp));\
     RELOC_INFO(rtype,(char *)ic_ptr-sizeof(long),rval);\
   }
#define MOVMR(r1,disp,r2)  { ic_put(0x89); \
   icPutAddrMode(r2,r1,(Offset)(disp)); }
#define MOVRI(r1,imm)     { ic_put(0xb8|r1); ic_putl((BigOffset)(imm)); }
#define MOVRI_RELOC(r1,imm,rtype,rval) \
  { ic_put(0xb8|r1); \
    RELOC_INFO(rtype,ic_ptr,rval); \
    ic_putl((BigOffset)(imm)); }
#define MOVMI(r1,disp,imm) { ic_put(0xc7); \
    icPutAddrMode(0,r1,(Offset)(disp)); \
    ic_putl(imm); }
/*
 * This baby is 6 bytes long (6 Code words) and is used by cp instructions
 * so that things align on proper word boundaries.
*/
#define LMOVRI(r1,imm)  { ic_put(0xc7); ic_put(0xc0 | r1); ic_putl(imm); }

/*
 * Load Effective Address to Register
 */
#define LEA(r1,r2,disp) { ic_put(0x8d); \
  icPutAddrMode(r1,r2,(Offset)(disp)); }

/*
 * Add Instructions 
 */
#define ADDRR(r1,r2)   { ic_put(0x03); ic_put(0xc0 | (r1<<3) | r2); }
#define ADDRM(r1,r2,disp) \
 { ic_put(0x03); \
   icPutAddrMode(r1,r2,(Offset)(disp)); }
#define ADDMR(r1,disp,r2) \
 { ic_put(0x01); \
   icPutAddrMode(r2,r1,(Offset)(disp)); }
#define ADDRI(r1,imm) { ic_put(0x81); ic_put(0xc0 | r1); ic_putl(imm); }
#define ADDMI(r1,disp,imm) \
 { ic_put(0x81); \
   icPutAddrMode(0,r1,(Offset)(disp)); \
   ic_putl(imm); }

/*
 * Subtract Instructions 
 */
#define SUBRR(r1,r2)   { ic_put(0x2b); ic_put(0xc0 | (r1<<3) | r2); }
#define SUBRM(r1,r2,disp) \
 { ic_put(0x2b); \
   icPutAddrMode(r1,r2,(Offset)(disp)); }
#define SUBMR(r1,disp,r2) \
 { ic_put(0x29); \
   icPutAddrMode(r2,r1,(Offset)(disp)); }
#define SUBRI(r1,imm)  { ic_put(0x81); ic_put(0xe8 | r1); ic_putl(imm); }
#define SUBMI(r1,disp,imm) \
 { ic_put(0x81); \
   icPutAddrMode(5,r1,(Offset)(disp));  \
   ic_putl(imm); }

/*
 * Compare Instructions
 */
#define CMPRR(r1,r2)   { ic_put(0x3b); ic_put(0xc0 | (r1<<3) | r2); }
#define CMPRM(r1,r2,disp)  { ic_put(0x3b);         \
         icPutAddrMode(r1,r2,(Offset)(disp)); }
#define CMPRM_RELOC(r1,r2,disp,rtype,rval)  \
      { ic_put(0x3b);      \
        icPutAddrMode(r1,r2,(Offset)(disp)); \
        RELOC_INFO(rtype,(char *)ic_ptr-sizeof(long),rval);\
         }
#define CMPMR(r1,disp,r2) { ic_put(0x39);         \
         icPutAddrMode(r2,r1,(Offset)(disp));}
#define CMPRI(r1,imm)   { ic_put(0x81); ic_put(0xf8 | r1); ic_putl(imm); }
#define CMPMI(r1,disp,imm)  { ic_put(0x81);         \
         icPutAddrMode(7,r1,(Offset)(disp));    \
         ic_putl(imm); }

/*
 * Increment Instructions
 */
#define INCR(r1)   { ic_put(0x40|r1); }  /* short form */
#define INCM(r1,disp)  { ic_put(0xff); icPutAddrMode(0,r1,(Offset)(disp)); }  

/*
 * Decrement Instructions
 */
#define DECR(r1)   { ic_put(0x48|r1); }  /* short form */
#define DECM(r1,disp)  { ic_put(0xff); icPutAddrMode(1,r1,(Offset)(disp)); }  

/*
 * NEG(Change Sign) Instructions
 */
#define NEGR(r1)   { ic_put(0xf7); ic_put(0xd8 | r1); }  
#define NEGM(r1,disp)  { ic_put(0xf7); icPutAddrMode(3,r1,(Offset)(disp)); }  

/*
 * Integer Multiply (signed) Instructions
 */
#define IMULRR(r1,r2)  { ic_put(0x0f); ic_put(0xaf);      \
         ic_put(0xc0 | (r1<<3) | r2); }
#define IMULRM(r1,r2,disp) { ic_put(0x0f); ic_put(0xaf);     \
         icPutAddrMode(r1,r2,(Offset)(disp));}

/*
 * Integer Divide (signed) Instructions
 */
#define IDIVEAXR(r1)  { ic_put(0xf7); ic_put(0xf8 | r1); }
#define IDIVEAXM(r1,disp) { ic_put(0xf7);         \
         icPutAddrMode(7,r1,(Offset)(disp)); }



/*
 * And Instructions
 */
#define ANDRR(r1,r2)   { ic_put(0x23); ic_put(0xc0 | (r1<<3) | r2); }
#define ANDRM(r1,r2,disp)  { ic_put(0x23);         \
         icPutAddrMode(r1,r2,(Offset)(disp)); }
#define ANDMR(r1,disp,r2) { ic_put(0x21);         \
         icPutAddrMode(r2,r1,(Offset)(disp)); }
#define ANDRI(r1,imm)   { ic_put(0x81); ic_put(0xe0 | r1); ic_putl(imm); }
#define ANDMI(r1,disp,imm)  { ic_put(0x81);         \
         icPutAddrMode(4,r1,(Offset)(disp));    \
         ic_putl(imm); }

/*
 * Or Instructions 
 */
#define ORRR(r1,r2)   { ic_put(0x0b); ic_put(0xc0 | (r1<<3) | r2); }
#define ORRM(r1,r2,disp)  { ic_put(0x0b);         \
         icPutAddrMode(r1,r2,(Offset)(disp)); }
#define ORMR(r1,disp,r2) { ic_put(0x09);         \
         icPutAddrMode(r2,r1,(Offset)(disp)); }
#define ORRI(r1,imm)   { ic_put(0x81); ic_put(0xc8 | r1); ic_putl(imm); }
#define ORMI(r1,disp,imm)  { ic_put(0x81);         \
         icPutAddrMode(1,r1,(Offset)(disp));    \
         ic_putl(imm); }

/*
 * Exclusive Or Instructions 
 */
#define XORRR(r1,r2)   { ic_put(0x33); ic_put(0xc0 | (r1<<3) | r2); }
#define XORRM(r1,r2,disp)  { ic_put(0x33);         \
         icPutAddrMode(r1,r2,(Offset)(disp)); }
#define XORMR(r1,disp,r2) { ic_put(0x31);         \
         icPutAddrMode(r2,r1,(Offset)(disp)); }
#define XORRI(r1,imm)   { ic_put(0x81); ic_put(0xf0 | r1); ic_putl(imm); }
#define XORMI(r1,disp,imm)  { ic_put(0x81);         \
         icPutAddrMode(6,r1,(Offset)(disp));    \
         ic_putl(imm); }

/*
 * NOT(Invert) Instructions
 */
#define NOTR(r1)   { ic_put(0xf7); ic_put(0xd0 | r1); }  
#define NOTM(r1,disp)  { ic_put(0xf7); icPutAddrMode(2,r1,(Offset)(disp)); }  


/*
 * Shift Left Instructions
 */
#define SHLR(r1,imm)   { ic_put(0xc1); ic_put(0xe0 | r1); ic_put(imm); }
#define SHLM(r1,disp,imm)  { ic_put(0xc1);         \
         icPutAddrMode(4,r1,(Offset)(disp));   \
         ic_put((char)imm); }
#define SHLCLR(r1)    { ic_put(0xd3); ic_put(0xe0 | r1); }
#define SHLCLM(r1,disp)  { ic_put(0xd3);         \
         icPutAddrMode(4,r1,(Offset)(disp)); }

/*
 * Shift Left (Arithmetic) Instructions (same as SHL)
 */
#define SALR(r1,imm)   SHLR(r1,imm) 
#define SALM(r1,disp,imm)  SHLM(r1,disp,imm) 
#define SALCLR(r1)    SHLCLR(r1) 
#define SALCLM(r1,disp)  SHLCLM(r1,disp) 

/*
 * Shift Right Instructions
 */
#define SHRR(r1,imm)   { ic_put(0xc1); ic_put(0xe8 | r1); ic_put(imm); }
#define SHRM(r1,disp,imm)  { ic_put(0xc1);         \
         icPutAddrMode(5,r1,(Offset)(disp));   \
         ic_put((char)imm); }
#define SHRCLR(r1)    { ic_put(0xd3); ic_put(0xe8 | r1); }
#define SHRCLM(r1,disp)  { ic_put(0xd3);         \
         icPutAddrMode(5,r1,(Offset)(disp)); }

/*
 * Shift Right (Arithmetic) Instructions
 */
#define SARR(r1,imm)   { ic_put(0xc1); ic_put(0xf8 | r1); ic_put(imm); }
#define SARM(r1,disp,imm)  { ic_put(0xc1);         \
         icPutAddrMode(7,r1,(Offset)(disp));   \
         ic_put((char)imm); }
#define SARCLR(r1)    { ic_put(0xd3); ic_put(0xf8 | r1); }
#define SARCLM(r1,disp)  { ic_put(0xd3);         \
         icPutAddrMode(7,r1,(Offset)(disp)); }



/*
 * Push and Pop Instructions
 */
#define PUSHR(r1)   { ic_put(0x50 | r1); }  /* short form */ 
#define PUSHM(r1,disp)  { ic_put(0xff); icPutAddrMode(6,r1,(Offset)(disp)); }
#define PUSHI(imm)   { ic_put(0x68); ic_putl((PWord)(imm)); }

#define POPR(r1)   { ic_put(0x58 | r1); }  /* short form */ 
#define POPM(r1,disp)  { ic_put(0x8f); icPutAddrMode(0,r1,(Offset)(disp)); }



/*
 * Control Transfer (Jump, Call, Return)
 */
#define JMPR(r1)   { ic_put(0xff); ic_put(0xe0 | r1); }
#define JMPI(r1,disp)  { ic_put(0xff); icPutAddrMode(4,r1,(Offset)(disp)); }
#define JMP(offset)  { ic_put(0xe9); ic_putl((BigOffset)(offset)); }

#define CALLR(r1)   { ic_put(0xff); ic_put(0xd0 | r1); }
#define CALLI(r1,disp)  { ic_put(0xff); icPutAddrMode(2,r1,(Offset)(disp)); }
#define CALLI_RELOC(r1,disp,rtype,rval)  \
      { ic_put(0xff); \
        icPutAddrMode(2,r1,(Offset)(disp)); \
        RELOC_INFO(rtype,(char *)ic_ptr-sizeof(long),rval);\
      }
#define CALL(addr)   { MOVRI(EBX,addr) CALLR(EBX) }
#define CALL_RELOC(addr,rtype,rval) \
      { MOVRI_RELOC(EBX,addr,rtype,rval); CALLR(EBX) }

#define RET    { ic_put(0xc3); }



/*
 * Conditional Jumps
 */
#define JO(off)   { ic_put(0x70); ic_put((SmallOffset)(off)); }
#define JNO(off)  { ic_put(0x71); ic_put((SmallOffset)(off)); }
#define JB(off)   { ic_put(0x72); ic_put((SmallOffset)(off)); }
#define JAE(off)  { ic_put(0x73); ic_put((SmallOffset)(off)); }
#define JE(off)   { ic_put(0x74); ic_put((SmallOffset)(off)); }
#define JNE(off)  { ic_put(0x75); ic_put((SmallOffset)(off)); }
#define JBE(off)  { ic_put(0x76); ic_put((SmallOffset)(off)); }
#define JA(off)   { ic_put(0x77); ic_put((SmallOffset)(off)); }
#define JS(off)   { ic_put(0x78); ic_put((SmallOffset)(off)); }
#define JNS(off)  { ic_put(0x79); ic_put((SmallOffset)(off)); }
#define JPE(off)  { ic_put(0x7a); ic_put((SmallOffset)(off)); }
#define JPO(off)  { ic_put(0x7b); ic_put((SmallOffset)(off)); }
#define JL(off)   { ic_put(0x7c); ic_put((SmallOffset)(off)); }
#define JGE(off)  { ic_put(0x7d); ic_put((SmallOffset)(off)); }
#define JLE(off)  { ic_put(0x7e); ic_put((SmallOffset)(off)); }
#define JG(off)   { ic_put(0x7f); ic_put((SmallOffset)(off)); }

#define JOL(off)  { ic_put(0x0f); ic_put(0x80); ic_putl((BigOffset)(off)); }
#define JNOL(off) { ic_put(0x0f); ic_put(0x81); ic_putl((BigOffset)(off)); }
#define JBL(off)  { ic_put(0x0f); ic_put(0x82); ic_putl((BigOffset)(off)); }
#define JAEL(off)  { ic_put(0x0f); ic_put(0x83); ic_putl((BigOffset)(off)); }
#define JEL(off)  { ic_put(0x0f); ic_put(0x84); ic_putl((BigOffset)(off)); }
#define JNEL(off)  { ic_put(0x0f); ic_put(0x85); ic_putl((BigOffset)(off)); }
#define JBEL(off)  { ic_put(0x0f); ic_put(0x86); ic_putl((BigOffset)(off)); }
#define JAL(off)  { ic_put(0x0f); ic_put(0x87); ic_putl((BigOffset)(off)); }
#define JSL(off)  { ic_put(0x0f); ic_put(0x88); ic_putl((BigOffset)(off)); }
#define JNSL(off)  { ic_put(0x0f); ic_put(0x89); ic_putl((BigOffset)(off)); }
#define JPEL(off)  { ic_put(0x0f); ic_put(0x8a); ic_putl((BigOffset)(off)); }
#define JPOL(off)  { ic_put(0x0f); ic_put(0x8b); ic_putl((BigOffset)(off)); }
#define JLL(off)  { ic_put(0x0f); ic_put(0x8c); ic_putl((BigOffset)(off)); }
#define JGEL(off)  { ic_put(0x0f); ic_put(0x8d); ic_putl((BigOffset)(off)); }
#define JLEL(off)  { ic_put(0x0f); ic_put(0x8e); ic_putl((BigOffset)(off)); }
#define JGL(off)  { ic_put(0x0f); ic_put(0x8f); ic_putl((BigOffset)(off)); }


/*
 * NOP
 */
#define NOP    { ic_put(0x90); }


/*
 * FAIL
 */
#define FAIL   { MOVRM(EAX,NOBASE,&wm_b_reg) JMPI(EAX,0) } 
#define FAIL_RELOC(rtype,rval)   \
     { MOVRM_RELOC(EAX,NOBASE,&wm_b_reg,rtype,rval) \
       JMPI(EAX,0) } 


extern int ic_base_nums[];
extern CodePtr ic_macropatch1;
extern CodePtr ic_macropatch2;

