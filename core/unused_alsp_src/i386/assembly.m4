__;
__;	assembly.m4		include file for all other m4 files
__;
__;		Consolidates wmregs.m4, wamregs.m4, wamops.m4,
__;		chpt.m4, cinter.m4, mtypes.m4, symbols.m4, token.m4,
__;		and wntbl.m4
__;
__;	Date : 4/9/93

__;
__; The following are from wmregs.m4
__;
__; The following are offsets to the associated values in the
__;	wm_regs structure. This should go away when we have kevins
__;	assembler_translator(atrans) working for 386.
__;
__;#define wm_HB_idx	0
__;#define wm_SPB_idx	1
__;#define wm_FAIL_idx	2
__;#define wm_B_idx	3
__;#define wm_TR_idx	4
__;#define wm_H_idx	5
__;#define wm_E_idx	6
__;#define wm_SP_idx	7

define(wm_HB,0)
define(wm_SPB,4)
define(wm_Fail,8)
define(wm_B,12)
define(wm_TR,16)
define(wm_H,20)
define(wm_E,24)
define(wm_SP,28)
define(wm_S,32)

__;
__; The following are from wamregs.m4
__;
__; Give definitions of the WAM registers for the assembler files.
__; These should also go away when atrans is used.
__;
__; These must match wamregs.h
__;

define(SP,ESP)
define(SPB,EDI)
define(HB,ESI)
define(H,EDX)
define(TR,ECX)
define(E,EBP)

__;
__; The following definitions are from  wamops.m4
__;
__; Definitions for WAM operations for the M4 files.
__;
__; Written by Keith Hughes	3/10/89
__; Applied Logic Systems, Inc.
__;

__;
__; ArgN(n)
__;
__; Give the proper addressing mode for argument N for the current procedure.
__;
__; We are assuming E hasn't moved.
__;

define(ArgN,`MADDR(eval($1*4+4),E)')

__;
__; GetArgN(dst,n)
__;
__; Put argument n into dst.
__;

define(GetArgN,`
	MOVL	OPRS($1,ArgN($2))
')

__;
__; PutArgN(n,src)
__;
__; Put src into argument n.
__;

define(PutArgN,`
	MOVL	OPRS(ArgN($1),$2)
')

__;
__; Bind(dst,src)
__;
__; Bind src to dst.
__;
__; In other word, dst is a variable pointer and we are binding this variable
__; with the value in src.
__;

define(Bind,`
	MOVL	OPRS(MADDR($1),$2)
')

__;
__; MakeCutPt(dst,src)
__;
__; Create a cut point integer in dst. The actual argument for wm_cut
__; is found in src.
__;
__; src is not destroyed by this operation.
__;

define(MakeCutPt,`
	MOVL	OPRS($1,GVAR(wm_heapbase))
	SUBL	OPRS($1,$2)

	MK_INT($1)
')

__;
__; ExtractCutPt(dst,src,srcb)
__;
__; Extract the cutpoint from src, which has it stored as a Prolog integer
__; and put the resulting cutpoint into dst.
__;
__; The value in dst is destroyed by the operation.
__;
__; Someday we will get rid of the srcb argument when we learn to use M4
__; a little better.
__;

define(ExtractCutPt,`
	ANDB	OPRS($3,IMM(SEMIMASK))
	SHRL	OPRS($2,IMM(MTP_CONSTSHIFT))
	MOVL	OPRS($1,GVAR(wm_heapbase))
	SUBL	OPRS($1,$2)
')

__;
__; Unbound(dst,src,label)
__;
__; src is assumed to be a variable. It is dereferenced once and the
__; result placed in dst. If it is an unbound, control will be transfered
__; to label
__;

define(Unbound,`
	MOVL	OPRS($1,MADDR($2))
	CMPL	OPRS($1,$2)
	JE	$3
')

__;
__; Deref(dst,src,srcb,grnd,drfloop)
__;
__; Provide a deref loop. The item to be dereferenced starts out in
__; src. The final dereferenced item will be found in dst. grnd is
__; a label to jump to if the item being dereferenced is ground. drfloop
__; is a label that the deref loop can use internally. srcb is the byte
__; representation of src.
__;
__; If src is ground, dst will have the only valid value. If src is
__; unbound, both dst and src will have copies of the unbound variable.
__;
__; This should be written so that the internal label (drfloop) is
__; generated internal to this macro. Also srcb should be removed when
__; possible.
__;

define(Deref,`

$5:			__; Jump here if we have to go around again.

			__; Make a copy of the item.
	MOVL	OPRS($1,$2)

			__; Check to see if ground
	ANDB	OPRS($3,IMM(MTP_TAGMASK))

			__; Jump if ground.
	JNE	SDISP($4)

			__; Dereference once and loop if another round.
	MOVL	OPRS($2,MADDR($1))
	CMPL	OPRS($1,$2)
	JNE	SDISP($5)
')

__;
__; Define choice point instructions
__;

__;
__; Cut
__;
__; Call the cut routine and return to next instruction
__;

define(Cut,`
	CALL	PROCADDR(docut)
')

__;
__; CutProceed
__;
__; Jump to the cut routine and don't return
__;

define(CutProceed,`
	POPL	EBP		_; Get Old E off of stack
	JMP	PROCADDR(docut)
')

__;
__; TryMe(Alternate)
__;
__; Put down a try_me instruction
__;

define(TryMe,`
	CALL	PROCADDR(wm_try_me)
	DD(PROCADDR($1))
')

__;
__; TrustMe
__;
__; Put down a try_me instruction
__;

define(TrustMe,`
	CALL	PROCADDR(wm_trust_me)
')

__;
__; Fail
__;
__; Fail the WAM
__;

define(Fail,`
			__; Get top address of choice point
	MOVL	OPRS(EAX,GVAR(wm_b_reg))
	JMP	REGDISP(MADDR(EAX))
')

__;
__; Proceed
__;
__; WAM PROCEED instruction.  Should match the one in the ICODE files.
__;
__; Destroys what was in EAX, but it is an assumption that this register
__; isn't valid through a normal control transfer.
__;

define(Proceed,`
	MOVL	OPRS(EAX,MADDR(4,EBP))	_; Get the return address
	MOVL	OPRS(EBP,MADDR(EBP))	_; Restore E from OldE

	JMP	REGDISP(EAX)		_; And proceed
')

__;
__; RegExecute(register)
__;
__; WAM execute instruction.  Should match the one in the ICODE files.
__; Assumes address to jump to is in a register.
__;
__; This is only identical to MemExecute because of assembler idiosyncracies.
__;

define(RegExecute,`
	MOVL	OPRS(EBP,ESP)	__; Make E point at top of new frame.

	JMP	REGDISP($1)	__; And proceed
')

__;
__; MemExecute(register)
__;
__; WAM execute instruction.  Should match the one in the ICODE files.
__; Assumes address to jump to is in memory.
__;
__; This is only identical to RegExecute because of assembler idiosyncracies.
__;

define(MemExecute,`
	MOVL	OPRS(EBP,ESP)	__; Make E point at top of new frame.

	JMP	REGDISP($1)	__; And proceed
')

__;
__; Trail(src)
__;
__; src must be trailed. Do it.
__;

define(Trail,`
	SUBL	OPRS(ECX,IMM(4))
	MOVL	OPRS(MADDR(ECX),$1)
')



__;
__; GCINFO(NArgsandNPV,mask)
__; 	
__; Low 16 bit of the first argument is the number of arguments and
__; high 16 bit is the size of the environment.
__;

define(GCINFO,`
	DD($1)								_; argument size and environment size
	DD($2) 								_; argument usage mask
	DD(-1) 								_; end of clause marker
	DD(0) 								_; builtin indicator
')

__;
__; wntbl.m4	-- machine specific name table entry sizes for wintcode.h if
__;		   used on the m4 assembler files. Only for 386.
__;
__; Must match values found in wntbl.h. This should go away when atrans is
__; implemented (except SIZEMODCODE and SIZERESOLVECODE )
__;	Copyright (c) 1989 Applied Logic Systems, Inc.
__;
__; Creation: 5/10/89
__; Author: Keith Hughes
__; 
__; Revision History:
__;     Revised:	mm/dd/yy	Who		Why and What
__;

__;
__; Size in Code ops of the stuff that comes before the code in the
__; ntbl_entry struct above. Code ops on 386 are bytes.
__;

define(NTBL_HEADERSIZE,36)

define(NTBL_OVERFLOWSIZE,6)
define(NTBL_CALLENTRYSIZE,1)
define(NTBL_EXECENTRYSIZE,14)
define(NTBL_CODESIZE,84)

__;
__; Offsets for name table entry fields. These should match the ntbl_entry
__; structure found in wintcode.h
__;

define(NTBL_TOKID_ARITY,0)
define(NTBL_FIRST_CLAUSE,4)
define(NTBL_LAST_CLAUSE,8)
define(NTBL_INDEX_BLOCK,12)
define(NTBL_TIMESTAMP,16)
define(NTBL_LO_ID,20)
define(NTBL_HI_ID,24)
define(NTBL_FLAGS,28)
define(NTBL_MODID,30)
define(NTBL_ICOUNT,32)
define(NTBL_NARGS,34)

__;
__; How big a complete name table entry (sans ->code area) is
__;

define(NTBL_ENTRYSIZE,eval(NTBL_HEADERSIZE+NTBL_OVERFLOWSIZE+
	NTBL_CALLENTRYSIZE+NTBL_EXECENTRYSIZE)
)

__; This is the number of Code words taken up by the module closure code in
__; the name table entry.

define(SIZEMODCODE,12)

__;
__; Size of the resolve_reference name table code in Code words (see iindex.c).
__;

define(SIZERESOLVECODE,7)



__;
__; The following are from chpt.m4
__;
__; Declarations describing choice points
__;

__;
__; Choice Point Structure:
__;
__;	+-----------------------+
__;	|       Prev B          |	+12
__;	+-----------------------+
__;	|       SPB             |	+8
__;	+-----------------------+
__;	|       HB              |	+4
__;	+-----------------------+
__;	|       Next Clause     | <-- B	+0
__;	+-----------------------+

define(chpt_HB,4)
define(chpt_SPB,8)
define(chpt_B,12)

define(chpt_SIZE,16)


__;
__; The following are from cinter.m4
__;
__; operations simplifying the interface to C
__;
__; Written by Keith Hughes
__;

__;
__; CReturn
__;
__; Register C uses to return values.
__;

define(CReturn,EAX)

__;
__; callCProc
__;

EXTRN(wm_regs,dword)
EXTRN(wm_regidx,dword)

define(callCProc,`
	MOVL	OPRS(EBX,GVAR(wm_regidx))
	SHLL	OPRS(EBX,IMM(6))
	ADDL	OPRS(EBX,OFFSET(GVAR(wm_regs)))
	MOVL	OPRS(MADDR(wm_TR,EBX),TR)
	MOVL	OPRS(MADDR(wm_H,EBX),H)
	MOVL	OPRS(MADDR(wm_E,EBX),E)
	MOVL	OPRS(MADDR(wm_HB,EBX),HB)
	MOVL	OPRS(MADDR(wm_SPB,EBX),SPB)

	CALL	PROCADDR($1)

	MOVL	OPRS(EBX,GVAR(wm_regidx))
	SHLL	OPRS(EBX,IMM(6))
	ADDL	OPRS(EBX,OFFSET(GVAR(wm_regs)))
	MOVL	OPRS(TR,MADDR(wm_TR,EBX))
	MOVL	OPRS(H,MADDR(wm_H,EBX))
	MOVL	OPRS(E,MADDR(wm_E,EBX))
	MOVL	OPRS(HB,MADDR(wm_HB,EBX))
	MOVL	OPRS(SPB,MADDR(wm_SPB,EBX))
')


__;
__; save Prolog registers
__; EBX is used as temporary register


define(SavePrologRegs, `
	MOVL	OPRS(EBX,GVAR(wm_regidx))
	SHLL	OPRS(EBX,IMM(6))
	ADDL	OPRS(EBX,OFFSET(GVAR(wm_regs)))
	MOVL	OPRS(MADDR(wm_TR,EBX),ECX)
	MOVL	OPRS(MADDR(wm_H,EBX),EDX)
	MOVL	OPRS(ECX,GVAR(wm_b_reg))		_; B register
	MOVL	OPRS(MADDR(wm_B,EBX),ECX)
	MOVL	OPRS(MADDR(wm_SP,EBX),ESP)
	MOVL	OPRS(MADDR(wm_E,EBX),EBP)
	MOVL	OPRS(MADDR(wm_HB,EBX),ESI)
	MOVL	OPRS(MADDR(wm_SPB,EBX),EDI)
')

__;
__; load Prolog registers.
__;	EBX is used as temporary register

define(LoadPrologRegs,`
	MOVL	OPRS(EBX,GVAR(wm_regidx))
	SHLL	OPRS(EBX,IMM(6))
	ADDL	OPRS(EBX,OFFSET(GVAR(wm_regs)))
	MOVL	OPRS(TR,MADDR(wm_TR,EBX))
	MOVL	OPRS(H,MADDR(wm_H,EBX))
	MOVL	OPRS(EDI,MADDR(wm_B,EBX))	_; Set B
	MOVL	OPRS(GVAR(wm_b_reg),EDI)
	MOVL	OPRS(SP,MADDR(wm_SP,EBX))
	MOVL	OPRS(E,MADDR(wm_E,EBX))
	MOVL	OPRS(HB,MADDR(wm_HB,EBX))
	MOVL	OPRS(SPB,MADDR(wm_SPB,EBX))
')


define(LoadCRegs,`
	MOVL 	OPRS(ESP,GVAR(save_esp)) 	_; change to C stack
	POPAL								_; Restore C registers
	MOVL	OPRS(GVAR(save_esp),EAX)	_; restore save_esp
')

define(SaveCRegs,`
	MOVL	OPRS(EAX,GVAR(save_esp))	_; save old save_esp by copying to eax
	PUSHAL								_; and saving all registers
	MOVL	OPRS(GVAR(save_esp),ESP)	_; save C sp in save_esp
')

__;
__; The following are from mtypes.m4 (again a candidate for nuking 
__;when we have atrans)
__; Definitions for WAM operations for the M4 files. These should be
__; reflected in the mtypes.h file. In fact, some day these two should
__; be generated from a common file.
__;
__; Written by Keith Hughes	3/10/89
__; Applied Logic Systems, Inc.
__;

__; ANDing MTP_TAGMASK with a word will leave only bits associated with the
__; basic tags.
__;
__: ANDing MTP_REFMASK with a word will leave only the bits making it into
__; a reference address

define(MTP_TAGMASK, 3)
define(MTP_TAGWIDTH,2)
define(MTP_REFMASK,HEX(fffffffc))

define(MTP_UNBOUND,0)
define(MTP_STRUCT,1)
define(MTP_LIST,2)
define(MTP_CONST,3)

__; ANDing this with a word will leave only the constant tags
define(MTP_CONSTMASK, 15)

__; How far to shift a constant word to remove the constant tag
define(MTP_CONSTSHIFT,4)

__; What the tag bit patterns are for constant types
define(MTP_SYM,7)
define(MTP_INT,3)
define(MTP_UIA,15)
define(MTP_FENCE,11)

__; Give the bit which differentiates between the fence/int pair of constants
__; from the uia/atom pair, and the bit which differentiates between UIAs and
__; atoms.
define(MTP_CHARCONSTBIT,2)
define(MTP_UIACONSTBIT,3)

__; Pattern to strip from a functor/arity word to leave just the functor
define(MTP_FUNCTMASK,HEX(fffff0))
define(MTP_ARITYSHIFT,24)

__;
__; Various instructions for tag testing.
__;

__; The tag which can be subtracted to decide the type of item we have.
define(MTP_TAGTEST,2)

__; Jump if it is a structure, list, or something
define(JStruct,JL)
define(JList,JE)
define(JConst,JG)

__; Test a tag for the J instructions found above.
define(TagTest,`
	CMPB	OPRS($1,IMM(MTP_TAGTEST))
')

__;
__; This stuff handles the semicolon flag.
__;

__; What to AND with a cutpoint to get rid of the semicolon flag
define(SEMIMASK,HEX(ef))

__; What to OR with a cutpoint to mark semi-colon flag
define(SEMIFLAG,16)

__; Bit used for the semicolon flag.
define(SEMIBIT,4)

__;
__; MK_INT(dst)
__;
__; Make the integer in dst into a Prolog integer.
__;

define(MK_INT,`
	SHLL	OPRS($1,IMM(MTP_CONSTSHIFT))
	ORL	OPRS($1,IMM(MTP_INT))
')

__;
__; MK_SYM(dst)
__;
__; Make the atom in dst into a Prolog symbol.
__;

define(MK_SYM,`
	SHLL	OPRS($1,IMM(MTP_CONSTSHIFT))
	ORL	OPRS($1,IMM(MTP_SYM))
')

__;
__; MK_FUNCTOR(dst,src)
__;
__; Make a functor word and put it into dst. dst should contain the atom
__; name, while src contains the arity.
__;
__; Destroys the copy of the arity in src
__;

define(MK_FUNCTOR,`
	MK_SYM($1)
	SHLL	OPRS($2,IMM(MTP_ARITYSHIFT))
	ORL	OPRS($1,$2)
')

__;
__; Get the arity from something and put it in a register
__;

define(GetArity,
	`SHRL	OPRS($1,IMM(MTP_ARITYSHIFT))'
)

__;
__; GetFunctor(dst,src)
__;
__; src is a register with a tagged structure. dst will receive the functor
__; of the structure refered to by src
__;

define(GetFunctor,
	`MOVL	OPRS($1,MADDR(-MTP_STRUCT,$2))'
)

__;
__; GetStructAddr(src/dst)
__; GetStructAddr(dst,src)
__;
__; src contains a tagged structure. src/dst will receive a pointer which
__; can be used by the other structure macros.
__;

define(GetStructAddr,`$0$#($*)')

define(GetStructAddr1,`
	SUBL	OPRS($1,IMM(MTP_STRUCT))
')

define(GetStructAddr2,`
	LEA	OPRS($1,MADDR(-MTP_STRUCT,$2))
')

__;
__; GetSubTermN(dst,src,n)
__;
__; src is a StructAddr gotten from GetStructAddr. n gives the number of the
__; subterm desired, which is put into dst. First subterm is numbered 1.
__;

define(GetSubTermN,`
	MOVL	OPRS($1,MADDR($2,$3,4))
')




__;
__; Random stuff for playing with the token table from assembler.
__;
__;EXTRN(tktble,dword)

EXTRN(toktable,dword)

__;
__; TokenName(reg)
__; Get a token table entry.
__;
__; reg has token number.
__;

define(TokenName,`
			__; Multiply reg by size of a token table entry to
			__; get the offset from the beginning of the table.
			__; This is a multiplication by 8.
	SHLL	OPRS($1,IMM(3))

			__; Get base of token table.
			__; ADDL	OPRS($1,OFFSET(GVAR(tktble)))
	ADDL	OPRS($1,GVAR(toktable))

			__; And get the pointer from where we are pointing.
			__; At the moment, the pointer into the string area
			__; is the first object in the structure.
	MOVL	OPRS($1,MADDR($1))
')


__;
__; token.m4
__;
__; Definitions for tokens and symbols. If there is any change in tokens.h
__; or in mtypes.h, this file should be updated accordingly.
__; In fact, some day this file should be produced from those two files.
__;
__; Written by Ilyas Cicekli 	1/6/92
__; Applied Logic Systems, Inc.
__;

__; TKCUT is 12 and symbol tag is 7
define(SYM_CUT, HEX(c7))

