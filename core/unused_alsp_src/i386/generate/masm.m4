__;
__; masm.m4
__;	Specific assembler macro file for MASM assembler.
__;
__; Written by Ilyas Cicekli 2/89
__; Applied Logic Systems, Inc.
__;

__;
__; Register definitions
__;

define(EAX,	`eax')
define(EBX,	`ebx')
define(ECX,	`ecx')
define(EDX,	`edx')
define(ESP,	`esp')
define(EBP,	`ebp')
define(ESI,	`esi')
define(EDI,	`edi')
define(AL,	`al')
define(BL,	`bl')
define(CL,	`cl')
define(DL,	`dl')
define(AX,	`ax')
define(BX,	`bx')
define(CX,	`cx')
define(DX,	`dx')

define(DIV,	`/')


define(DMADDR,	`dword ptr MADDR($*)')
define(WMADDR,	`word ptr MADDR($*)')
define(BMADDR,	`byte ptr MADDR($*)')
define(MADDR,	`$0$#($*)')
define(MADDR1,	`[$1]')
define(MADDR2,	`$1[$2]')
define(MADDR3,	`[$1+$2*$3]')
define(MADDR4,	`$1[$2+$3*$4]')

define(OFFSET,	`offset $1')

define(PROCADDR,	`FNAMES($1)')

define(GVAR,	`FNAMES($1)')

define(DISP,	`$1')
define(REGDISP,	`$1')
define(SDISP,	`short $1')

define(IMM,	`$1')
define(HEX,	`0$1h')
define(OCT,	`$1o')
define(BIN,	`$1b')

define(EXPR,	`($1)')

define(DB,	`db	$1')
define(DW,	`dw	$1')
define(DD,	`$0$#($*)')
define(DD1,	`dd	$1')
define(DD2,	`$1	dd	$2')

define(DUPDB, 	`$1 	db 	$2 dup(?)')

define(COMM,	`$1	dd	?')

define(GLOBAL,	`public	FNAMES($1)')

define(EQU,	`$1	equ	$2')

define(ALIGN4,	`align	4')




define(FILE,`TITLE	$$1
	.386')

define(EXTRN,`extrn	FNAMES($1):$2')

define(_dgroup,`DGROUP	GROUP CONST, _BSS, _DATA')

define(_assume,`ASSUME	CS:_TEXT, DS:DGROUP, SS:DGROUP, ES:DGROUP')

define(_datastart,`_DATA	SEGMENT DWORD USE32 PUBLIC ''DATA')
define(_dataend,`_DATA	ENDS')

define(_bssstart,`_BSS	SEGMENT	DWORD USE32 PUBLIC ''BSS')
define(_bssend,`_BSS	ENDS')

define(_conststart,`CONST	SEGMENT DWORD USE32 PUBLIC ''CONST')
define(_constend,`CONST	ENDS')

define(_textstart,`_TEXT	SEGMENT DWORD USE32 PUBLIC ''CODE')
define(_textend,`_TEXT	ends')

define(_end,`END')


define(PROC,`
	align	4
	public	FNAMES($1)
FNAMES($1)	proc	near')

define(ENDPROC,`FNAMES($1)	endp')

define(OPRS,	`$1,$2')

define(REPE,	`repe	$1')
define(REP,	`rep	$1')
define(CMPSD,	`cmpsd')
define(MOVSD,	`movsd')
define(CLD,	`cld')
define(STD,	`std')
define(MOVB,	`mov')
define(MOVW,	`mov')
define(MOVL,	`mov')
define(ANDB,	`and')
define(ANDW,	`and')
define(ANDL,	`and')
define(TESTB,	`test')
define(TESTW,	`test')
define(TESTL,	`test')
define(BTL,	`bt')
define(JNE,	`jne')
define(JNB,	`jnb')
define(CMPL,	`cmp')
define(CMPW,	`cmp')
define(CMPB,	`cmp')
define(SHRL,	`shr')
define(JMP,	`jmp')
define(JG,	`jg')
define(JE,	`je')
define(JA,	`ja')
define(JL,	`jl')
define(JZ,	`jz')
define(POPL,	`pop')
define(PUSHL,	`push')
define(PUSHAL,	`pushad')
define(POPAL,	`popad')
define(LEA,	`lea')
define(NEGL,	`neg')
define(XCHGL,	`xchg')
define(SUBL,	`sub')
define(SUBB,	`sub')
define(ADDL,	`add')
define(ADDB,	`add')
define(LOOP,	`loop')
define(CALL,	`call')
define(JB,	`jb')
define(JGE,	`jge')
define(JBE,	`jbe')
define(JAE,	`jae')
define(JNC,	`jnc')
define(JC,	`jc')
define(BTRL,	`btr')
define(ORL,	`or')
define(ORB,	`or')
define(SHLL,	`shl')
define(INCL,	`inc')
define(DECL,	`dec')
define(RET,	`ret')
define(NOP,	`nop')

