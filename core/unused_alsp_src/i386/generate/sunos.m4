define(EAX,	`%eax')
define(EBX,	`%ebx')
define(ECX,	`%ecx')
define(EDX,	`%edx')
define(ESP,	`%esp')
define(EBP,	`%ebp')
define(ESI,	`%esi')
define(EDI,	`%edi')
define(AL,	`%al')
define(BL,	`%bl')
define(CL,	`%cl')
define(DL,	`%dl')
define(AX,	`%ax')
define(BX,	`%bx')
define(CX,	`%cx')
define(DX,	`%dx')

define(DIV,	`\/')


define(DMADDR,	`MADDR($*)')
define(WMADDR,	`MADDR($*)')
define(BMADDR,	`MADDR($*)')
define(MADDR,	`$0$#($*)')
define(MADDR1,	`($1)')
define(MADDR2,	`$1($2)')
define(MADDR3,	`($1,$2,$3)')
define(MADDR4,	`$1($2,$3,$4)')


define(OFFSET,	`$$1')

define(PROCADDR,	`FNAMES($1)')

define(GVAR,	`FNAMES($1)')

define(DISP,	`$1')
define(REGDISP,	`*$1')
define(SDISP,	`$1')

define(IMM,	`$$1')
define(HEX,	`0x$1')
define(OCT,	`0$1')
define(BIN,	`0b$1')

define(EXPR,	`[$1]')

define(DB,	`.byte	$1')
define(DW,	`.word	$1')
define(DD,	`$0$#($*)')
define(DD1,	`.long	$1')
define(DD2,	`$1:	.long	$2')

define(DUPDB, 	`	.comm	$1,$2')

define(COMM,	`	.lcomm 	$1,4')

define(GLOBAL,	`.globl	FNAMES($1)')

define(EQU,	`$1 = $2')

define(ALIGN4,	`.align	4')



define(FILE,`.file	"$1.s"')

define(EXTRN,`')

define(_dgroup,`')

define(_assume,`')

define(_datastart,`	.data')
define(_dataend,`')

define(_bssstart,`	.bss')
define(_bssend,`')

define(_conststart,`')
define(_constend,`')

define(_textstart,`	.text')
define(_textend,`')

define(_end,`')


define(PROC,`
	.align	4
	.globl	FNAMES($1)
FNAMES($1):')

define(ENDPROC,`')

define(OPRS,	`$2,$1')

define(ANDB,	`andb')
define(ANDW,	`andw')
define(ANDL,	`andl')
define(TESTB,	`testb')
define(TESTW,	`testw')
define(TESTL,	`testl')
define(BTL,	`btl')
define(CMPL,	`cmpl')
define(CMPW,	`cmpw')
define(CMPB,	`cmpb')
define(SHRL,	`shrl')
define(JA,	`ja')
define(JE,	`je')
define(JC,	`jb')
define(JG,	`jg')
define(JL,	`jl')
define(JZ,	`jz')
define(JNB,	`jnb')
define(JNC,	`jnb')
define(JNE,	`jne')
define(JMP,	`jmp')
define(LEA,	`lea')
define(MOVB,	`movb')
define(MOVW,	`movw')
define(MOVL,	`movl')
define(NEGL,	`negl')
define(POPAL,	`popal')
define(POPL,	`popl')
define(PUSHL,	`pushl')
define(PUSHAL,	`pushal')
define(XCHGL,	`xchgl')
define(SUBL,	`subl')
define(SUBB,	`subb')
define(ADDL,	`addl')
define(ADDB,	`addb')
define(LOOP,	`loop')
define(CALL,	`call')
define(JB,	`jb')
define(JGE,	`jge')
define(JBE,	`jbe')
define(JAE,	`jae')
define(BTRL,	`btrl')
define(ORL,	`orl')
define(ORB,	`orb')
define(SHLL,	`shll')
define(INCL,	`incl')
define(DECL,	`decl')
define(RET,	`ret')
define(NOP,	`nop')
define(REPE,	`repz
		$1')
define(REP,	`rep
		$1')
define(CMPSD,	`cmps')
define(MOVSD,	`movs')
define(CLD,	`cld')
define(STD,	`std')

