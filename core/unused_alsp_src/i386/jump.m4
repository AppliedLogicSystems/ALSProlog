	FILE(jump) 

__;
__; jump.m4		-- The jump/2 evaluatable.
__;
__; Written by Keith Hughes
__;		3/20/89
__;

include(`assembly.m4')

EXTRN(jump_validate_dbref,near)

EXTRN(wm_b_reg,dword)
EXTRN(wm_trigger,dword)
EXTRN(wm_safety,dword)
EXTRN(wm_heapbase,dword)

_dgroup

_assume

_datastart
_dataend

_bssstart
_bssend

_conststart
_constend

_textstart


__;
__; Coming into jump/2, the top stack frame is
__;
__;	|	........................	|
__;	|---------------------------------------|
__;	| Call structure (term being called) 	|
__;	|---------------------------------------|
__;	| DBRef for clause being jumped to	|
__;	|---------------------------------------|
__;	| Return address for call		|
__;	|---------------------------------------|
__;	| Old Environment			|
__;	|---------------------------------------|
__;

__;
__; wm_jump: This is a two argument call predicate. The DBRef and term
__; representing the call to be made are passed in and a jump_validate_dbref is
__; done.
__;

__; Number of arguments used by jump/2
define(ProcArity,2)

PROC(wm_jump)

			_; Get the call structure and dereference it.
	GetArgN(EAX,2)
	Deref(EBX,EAX,AL,callgrnd,drfcll)

			_; Blew it. Call structure is variable.
	Fail

callgrnd:
	PutArgN(2,EBX)	_; Put back in frame for later use.
__;	PUSHL	IMM(0)	_; Save room for return address.
	PUSHL	EBX	_; Save the call structure.

			_; Get the dbref structure and dereference it.
	GetArgN(EAX,1)
	Deref(EBX,EAX,AL,dbrefgrnd,drfdbref)

			_; Fail if it is a variable
dbreffail:
	Fail

dbrefgrnd:
	CMPB	OPRS(AL,IMM(MTP_STRUCT))	_; See if structure
	JNE	SDISP(dbreffail)	_; And fail if not

_;	ANDL	OPRS(EBX,IMM(MTP_REFMASK))
	PUSHL	EBX	_; Put argument on stack

			__; We've now placed the arguments down for a call
			__; to jump_validate_dbref on the stack. Make the
			__; call.
	callCProc(jump_validate_dbref)
	ADDL	OPRS(ESP,IMM(8))

			__; Fail if we got a 0 back.
	CMPL	OPRS(CReturn,IMM(0))
	JE	dbreffail

			__; Save the jump address where DBRef was (we don't
			__; need the DBRef anymore.
	PutArgN(1,CReturn)

			__; Get the term back and make a copy.
	GetArgN(EAX,2)
	MOVL	OPRS(EBX,EAX)

			__; Check the tag to see if any args.
	ANDB	OPRS(AL,IMM(MTP_TAGMASK))
	TagTest(AL)
	JStruct	jumpwargs

			__; Get address to transfer control to
	GetArgN(EAX,1)

			__; Put Old E and return address into a 0 arg frame.
	POPL	DMADDR(8,E)
	POPL	DMADDR(12,E)

			__; And jump to the clause
	MOVL	OPRS(E,SP)
	JMP	REGDISP(EAX)

jumpwargs:
			__; Save room on stack for all of the arguments needed
	GetFunctor(EBX,EBX)
	GetArity(EBX)
	NEGL	EBX
	LEA	OPRS(SP,MADDR(eval(ProcArity*4-4),E,EBX,4))
	NEGL	EBX

			__; Copy OldE, jump addr, and the return address.
			__;
			__; This coding is opaque since we are trying to
			__; avoid having to put the jump addr in a funny
			__; place when we have only one argument in the
			__; goal.
	MOVL	OPRS(EAX,MADDR(E))
	MOVL	OPRS(MADDR(SP),EAX)
	PUSHL	ArgN(1)
	MOVL	OPRS(EAX,MADDR(4,E))
	MOVL	OPRS(MADDR(8,SP),EAX)

			__; Need another register. Make it so we can use auto-
			__; decrement.
	PUSHL	ECX
	MOVL	OPRS(ECX,EBX)

			__; Get address of the term so we can copy it
	GetArgN(EAX,2)
	GetStructAddr(EAX)

copyloop:
	GetSubTermN(EBX,EAX,ECX)
	MOVL	OPRS(MADDR(12,SP,ECX,4),EBX)
	LOOP	SDISP(copyloop)

			__; Get ECX back and jump to the clause.
	POPL	ECX
	LEA	OPRS(E,MADDR(4,SP))
	RET

ENDPROC(wm_jump)

undefine(`ProcArity')

_textend

_end
 
