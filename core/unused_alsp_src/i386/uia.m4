	FILE(uia) 

__;
__; uia.m4		-- UIA handling routines
__;
__; Written by Keith Hughes
__;		3/30/89
__;

include(`assembly.m4')

EXTRN(wm_b_reg,dword)
EXTRN(wm_heapbase,dword)
EXTRN(from_w_unify,dword)

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
__; uiacopy:
__;	Copy a UIA
__;
__; Destroys EBX
__;
__; In:
__;	EAX	pointer to first word of UIA structure in code
__;
__; Out:
__;	EAX	pointer to end of UIA structure in code.
__;

PROC(uiacp)

uiacopy:
			__; Need a register
	PUSHL	ECX

			__; Get the number of words to copy and save it.
	MOVL	OPRS(ECX,MADDR(EAX))
	PUSHL	ECX

			__; Copy the items from the code area to the heap
ucopyloop:
	MOVL	OPRS(EBX,MADDR(EAX,ECX,4))
	MOVL	OPRS(MADDR(-4,H,ECX,4),EBX)
	LOOP	SDISP(ucopyloop)

			__; Adjust EAX and H
	POPL	ECX
	LEA	OPRS(H,MADDR(H,ECX,4))
	LEA	OPRS(EAX,MADDR(4,EAX,ECX,4))

			__; Get the regsiter we borrowed back
	POPL	ECX

			__; And go back.
	RET

ENDPROC(uiacp)

__;
__; wm_p_uia:
__;
__; Copy the UIA found after the return address on the stack and copy it onto
__; the heap. Then make a UIA object and return it.
__;
__; The UIA object is returned in EAX
__;

PROC(wm_p_uia)

			__; Get the return address (which points at the number
			__; of long words to copy.
	POPL	EAX

			__; Make the UIA pointer and save it.
	MOVL	OPRS(EBX,H)
	SUBL	OPRS(EBX,GVAR(wm_heapbase))
	SHLL	OPRS(EBX,IMM(MTP_CONSTSHIFT))
	ORL	OPRS(EBX,IMM(MTP_UIA))
	PUSHL	EBX

			__; Copy the bugger
	CALL	SDISP(uiacopy)

			__; Get the answer
	POPL	EBX

	JMP	REGDISP(EAX)

ENDPROC(wm_p_uia)

__;
__; wm_g_uia:
__;
__; Copy the UIA found after the return address on the stack and copy it onto
__; the heap. Then make a UIA object and return it.
__;
__; The item to be examined better be in EAX
__;

PROC(wm_g_uia)

			__; Deref the sucker
	Deref(EBX,EAX,AL,uiagrnd,guialoop)

			__; It was variable. Our life is a lot easier.

			__; The variable we will be binding is based on
			__; the current H value. DO the binding and the
			__; trail check, and then copy the UIA. Simpler
			__; that way.

			__; Make the UIA pointer.
	MOVL	OPRS(EAX,H)
	SUBL	OPRS(EAX,GVAR(wm_heapbase))
	SHLL	OPRS(EAX,IMM(MTP_CONSTSHIFT))
	ORL	OPRS(EAX,IMM(MTP_UIA))
	
			__; Do the binding and, if necessary, trailing.
	Bind(EBX,EAX)
	CMPL	OPRS(HB,EBX)
	JBE	SDISP(notrail)
	CMPL	OPRS(SPB,EBX)
	JA	SDISP(notrail)
	Trail(EBX)

notrail:
			__; Get address of beginning of UIA
	POPL	EAX

			__; Copy the bugger
	CALL	SDISP(uiacopy)

			__; And go home
	JMP	REGDISP(EAX)
	
			__; Bastard was ground. Now we have a lot of work.
uiagrnd:

	CMPB	OPRS(AL,IMM(MTP_CONST))
	JE	SDISP(isconst)

			__; Not a constant of any type. Die.
uiafail:
	Fail

			__; Got a constant of some type. Differentiate between
			__; UIA/atom and others.

isconst:
	BTL	OPRS(EBX,IMM(MTP_CHARCONSTBIT))
	JNC	SDISP(uiafail)	

	BTL	OPRS(EBX,IMM(MTP_UIACONSTBIT))
	JC	SDISP(uiauiacmp)

			__; Got to compare an atom against a UIA

			__; Get the token ID and then a pointer to the 
			__; string representing the name.
	SHRL	OPRS(EBX,IMM(MTP_CONSTSHIFT))
	TokenName(EBX)

			__; Move back 1 byte for loop below
	DECL	EBX
			__; Get UIA in name space
	MOVL	OPRS(EAX,MADDR(SP))

			__; Put size from name space onto stack
	PUSHL	MADDR(EAX)

			__; Point just before the first byte in the UIA
	ADDL	OPRS(EAX,IMM(7))

			__; Need an extra register since can't do a memory
			__; to memory compare on the 386
	PUSHL	ECX

symuialoop:
	INCL	EAX
	INCL	EBX

	MOVB	OPRS(CL,MADDR(EAX))
	CMPB	OPRS(CL,MADDR(EBX))
	JNE	SDISP(gonnafail)

	CMPB	OPRS(CL,IMM(0))
	JNE	SDISP(symuialoop)

			__; It war equal. Succeed.
	POPL	ECX
	POPL	EBX
	POPL	EAX

	LEA	OPRS(EAX,MADDR(4,EAX,EBX,4))
	JMP	REGDISP(EAX)

gonnafail:
	POPL	ECX
	Fail

			__; Wow. Gotta compare the UIA referenced by EBX with
			__; the UIA found at the location pointed at by the
			__; value at the top of the heap.
uiauiacmp:

			__; Get a pointer to the UIA on the heap.
	SHRL	OPRS(EBX,IMM(MTP_CONSTSHIFT))
	ADDL	OPRS(EBX,GVAR(wm_heapbase))

			__; Get the pointer to the UIA in code
	POPL	EAX

			__; Save ECX, get the size of the UIA on the stack
			__; and save it on the stack.
	PUSHL	ECX
	MOVL	OPRS(ECX,MADDR(EAX))
	ADDL	OPRS(EAX,IMM(4))

			__; Going to use the string compare instructions. Set
			__; up the right register usage
	XCHGL	OPRS(EAX,EDI)
	XCHGL	OPRS(EBX,ESI)

			__; Compare the strings as long words.
	REPE(CMPSD)

			__; Put the registers back the way they should be.
			__; These operations do not effect the flags set
			__; by the CMPSD
	XCHGL	OPRS(EAX,EDI)
	XCHGL	OPRS(EBX,ESI)

			__; Get the objects off of the stack.
	POPL	ECX
	
			__; If not equal, die scum pig.
	JNE	SDISP(uiafail)

			__; Calculate return address and go back.
	JMP	REGDISP(EAX)

ENDPROC(wm_g_uia)

__;
__; wm_g_sym
__;
__; getsymbol instruction
__;
__; Top of stack should contain the symbol to be compared, while
__; EAX has the item to be dereferenced.
__;

PROC(wm_g_sym)

	Deref(EBX,EAX,AL,gndgsym,gsymloop)

			__; Gotta variable. Bind it with the sym on top of the
			__; stack. Trail if necessary.
	MOVL	OPRS(EAX,MADDR(4,SP))
	Bind(EBX,EAX)
	CMPL	OPRS(HB,EBX)
	JBE	SDISP(gsymok)
	CMPL	OPRS(SPB,EBX)
	JA	SDISP(gsymok)
	Trail(EBX)

gsymok:

			__; Get the return address and the constant
	RET

gndgsym:
	CMPL	OPRS(EBX,MADDR(4,SP))
	JE	SDISP(gsymok)

			__; See if we're pointing at a UIA
	MOVL	OPRS(EAX,EBX)
	ANDL	OPRS(EAX,IMM(MTP_CONSTMASK))
	CMPB	OPRS(AL,IMM(MTP_UIA))
	JNE	SDISP(gsymfail)

			__; Yep. Gotta UIA. Now we have tons of work. Got to
			__; compare an atom against a UIA. However, we farm
			__; it out.
	MOVL	OPRS(EAX,MADDR(4,SP))
	JMP	SDISP(PROCADDR(symuiacmp))

gsymfail:
	Fail

ENDPROC(wm_g_sym)

__;
__; wm_u_sym
__;
__; Wam UNIFY_SYM instruction.
__;
__; Arguments
__; const on stack 1 word above the return address, which is TOS
__; EAX: Pointer to item to be dereferenced and examined.
__;
__; We use Unbound() because EAX looks like a variable. It saves a tag check
__; that will probably not usually be necessary if EAX is a variable with its
__; home in a term. This assumes that usym in read mode has variables in it,
__; not constants.
__;
__; The above is a Kevin optimization. This code isn't as optimal as
__; his, but I wanted to share some code with wm_g_sym and the unifier.
__;

PROC(wm_u_sym)

			__; If unbound, go immediately.
	Unbound(EBX,EAX,SDISP(varusym))

	Deref(EBX,EAX,AL,gndusym,usymloop)

			__; Ooh, ooh. A variable. Simple case.
varusym:
			__; Get constant from stack, bind, and trail if needed.
			__; Only need to do trail check on heap since the
			__; variable came from a structure and structures 
			__; don't point onto the stack.
	MOVL	OPRS(EAX,MADDR(4,SP))
	Bind(EBX,EAX)
	CMPL	OPRS(HB,EBX)
	JBE	SDISP(usymok)
	Trail(EBX)

usymok:
	RET

gndusym:
			__; It is ground. See what we got. If equal, succeed.
	CMPL	OPRS(EBX,MADDR(4,SP))
	JE	SDISP(usymok)

			__; See if we have a UIA.
	MOVL	OPRS(EAX,EBX)
	ANDB	OPRS(AL,IMM(MTP_CONSTMASK))
	CMPB	OPRS(AL,IMM(MTP_UIA))
	JNE	SDISP(usymfail)

			__; Load up EAX with the tagged
			__; constant on the stack and go to symuiacmp.
			__; since they are the same.
	MOVL	OPRS(EAX,MADDR(4,SP))
	JMP	SDISP(PROCADDR(symuiacmp))

usymfail:
	Fail
	
ENDPROC(wm_u_sym)

__;
__; symuiacmp
__;
__; Compare the tagged constant in EAX with the tagged UIA in EBX. If
__; we succeed, we'll jump to the address on the top of the stack. Otherwise,
__; we'll fail.
__;

PROC(symuiacmp)

			__; Get a pointer to the UIA on the heap.
	SHRL	OPRS(EBX,IMM(MTP_CONSTSHIFT))
	ADDL	OPRS(EBX,GVAR(wm_heapbase))

			__; Point just before the first byte in the UIA
	ADDL	OPRS(EBX,IMM(3))


			__; Get the token ID and then a pointer to the 
			__; string representing the name.
	SHRL	OPRS(EAX,IMM(MTP_CONSTSHIFT))
	TokenName(EAX)

			__; Move back 1 byte for loop below
	DECL	EAX

			__; Need an extra register since can't do a memory
			__; to memory compare on the 386
	PUSHL	ECX

			__; Compare the two strings, jumping to failure if
			__; they are ever not equal, and stopping when
			__; one gets a null byte. Since this check is done
			__; after the equivalence check, we know the other
			__; string is done also.
symuiacmploop:
	INCL	EAX
	INCL	EBX

	MOVB	OPRS(CL,MADDR(EAX))
	CMPB	OPRS(CL,MADDR(EBX))
	JNE	SDISP(symuiacmpfail)

	CMPB	OPRS(CL,IMM(0))
	JNE	SDISP(symuiacmploop)

			__; It was equal. Succeed.
	POPL	ECX
	MOVL	OPRS(EAX,IMM(1))		_; return success
	RET

symuiacmpfail:
	POPL	ECX

	CMPL	OPRS(GVAR(from_w_unify),IMM(1))
	JE		SDISP(symuiafail_return)

	Fail

symuiafail_return:
	MOVL	OPRS(EAX,IMM(0))		_; return failure
	RET

ENDPROC(symuiacmp)

_textend

_end

