	FILE(choice) 	

__;
__; choice.m4		-- Choice point instructions.
__;	Copyright [c] 1987 Applied Logic Systems, Inc.
__;
__;	This module contains the pieces of the system which are most convenient
__;	to implement directly in assembler.
__;
__; Author: Kevin Buettner
__; Creation: 2/23/87
__;	1/88	kmh	rewritten for 386
__;

include(`assembly.m4')

_dgroup

_assume

EXTRN(next_choice_in_a_deleted_clause,near)

EXTRN(wm_aborted,dword)

_conststart 
_constend   

_datastart

	GLOBAL(wm_b_reg)
DD(GVAR(wm_b_reg),0)

	GLOBAL(c22dat)
DD(GVAR(c22dat),0)

_dataend


_bssstart
_bssend


_textstart

__;
__;
__; wm_try_me
__;
__;	This is the code for the try_me_else instruction.
__;
__;	This code should be called [with call].  It is expected that the
__;	return address on the stack points to a longword with the next
__;	clause address.  The words following this address are the code to
__;	continue at.
__;
__;

PROC(wm_try_me)

		POPL	EAX		_; pop the return address
		MOVL	OPRS(SPB,SP)	_; move SP to SPB
		MOVL	OPRS(HB,H)	_; move H to HB
		SUBL	OPRS(TR,IMM(chpt_SIZE))	_; Make room for chpt

__;		CMPL	OPRS(TR,H)
__;		JBE	DISP(PROCADDR(bad))

		MOVL	OPRS(EBX,GVAR(wm_b_reg))	_; Prev B
		MOVL	OPRS(MADDR(chpt_B,ECX),EBX)
		MOVL	OPRS(MADDR(chpt_SPB,ECX),EDI)	_; SPB
		MOVL	OPRS(MADDR(chpt_HB,ECX),ESI)	_; HB
		MOVL	OPRS(EBX,MADDR(EAX))	_; Next clause
		MOVL	OPRS(MADDR(ECX),EBX)
		MOVL	OPRS(GVAR(wm_b_reg),ECX)  _; set B to its new value
		ADDL	OPRS(EAX,IMM(4))
		JMP	REGDISP(EAX)		_; execute the clause code

ENDPROC(wm_try_me)

__;
__; wm_retry_me
__;

PROC(wm_retry_me)

		MOVL	OPRS(EAX,GVAR(wm_b_reg))	_; Get where to take 
						_; the stack back to
		JMP	SDISP(retry_me2)
						_; Untrailing loop
retry_me1:	MOVL	OPRS(EBX,MADDR(ECX))	_; Get var
		MOVL	OPRS(MADDR(EBX),EBX)	_; And reset it
		ADDL	OPRS(ECX,IMM(4))	_; Point to next entry
retry_me2:	CMPL	OPRS(ECX,EAX)
		JNE	SDISP(retry_me1)

		MOVL	OPRS(EDX,ESI)		_; reset H from HB
		POPL	EAX			_; Get calling address
		MOVL	OPRS(ESP,EDI)		_; Reset SP from SPB
		MOVL	OPRS(EBP,EDI)		_; reset E from SPB
		MOVL	OPRS(EBX,MADDR(EAX))	_; Get next clause address
		MOVL	OPRS(MADDR(ECX),EBX)	_; Put in Choice point
		ADDL	OPRS(EAX,IMM(4))
		JMP	REGDISP(EAX)		_; execute clause code

ENDPROC(wm_retry_me)

__;
__; wm_trust_me
__;

PROC(wm_trust_me)

		MOVL	OPRS(EAX,GVAR(wm_b_reg))	_; Get where to take 
						_; the stack back to
		JMP	SDISP(trust_me2)
						_; Untrailing loop
trust_me1:	MOVL	OPRS(EBX,MADDR(TR))	_; Get var
		MOVL	OPRS(MADDR(EBX),EBX)	_; And reset it
		ADDL	OPRS(TR,IMM(4))		_; Point to next entry
trust_me2:	CMPL	OPRS(TR,EAX)
		JNE	SDISP(trust_me1)

		MOVL	OPRS(H,HB)		_; Reset H
		POPL	EBX			_; And get return address
		MOVL	OPRS(SP,SPB)		_; Set SP from SPB
		MOVL	OPRS(E,SPB)		_; Set E from SPB
		MOVL	OPRS(EAX,MADDR(chpt_B,ECX))	_; Reset B
		MOVL	OPRS(GVAR(wm_b_reg),EAX)
		ADDL	OPRS(TR,IMM(chpt_SIZE))	_; Remove choice point
		MOVL	OPRS(ESI,MADDR(chpt_HB,EAX))	_; Reset HB
		MOVL	OPRS(EDI,MADDR(chpt_SPB,EAX))	_; set SPB
		ANDL	OPRS(EDI,IMM(HEX(fffffffe)))	_; nuke compaction bit

		JMP	REGDISP(EBX)			_; execute clause code

ENDPROC(wm_trust_me)

__;
__; The following routines are used by clause indexing
__;
__; An indexing block of code includes patches of the form
__;
__;	call wm_try
__;	address of try clause
__;	call retry
__;	address of retry clause
__;	..... other retrys like above
__;	call wm_trust
__;	address of trust clause
__;
__; The next_clause address in the choice point created by these routines should
__; be the return address from the call to the wm_try, wm_retry, wm_trust
__; routines after going over the address of the clause to go to.
__;

PROC(wm_try)

	POPL	EAX			_; pop the return address
	MOVL	OPRS(SPB,SP)		_; move SP to SPB
	MOVL	OPRS(HB,H)		_; move H to HB
	SUBL	OPRS(TR,IMM(chpt_SIZE))	_; Make room for chpt

__;	CMPL	OPRS(ECX,EDX)
__;	JBE	DISP(PROCADDR(bad))

	MOVL	OPRS(EBX,GVAR(wm_b_reg))	_; Prev B
	MOVL	OPRS(MADDR(chpt_B,ECX),EBX)
	MOVL	OPRS(MADDR(chpt_SPB,ECX),EDI)	_; SPB
	MOVL	OPRS(MADDR(chpt_HB,ECX),ESI)	_; HB
	LEA	OPRS(EBX,MADDR(5,EAX))	_; Next clause
	MOVL	OPRS(MADDR(ECX),EBX)
	MOVL	OPRS(GVAR(wm_b_reg),ECX)  _; set B to its new value

	JMP	REGDISP(MADDR(1,EAX))	_; execute the clause code

ENDPROC(wm_try)


PROC(wm_retry)

	MOVL	OPRS(EAX,GVAR(wm_b_reg))	_; Get where to take 
						_; the stack back to
	JMP	SDISP(retry2)
						_; Untrailing loop
retry1:
	MOVL	OPRS(EBX,MADDR(ECX))	_; Get var
	MOVL	OPRS(MADDR(EBX),EBX)	_; And reset it
	ADDL	OPRS(ECX,IMM(4))	_; Point to next entry
retry2:
	CMPL	OPRS(ECX,EAX)
	JNE	SDISP(retry1)

	MOVL	OPRS(EDX,ESI)		_; reset H from HB
	POPL	EAX			_; Get calling address
	MOVL	OPRS(ESP,EDI)		_; Reset SP from SPB
	MOVL	OPRS(EBP,EDI)		_; reset E from SPB
	LEA	OPRS(EBX,MADDR(5,EAX))	_; Get next clause address
	MOVL	OPRS(MADDR(ECX),EBX)	_; Put in Choice point

	JMP	REGDISP(MADDR(1,EAX))		_; execute clause code

ENDPROC(wm_retry)


PROC(wm_trust)

	MOVL	OPRS(EAX,GVAR(wm_b_reg))	_; Get where to take 
						_; the stack back to
	JMP	SDISP(trust2)
						_; Untrailing loop
trust1:
	MOVL	OPRS(EBX,MADDR(ECX))	_; Get var
	MOVL	OPRS(MADDR(EBX),EBX)	_; And reset it
	ADDL	OPRS(ECX,IMM(4))	_; Point to next entry
trust2:
	CMPL	OPRS(ECX,EAX)
	JNE	SDISP(trust1)

	MOVL	OPRS(EDX,ESI)		_; Reset H
	POPL	EBX			_; And get return address
	MOVL	OPRS(ESP,EDI)		_; Set SP from SPB
	MOVL	OPRS(EBP,EDI)		_; Set E from SPB
	MOVL	OPRS(EAX,MADDR(chpt_B,ECX))	_; Reset B
	MOVL	OPRS(GVAR(wm_b_reg),EAX)
	ADDL	OPRS(TR,IMM(chpt_SIZE))	_; Remove choice point
	MOVL	OPRS(ESI,MADDR(chpt_HB,EAX))	_; Reset HB
	MOVL	OPRS(EDI,MADDR(chpt_SPB,EAX))	_; set SPB
	ANDL	OPRS(EDI,IMM(HEX(fffffffe)))	_; nuke compaction bit

	JMP	REGDISP(MADDR(1,EBX))		_; execute clause code

ENDPROC(wm_trust)


PROC(wm_trust_fail)

		NOP			_; Fill to correct alignment
		NOP
		CALL	SDISP(PROCADDR(wm_trust))
		NOP
		DD(PROCADDR(wm_fail))

ENDPROC(wm_trust_fail)

__;
__; wm_fail
__;	This is the code to execute upon failure.
__;

PROC(wm_fail)

	Fail

ENDPROC(wm_fail)


__;
__; wm_nciadc
__;       Called to obtain the Next Choice In A Deleted Clause.
__;

PROC(wm_nciadc)
        CALL    PROCADDR(next_choice_in_a_deleted_clause)
        JMP  	REGDISP(EAX)
ENDPROC(wm_nciadc)



__;
__; wm_catch22
__;
__; Implement catch/0.
__;

PROC(wm_catch22)

			_; Get the alternative address
	MOVL	OPRS(EAX,GVAR(wm_b_reg))
	MOVL	OPRS(EAX,DMADDR(EAX))

			_; Put it into the catch variable.
	MOVL	OPRS(GVAR(c22dat),EAX)

			_; And continue on with the next goal.
	Proceed

ENDPROC(wm_catch22)

__;
__; wm_throw
__;
__; Implement throw/0.
__;
__; We are going to sleaze here. We need a register. Since throw works by
__; failure, we'll use the H register (EDX), knowing that it will be reset
__; when the fail happens. We'll also borrow  ESI for the same reason, though
__; it must be restored.
__;
__; This code works under the theory that multiple choice points will be
__; passed over, so at any point in the loop, the registers are not in
__; the proper state. Only at the end points.
__;

PROC(wm_throw)

			__; Get the catch22 value
	MOVL	OPRS(EDX,GVAR(c22dat))

throwentry:

			_; Get the old B value from the wm_regs array.
	MOVL	OPRS(EAX,GVAR(wm_regidx))
	DECL	EAX
	SHLL	OPRS(EAX,IMM(6))
	ADDL	OPRS(EAX,OFFSET(GVAR(wm_regs)))
	MOVL	OPRS(EAX,DMADDR(wm_B,EAX))

			_; Get the current B value
	MOVL	OPRS(ESI,GVAR(wm_b_reg))

			_; Momentarily inflate TR so that the multiple
			_; choice point search case will work.
	SUBL	OPRS(TR,IMM(chpt_SIZE))

tloop1:
			_; Get the B value where we expect it.
	MOVL	OPRS(EBX,ESI)

			_; Move to trail entries
	ADDL	OPRS(TR,IMM(chpt_SIZE))

			_; See if catch22 and alternative match

	CMPL	OPRS(EDX,DMADDR(EBX))
	JNE	SDISP(tnotyet1)

			_; They match. Fail.
	MOVL	OPRS(GVAR(wm_b_reg),EBX)
	MOVL	OPRS(HB,DMADDR(chpt_HB,EBX))
	MOVL	OPRS(SPB,DMADDR(chpt_SPB,EBX))
	ANDL	OPRS(SPB,IMM(HEX(fffffffe)))	_; nuke compaction bit
	JMP	REGDISP(EDX)

tuntrail:
			_; Nudder to untrail

	MOVL	OPRS(ESI,DMADDR(ECX))
	MOVL	OPRS(MADDR(ESI),ESI)
	ADDL	OPRS(ECX,IMM(4))

tnotyet1:
			_; No match yet. Untrail entries (if any).

	CMPL	OPRS(EBX,ECX)
	JA	SDISP(tuntrail)

			_; See if hit one before the last one.
			_; Since there is a dummy choice point, 
			_; We have to stop the choice point before
			_; the lat one --- Ilyas 8/14/89

	MOVL	OPRS(ESI,DMADDR(chpt_B,EBX))
	CMPL	OPRS(DMADDR(chpt_B,ESI),EAX)
	JNE	SDISP(tloop1)

			_; Hit last one. Looks like we'll abort.

	MOVL	OPRS(GVAR(wm_aborted),IMM(1))
	MOVL	OPRS(GVAR(wm_b_reg),EBX)
	MOVL	OPRS(HB,DMADDR(chpt_HB,EBX))
	MOVL	OPRS(SPB,DMADDR(chpt_SPB,EBX))
	ANDL	OPRS(SPB,IMM(HEX(fffffffe)))	_; nuke compaction bit

			_; Get alternative address.

	MOVL	OPRS(EDX,DMADDR(EBX))
	JMP	REGDISP(EDX)

ENDPROC(wm_throw)

__;
__; wm_abort
__;
__; Implement a default abort/0.
__;
__; We are going to sleaze here. We need a register. Since throw works by
__; failure, we'll use the H register (EDX), knowing that it will be reset
__; when the fail happens.
__;

PROC(wm_abort)

			__; 0 is never an alternative address
	MOVL	OPRS(EDX,IMM(0))

			__; Go borrow throw
	JMP	SDISP(throwentry)

ENDPROC(wm_abort)

_textend

_end
