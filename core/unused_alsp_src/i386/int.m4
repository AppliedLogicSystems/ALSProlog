	FILE(int) 	

__;
__; int.m4	-- Interrupt handling.
__;	Copyright [c] 1987 Applied Logic Systems, Inc.
__;
__;	This module contains the pieces of the system which are most convenient
__;	to implement directly in assembler.
__;
__; Author: Keith Hughes
__; Creation: 1/88
__;

include(`assembly.m4')

_dgroup

_assume

EXTRN(gc,near)
EXTRN(decr_icount,near)

EXTRN(wm_overcode,dword)
EXTRN(wm_normal,dword)
EXTRN(wm_heapbase,dword)
EXTRN(wm_interrupt_caught,dword)
EXTRN(wm_safety,dword)
EXTRN(wm_trigger,dword)
EXTRN(wm_spying,dword)
EXTRN(wm_b_reg,dword)

_conststart 
_constend   


_datastart

__;
__; This is set to wm_overflowgc to have GC done by the overflow code
__; and wm_overflow to have it under user control.
__;

	GLOBAL(OverflowPtr)
DD(GVAR(OverflowPtr),PROCADDR(wm_overflowgc))

DD(overflow_tmp0,0)
DD(overflow_tmp1,0)
DD(overflow_tmp2,0)
DD(overflow_tmp3,0)

_dataend


_bssstart
_bssend


_textstart

__;
__; dbg_spycheck:
__; 
__; We jump to this procedure from the name table entry of a spied procedure.
__; If the a global flag "wm_spying" is set, we move "wm_trigger" to
__; to "wm_safety" to trigger an interrupt. 
__; Since the name table entry of a spied procedure is changed to
__; jump procedure, we check the heap overflow in this procedure before
__; we return the calling name table entry.
__; We know that the next instruction following the call instruction
__; to this procedure is a conditional jump instruction to the overflow 
__; entry of that name table entry.
__;

PROC(dbg_spycheck)

	__; Check the global flag "wm_spying" to trigger an interrupt or not.

	CMPL	OPRS(GVAR(wm_spying),IMM(1))
	JNE 	SDISP(checkoverflow)		_; don't trigger an interrupt
	MOVL 	OPRS(EBX,GVAR(wm_trigger))	_; trigger an interrupt
	MOVL 	OPRS(GVAR(wm_safety),EBX)

checkoverflow:
	__; Check the heap overflow

	MOVL 	OPRS(EBX,TR) 				_; Get TR
	SUBL	OPRS(EBX,H) 				_; Subtract H
	CMPL 	OPRS(EBX,GVAR(wm_safety))	_; See if we are triggered

	__; Now flags are affected. 
	__; Return to the conditional jump
	__; instruction in the name table entry to decide whether we jump
	__; to the overflow entry of that name table or not.

	RET

ENDPROC(dbg_spycheck)



PROC(dbg_decr_icount)
	callCProc(decr_icount)				_; Return address used as argument
	MOVL 	OPRS(EBX,TR) 				_; Get TR
	SUBL	OPRS(EBX,H) 				_; Subtract H
	CMPL 	OPRS(EBX,GVAR(wm_safety))	_; See if we are triggered
	JBE		SDISP(do_ovflow)
	RET				_; return to the conditional branch

do_ovflow:
	SUBL	OPRS(DMADDR(ESP),IMM(eval(NTBL_ENTRYSIZE-NTBL_HEADERSIZE)))
	JMP		PROCADDR(wm_overflowgc)

ENDPROC(dbg_decr_icount)


__;
__; wm_overflowgc:
__;
__; Handle Prolog exceptions with gc called if that is what really caused
__; the interrupt.
__;
__; We assume the value to check against is in EBX
__;

PROC(wm_overflowgc)

	__; See if actually a GC interrupt

	CMPL	OPRS(EBX,GVAR(wm_normal))

			__; If not, process the interrupt as normal.

	JA	PROCADDR(wm_overflow)

			__; Gotta do a GC
			__; can't use wm_execute_builtin since it proceeds out

	SavePrologRegs

	CALL	PROCADDR(gc)

	LoadPrologRegs

			__; We are done. Go back. We assume that we will
			__; be popping back into the procedure entry
			__; and check for an interrupt again in case
			__; there was a real one. This isn't good for
			__; real-time stuff.

	POPL	EAX

			__; NTBL_CALLENTRYSIZE+2.
			__; The +2 assumes that the instruction before
			__; the overflow check will be in effect a NOP
			__; when jumped to from here. So we ignore it
			__; to save a whole 2 clocks (whoop, whoop).

	ADDL	OPRS(EAX,IMM(3))

	JMP	REGDISP(EAX)

ENDPROC(wm_overflowgc)

__;
__; wm_overflow: Handle Prolog exceptions.
__;

__; Where from the interrupt return address the top of the name table entry
__; resides.
define(topentry,`eval($1-NTBL_HEADERSIZE-NTBL_OVERFLOWSIZE)')

PROC(wm_overflow)

	MOVL 	OPRS(EAX,GVAR(wm_normal))
	MOVL	OPRS(GVAR(wm_safety),EAX)	_; Turn off interrupts

	POPL	EAX							_; Get interrupt return address
	POPL	overflow_tmp0				_; Get Old E
	POPL	overflow_tmp1				_; Get return address

	__; Now we reach into the name table entry since that is where the
	__; return addr points to. NTBL_HEADERSIZE+NBTL_OVERFLOWSIZE bytes
	__; back is the beginning of the proc entry.

			_; Get modid, strip off extra garbage, turn into a
			_; Prolog symbol, and save away.

	MOVW	OPRS(BX,WMADDR(topentry(NTBL_MODID),EAX))
	ANDL	OPRS(EBX,IMM(HEX(ffff)))
	MK_SYM(EBX)
	MOVL	OPRS(overflow_tmp2,EBX)

			_; Get arity from narg entry.

	MOVW	OPRS(BX,WMADDR(topentry(NTBL_NARGS),EAX))
	ANDL	OPRS(EBX,IMM(HEX(ffff)))	_; Strip off extra garbage

	JNE	SDISP(createrm)					_; If non-zero arity, must make term

			_; A2 is f/a.

	PUSHL	MADDR(topentry(NTBL_TOKID_ARITY),EAX)
	PUSHL	overflow_tmp2						_; A1 is module id.
	MOVL	OPRS(EAX,GVAR(wm_interrupt_caught))	_; A0 is wm_interrupt_caught
	MK_INT(EAX)									_; make it a Prolog integer
	PUSHL	EAX
	MOVL	OPRS(GVAR(wm_interrupt_caught),IMM(0))	_; clear wm_interrupt_caught

	PUSHL	overflow_tmp1		_; Put return address where should be.
	PUSHL	overflow_tmp0		_; Put old e where should be.

			_; Go do the interrupt code

	MemExecute(GVAR(wm_overcode))

			__; We must build up an actual term

createrm:

	MOVL	OPRS(overflow_tmp3,EBX)	_; Save the arg count

			__; Save the actual arity

	PUSHL	EBX

			__; Get the functor/arity, strip off old arity,
			__; and put nargs arity in. This is so cut
			__; macros work out.

	MOVL	OPRS(EAX,MADDR(topentry(NTBL_TOKID_ARITY),EAX))
	ANDL	OPRS(EAX,IMM(MTP_FUNCTMASK))
	SHRL	OPRS(EAX,IMM(MTP_CONSTSHIFT))
	MK_FUNCTOR(EAX,EBX)

			__; Put F/A at beginning of structure and increment
			__; heap pointer.

	MOVL	OPRS(MADDR(H),EAX)
	ADDL	OPRS(H,IMM(4))

			__; Get arity back.

	POPL	EBX

creloop:
	POPL	MADDR(H)				_; Get argument and put on heap.
	ADDL	OPRS(H,IMM(4))			_; Increment heap pointer.
	DECL	EBX						_; One less to worry about
	JNE	SDISP(creloop)				_; Keep going if more

	MOVL	OPRS(EBX,overflow_tmp3)	_; Get count back

			__; A2 is structure built on heap.

	NEGL	EBX						_; We want a negative count.
									_; -4 for f/a, +MTP_STRUCT for tag.
	LEA	OPRS(EAX,MADDR(eval(MTP_STRUCT-4),EDX,EBX,4))
	PUSHL	EAX

	PUSHL	overflow_tmp2			_; A1 is module id
					
	MOVL	OPRS(EAX,GVAR(wm_interrupt_caught))	_; A0 is wm_interrupt_caught
	MK_INT(EAX)									_; make it a Prolog integer
	PUSHL	EAX
	MOVL	OPRS(GVAR(wm_interrupt_caught),IMM(0))	_; clear wm_interrupt_caught

	PUSHL	overflow_tmp1			_; Put return address where it belongs.
	PUSHL	overflow_tmp0			_; Put Old E where it belongs.

	MOVL	OPRS(E,SP)				_; Update E register

			__; Now we are going to scan from A0 to An doing a put_unsafe

	PUSHL	ESI						_; We need a register.
	PUSHL	EDX						_; I lied. We need two.

	MOVL	OPRS(ESI,EBX)			_; Copy the count where we need it

			__; Move to next argument and dereference it.

creloop2:
	MOVL	OPRS(EAX,MADDR(EDX,ESI,4))
	Deref(EBX,EAX,AL,clok,clderef)

			__; Got a variable. See if on heap.

	CMPL	OPRS(EBX,GVAR(wm_heapbase))
	JA	SDISP(clok)					_; If on heap, no need to check

	POPL	EBX						_; Get H from stack and make variable
	MOVL	OPRS(MADDR(EBX),EBX)
	ADDL	OPRS(EBX,IMM(4))		_; And push new H
	PUSHL	EBX
	SUBL	OPRS(EBX,IMM(4))		_; And fix EBX [yech]

	MOVL	OPRS(MADDR(EAX),EBX)	_; Store into environment

	CMPL	OPRS(SPB,EAX)			_; See if must trail
	JA	SDISP(clok)					_; No need to trail

	Trail(EAX)						_; Trail the bugger
	
clok:
	MOVL	OPRS(MADDR(EDX,ESI,4),EBX)	_; Put final object back

	INCL	ESI						_; One less to do
	JNE	SDISP(creloop2)				_; Go again if more

	POPL	EDX						_; We want our EDX [not MTV]
	POPL	ESI						_; Get ESI back before call

			__; Finally we can call the code

	JMP	REGDISP(GVAR(wm_overcode))

ENDPROC(wm_overflow)

_textend

_end

