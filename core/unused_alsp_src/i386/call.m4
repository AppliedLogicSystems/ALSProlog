	FILE(call) 

__;
__; call.s		-- The interpreter evaluatables
__;
__; Written by Keith Hughes
__;		3/28/88
__;

include(`assembly.m4')

EXTRN(wm_try_me,near)
EXTRN(wm_trust_me,near)
EXTRN(call_resolve_reference,near)
EXTRN(docut,near)

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
__; Coming into colon/2, the top stack frame is
__;
__;	|	........................			|
__;	|---------------------------------------|
__;	|	cutpt								|
__;	|---------------------------------------|
__;	| Call structure (term being called) 	|
__;	|---------------------------------------|
__;	| Module								|
__;	|---------------------------------------|
__;	| Return address for call				|
__;	|---------------------------------------|
__;	| Old Environment						|
__;	|---------------------------------------|
__;

__;
__; wm_colon: This is a two argument call predicate. The module and term
__; representing the call to be made are passed in and a resolve_ref is
__; done.
__;

PROC(wm_colon)

	MOVL	OPRS(EAX,MADDR(8,ESP))		_; Get the module atom and dereference it.
	Deref(EBX,EAX,AL,modgrnd,colondrfmod)

colonfail:								_; Fail if it is a variable
	Fail

modgrnd:
	MOVL	OPRS(EAX,EBX)				_; Got to examine tag.
	ANDB	OPRS(AL,IMM(MTP_CONSTMASK)) _; Nuke all but the tag.
	CMPB	OPRS(AL,IMM(MTP_SYM))		_; See if symbol.
	JB		SDISP(colonfail)			_; And fail if not
	MOVL	OPRS(MADDR(8,ESP),EBX)		_; And store back in frame

wm_call:								_; Get call structure and deref
	MOVL	OPRS(EAX,MADDR(12,ESP))
	Deref(EBX,EAX,AL,cllgrnd,calldrfcll)

callfail:								_; Blew it. Call structure is variable.
	Fail

cllgrnd:								_; Look at tag in more detail
	TagTest(AL)
	JStruct	SDISP(callstruct)			_; Jump if structure.
	JList	SDISP(callfail)				_; Die if a list.

										_; A constant of some type. Find out what
	MOVL	OPRS(EAX,EBX)				_; Got to examine tag.
	ANDB	OPRS(AL,IMM(MTP_CONSTMASK)) _; Nuke all but the tag.
	CMPB	OPRS(AL,IMM(MTP_SYM))		_; See if symbol.
	JB		SDISP(colonfail)			_; And fail if not

	MOVL	OPRS(EAX,MADDR(8,ESP))		_; Get the module

	CMPL 	OPRS(EBX,IMM(SYM_CUT)) 		_; See if symbol is cut
	JE 		SDISP(call_cut)

										_; Set up call frame for regular call
	POPL 	MADDR(8,ESP)		 		_; Put old E on top of thing to call
	POPL 	MADDR(8,ESP) 				_; Put return addr on top of the cutpt
	ADDL 	OPRS(ESP,IMM(4))			_; nuke the module
	JMP 	SDISP(call_continue1)

call_cut:
										_; Set up call frame for cut.
	POPL	MADDR(4,ESP)				_; Put old E on top of  module.
	POPL	MADDR(4,ESP)				_; Put return addr on top of thing to call 

call_continue1:
										_; Set up resolve_ref call.
	PUSHL	IMM(0)						_; Want overflow code.
	PUSHL	IMM(0)						_; Last arg is arity
	PUSHL	EBX							_; Penultimate arg is functor.
	PUSHL	EAX							_; First arg is mod id.

	JMP	SDISP(callit)					_; And then do it

callstruct:								_; It was a structure. Much work.

						 				_; Go to the heap and get the arity.
	MOVL	OPRS(EAX,MADDR(-MTP_STRUCT,EBX))
	GetArity(EAX)

	NEGL	EAX							_; Temporarily need a negative number.
						_; Make room for args and resolve frame.
						_; nuke modid & callstruct, make space for 4*numargs
						_; and 4 arguments for call_resolve_reference
	LEA		OPRS(ESP,MADDR(-8,ESP,EAX,4))
	NEGL	EAX							_; And put arity back to positive.

	PUSHL	ECX							_; Need another register for a while.

										_; Go to the heap and get the functor id
	MOVL	OPRS(ECX,MADDR(-MTP_STRUCT,EBX))
	ANDL 	OPRS(ECX,IMM(HEX(ffffff)))
	CMPL 	OPRS(ECX,IMM(SYM_CUT)) 		_; See if symbol is cut macro
	JBE 	SDISP(callstruct_continue)
	POPL 	MADDR(ESP)					_; symbol is not a cut macro 

callstruct_continue:
										_; Set up resolve_ref call and part of
										_; the final call frame.

	MOVL	OPRS(ECX,MADDR(8,EBP))		_; Get the module.
	MOVL	OPRS(MADDR(4,ESP),ECX)		_; And put down into resolve frame.
	PUSHL	MADDR(EBP)					_; Get old E
	MOVL	OPRS(ECX,MADDR(4,EBP))		_; Get return address
	MOVL	OPRS(MADDR(28,ESP),ECX)		_; And put down into final call frame.

	POPL	MADDR(20,ESP)				_; Put old e into final call frame.
										_; This is 24 from the current top of stack,
							 			_; but popping into an indirect from the 
										_; stack pointer is strange.

	MOVL	OPRS(DMADDR(16,ESP),IMM(0))	_; Want overflow.
	MOVL	OPRS(MADDR(12,ESP),EAX)		_; Put down arity.

	GetFunctor(ECX,EBX)					_; Get functor.

	MOVL	OPRS(MADDR(8,ESP),ECX)		_; Put down functor

										_; Now we'll copy arguments
	XCHGL	OPRS(EAX,ECX)				_; We want auto-decrement.
	SUBL	OPRS(EBX,IMM(MTP_STRUCT))	_; Point to beginning of term.

callloop:
	MOVL	OPRS(EAX,MADDR(EBX,ECX,4))	_; Get Ai
										_; and put into proper stack location.	
	MOVL	OPRS(MADDR(24,ESP,ECX,4),EAX)
	LOOP	SDISP(callloop)
	
	POPL	ECX							_; Restore ecx

callit:									_; Now we'll do the actual call

	callCProc(call_resolve_reference)
	ADDL	OPRS(ESP,IMM(16))			_; nuke 4 arguments on stack

	JMP	REGDISP(CReturn)				_; Jump to the routine being called

ENDPROC(wm_colon)


__;
__; Environment which is calling wm_comma has four arguments
__; (modid,first call, second call, and cutpoint) and no permanent variable.
__; We also want to mark from these four argument during garbage collection.
__;

	ALIGN4
gcc2:
	GCINFO(HEX(4),15) 					_; mask=15, nargs=4, npv=0

PROC(wm_comma)
	MOVL	OPRS(EAX,DMADDR(20,EBP))	_; Get cutpt
	ANDB	OPRS(AL,IMM(SEMIMASK))		_; Get rid of the semicolon flag.
	MOVL	OPRS(MADDR(20,EBP),EAX)		_; Put back for next time used.

										_; Set up the first call.
	PUSHL	EAX							_; Put cutpt
	PUSHL	DMADDR(12,EBP)				_; Put first call.
	PUSHL	DMADDR(8,EBP)				_; Put modid.

				_; A fake call and call_entry for wm_colon

	PUSHL	OFFSET(gcc1)				_; Put down return address
	PUSHL	EBP							_; Put down old E

				_; A fake exec entry for wm_colon
	MOVL	OPRS(EBP,ESP)				_; Move frame pointer to new frame.

	JMP	PROCADDR(wm_colon)				_; Do dah call,mang.

gcc1:
	MOVL	OPRS(EAX,IMM(EXPR(gcc2 - gcc1)))

				_; See if choice point protecting
				_; the environment.
	CMPL	OPRS(EDI,EBP)				_; Compare E and SPB
	JB	SDISP(commaprot)				_; Jump if protected by a choice point

				_; Not protected by a choice point.

	MOVL	OPRS(EAX,MADDR(8,EBP))		_; Get modid
	MOVL	OPRS(MADDR(12,EBP),EAX)		_; And put down where it is needed
	MOVL	OPRS(EAX,MADDR(4,EBP))		_; Get return address
	MOVL	OPRS(MADDR(8,EBP),EAX)		_; And put down where it is needed
	MOVL	OPRS(EAX,MADDR(EBP))		_; Get old E
	MOVL	OPRS(MADDR(4,EBP),EAX)		_; And put down where it is needed
	LEA	OPRS(ESP,MADDR(4,EBP))			_; Collapse stack frame
	MOVL	OPRS(EBP,ESP)				_; Set E to new frame

	JMP	DISP(PROCADDR(wm_colon))		_; And do the call

commaprot:								_; Protected by a choice point.
	MOVL	OPRS(SP,SPB)				_; Deallocate stack
	PUSHL	DMADDR(20,EBP)				_; Cutpt
	PUSHL	DMADDR(16,EBP)				_; The second call
	PUSHL	DMADDR(8,EBP)				_; Modid
	PUSHL	DMADDR(4,EBP)				_; Return address
	PUSHL	DMADDR(EBP)					_; Old E

	MOVL	OPRS(EBP,ESP)				_; Set E to new frame

	JMP	DISP(PROCADDR(wm_colon))		_; And do the call.

ENDPROC(wm_comma)


__;
__; A ; B
__;

PROC(semitrust)

			_; Nuke choice point
	TrustMe

	MOVL	OPRS(EAX,DMADDR(8,ESP))	_; Pick up modid
	MOVL	OPRS(MADDR(12,ESP),EAX)	_; and put down over first alternative.
	MOVL	OPRS(EAX,DMADDR(4,ESP))	_; Pick up return address
	MOVL	OPRS(MADDR(8,ESP),EAX)	_; and put down over old modid place.
	POPL	EAX			_; Pick up Old E
	MOVL	OPRS(MADDR(ESP),EAX)	_; and put in place.

	MOVL	OPRS(EBP,ESP)		_; Adjust E.

	JMP	DISP(PROCADDR(wm_colon))	_; And run the goal.

ENDPROC(semitrust)



__; Environment which is calling wm_semicolon has four arguments
__; (modid,first call, second call, and cutpoint) and no permanent variable.
__; We also want to mark from these four argument during garbage collection.
	ALIGN4
scgcc2:
	GCINFO(HEX(4),15) 	_; mask=15, nargs=4, npv=0
__;	GCINFO(15,4,0) 	_; mask=15, nargs=4, npv=0
__;DD(HEX(603))
__;DD(-1)
__;DD(0)

PROC(wm_semicolon)

	__; Need a choice point. Alternate is above.
	TryMe(semitrust)

	MOVL 	OPRS(EAX,DMADDR(20,ESP))	_; Pick  up the cutpt
	ORB		OPRS(AL,IMM(SEMIFLAG))	_; Mark cutpt as part of semicolon

	PUSHL	EAX			_; Put cutpt on stack
	PUSHL	DMADDR(16,ESP)		_; Put down first call
	PUSHL	DMADDR(16,ESP)		_; Put down modid
	PUSHL	OFFSET(scgcc1)	_; Put down return address
	PUSHL	EBP		_; Put down old E

	MOVL	OPRS(EBP,ESP)		_; Adjust E for this frame
	
	JMP	DISP(PROCADDR(wm_colon))		_; And do the call

scgcc1:
	MOVL	OPRS(EAX,IMM(EXPR(scgcc2 - scgcc1)))

	__; Now we have to deallocate the environment allocated
	__; above, and execute a proceed instruction.

	__; See if choice point protecting the environment.

	CMPL	OPRS(EDI,EBP)		_; Compare E and SPB
	JA  	SDISP(scnotprot)	_; Jump if not protected by a choice point

	__; Protected by a choice point

	LEA 	OPRS(ESP,MADDR(-24,EDI)) 	_; Knock down the stack
	JMP 	SDISP(sccont)

	__; Not protected by a choice point.

scnotprot:
	LEA 	OPRS(ESP,MADDR(EBP)) 	_; Knock down the stack

sccont:
	MOVL 	OPRS(EAX,MADDR(4,EBP)) 		_; Get return adress
	MOVL 	OPRS(MADDR(20,ESP),EAX) 	_; Put it 
	MOVL 	OPRS(EAX,MADDR(EBP)) 		_; Get old E
	MOVL 	OPRS(MADDR(16,ESP),EAX) 	_; Put it 
	LEA 	OPRS(ESP,MADDR(16,ESP)) 	_; Knock down the stack
	MOVL 	OPRS(EBP,ESP)				_; New E 

	MOVL 	OPRS(EAX,MADDR(4,EBP))	_; Get return address
	MOVL 	OPRS(EBP,MADDR(EBP)) 	_; Get old E
	JMP		REGDISP(EAX)			_; Return 	

ENDPROC(wm_semicolon)


__;
__; Stack frame when wm_arrow called.
__;
__; |-------------------------------------------|
__; | Cutpt for procedure calling wm_arrow	|
__; |-------------------------------------------|
__; | Then of arrow				|
__; |-------------------------------------------|
__; | If of arrow				|
__; |-------------------------------------------|
__; | Module ID of calling procedure		|
__; |-------------------------------------------|
__; | Return address				|
__; |-------------------------------------------|
__; | Old E					|
__; |-------------------------------------------|
__;

__; Environment which is calling wm_arrow has four arguments
__; (modid,first call, second call, and cutpoint) and one permanent variable.
__; The permanent variable is the arrow cut point saved by wm_arrow routine.
__; We also want to mark from these four argument during garbage collection.
	ALIGN4
arrowgc2:
	GCINFO(HEX(10004),15) 	_; mask=15, nargs=4, npv=1
__;	GCINFO(15,4,1) 	_; mask=15, nargs=4, npv=1
__;	DD(HEX(603))
__;	DD(-1)
__;	DD(0)

PROC(wm_arrow)

	MOVL	OPRS(EAX,DMADDR(20,EBP))	_; Get procedure cutpt.
	MOVL	OPRS(EBX,GVAR(wm_heapbase))	_; Need heap base to 
						_; make arrow cutpt.

	BTRL	OPRS(EAX,IMM(SEMIBIT))	_; Bit SEMIBIT is semi flag.
	JNB	SDISP(notset)	_; Jump if not from semicolon

				_; It is from semicolon. The cutpt should be
				_; SPB since we want to cut the semicolon
				_; choice point.
	SUBL	OPRS(EBX,EDI)	_; Subtract SPB from heapbase (see above)
	JMP	SDISP(cont)	_; and continue.

notset:
				__; We don't have a semicolon choice point
				__; to remove. However,we can't use the
				__; cutpt already in progress,because that
				__; will cut the procedure,and -> doesn't
				__; do that. The only safe thing to cut back to
				_; is the current E pointer.
	SUBL	OPRS(EBX,EBP)	_; Subtract E from heapbase (see above)

cont:
	MK_INT(EBX)		_; Make cutpt into an integer.

	PUSHL	EBX			_; and put into the environment.

					_; The cutpt has had the semi flag 
					_; removed by the bit test above.
	MOVL	OPRS(MADDR(20,EBP),EAX)	_; Put the corrected version back.

				_; We want -> transparent to cut. To make
				_; it opaque, push EBX rather than EAX.
	PUSHL	EAX		_; Put cutpt for the IF part of the call.
	PUSHL	DMADDR(12,EBP)	_; Put down call
	PUSHL	DMADDR(8,EBP)	_; Put down modid

				_; A fake call and call_entry for wm_colon

	PUSHL	OFFSET(arrowgc1)	_; Put down return address
	PUSHL	EBP		_; Put down old E

				_; A fake exec entry for wm_colon
	MOVL	OPRS(EBP,ESP)	_; Move frame pointer to new frame.

	JMP	PROCADDR(wm_colon)	_; Do the If.

arrowgc1:
	MOVL	OPRS(EAX,IMM(EXPR(arrowgc2-arrowgc1)))

				_; Now we must cut away any choice point
				_; left by the If call.
	MOVL	OPRS(EBX,DMADDR(-4,EBP))	_; Pick up arrow cutpt.
	ExtractCutPt(EAX,EBX,BL)		_; Get actual cut point.

			_; Cut away the If (and possibly semicolon).
	Cut

	MOVL	OPRS(ESP,EBP)		_; Knock the stack down

	MOVL	OPRS(EAX,DMADDR(8,ESP)) _; Get the modid
	MOVL	OPRS(MADDR(12,ESP),EAX)	_; and move on top of If.
	MOVL	OPRS(EAX,DMADDR(4,ESP)) _; Get the return address
	MOVL	OPRS(MADDR(8,ESP),EAX)	_; and move into place.
	POPL	EAX			_; Pick up Old E
	MOVL	OPRS(MADDR(ESP),EAX)	_; and move into place.

	MOVL	OPRS(E,SP)		_; Set the E pointer.
	JMP	DISP(PROCADDR(wm_colon))	_; And execute the Then.
	
ENDPROC(wm_arrow)

__;
__; ocall: Setup a call and set an interrupt before doing the call. The call
__; will ignore the overflow check in the called procedure.
__;

PROC(wm_ocall)

	MOVL	OPRS(EBX,GVAR(wm_trigger))	_; Set ouch
	MOVL	OPRS(GVAR(wm_safety),EBX)

wm_ocall_entry:
				_; Get the module atom and deref it.
	MOVL	OPRS(EAX,MADDR(8,ESP))
	Deref(EBX,EAX,AL,omodgrnd,omoddrf)

omodfail:								_; Fail if it is a variable.
	Fail

omodgrnd:
	MOVL	OPRS(EAX,EBX)				_; Got to examine tag.
	ANDB	OPRS(AL,IMM(MTP_CONSTMASK)) _; Nuke all but the tag.
	CMPB	OPRS(AL,IMM(MTP_SYM))		_; See if symbol.
	JB		SDISP(omodfail)				_; And fail if not
	MOVL	OPRS(MADDR(8,ESP),EBX)		_; Store back in frame.

			_; Get the call structure and dereference it.

	MOVL	OPRS(EAX,MADDR(12,ESP))
	Deref(EBX,EAX,AL,ocllgrnd,ocalldrfcll)

ocallfail:								_; Blew it. Call structure is variable.
	Fail

ocllgrnd:
			_; Look at tag in more detail and branch accordingly.
	TagTest(AL)
	JStruct	SDISP(ocallstruct)			_; Jump if structure.
	JList	SDISP(ocallfail)			_; Die if a list.

					_; A constant of some type. Find out what.

	MOVL	OPRS(EAX,EBX)				_; Got to examine tag.
	ANDB	OPRS(AL,IMM(MTP_CONSTMASK)) _; Nuke all but the tag.
	CMPB	OPRS(AL,IMM(MTP_SYM))		_; See if symbol.
	JB		SDISP(ocallfail)			_; And fail if not

					_; Set up call frame for atom call.

	MOVL	OPRS(EAX,MADDR(8,ESP))		_; Get the module
	POPL	MADDR(4,ESP)				_; put OldE where we had modid
	POPL	MADDR(4,ESP)				_; put ret addr where we had callstruct

					_; Set up resolve_ref call.

	PUSHL	IMM(1)						_; Don't want overflow code.
	PUSHL	IMM(0)						_; Last arg is arity
	PUSHL	EBX							_; Penultimate arg is functor.
	PUSHL	EAX							_; First arg is mod id.

	JMP	SDISP(ocallit)					_; And then do it

ocallstruct:							_; It was a structure. Much work.

				_; Go to the heap, get the functor word and
				_; remove the arity.
	MOVL	OPRS(EAX,MADDR(-MTP_STRUCT,EBX))
	GetArity(EAX)

	NEGL	EAX							_; Temporarily need a negative number.
						_; Make room for args and resolve frame.
						_; nuke modid & callstruct, make space for 4*numargs
						_; and 4 arguments for call_resolve_reference	
	LEA	OPRS(ESP,MADDR(-8,ESP,EAX,4))

	NEGL	EAX							_; And put arity back to positive.

	PUSHL	ECX							_; Need another register for a while.

				_; Set up resolve_ref call and part of
				_; the final call frame.

	MOVL	OPRS(ECX,MADDR(8,EBP))		_; Get the module.
	MOVL	OPRS(MADDR(4,ESP),ECX)		_; And put down into resolve frame.
	PUSHL	MADDR(EBP)					_; Get old E
	MOVL	OPRS(ECX,MADDR(4,EBP))		_; Get return address
	MOVL	OPRS(MADDR(28,ESP),ECX)		_; And put down into final call frame.

			__; Put old e into final call frame.
			__; This is 24 from the current top of stack, but
			__; popping into an indirect from the stack pointer
			__; is strange.

	POPL	MADDR(20,ESP)

	MOVL	OPRS(DMADDR(16,ESP),IMM(1))	_; Don't want overflow.
	MOVL	OPRS(MADDR(12,ESP),EAX)		_; Put down arity.

	GetFunctor(ECX,EBX)					_; Get functor.

	MOVL	OPRS(MADDR(8,ESP),ECX)		_; Put down functor

										_; Now we'll copy arguments
	XCHGL	OPRS(EAX,ECX)				_; We want the auto-decrement register
	SUBL	OPRS(EBX,IMM(MTP_STRUCT))	_; Point to beginning of term.

ocallloop:
	MOVL	OPRS(EAX,MADDR(EBX,ECX,4))	_; Get Ai
	MOVL	OPRS(MADDR(24,ESP,ECX,4),EAX)	_; and put into proper stack location
	LOOP	SDISP(ocallloop)
	
	POPL	ECX							_; Restore ecx

ocallit:								_; Now we'll do the actual call

	callCProc(call_resolve_reference)

	ADDL	OPRS(ESP,IMM(16))			_; nuke 4 args of call_resolve_reference

	RegExecute(CReturn)					_; Jump to the routine being called

ENDPROC(wm_ocall)


__;
__; wm_dbg_call: Setup a call. The called procedure will ignore overflow check.
__;

PROC(wm_dbg_call)
	JMP 	PROCADDR(wm_ocall_entry)
ENDPROC(wm_dbg_call)


_textend

_end

