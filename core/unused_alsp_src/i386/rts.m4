	FILE(rts) 	

__;
__; rts.m4			-- run time system
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

ifdef(`DOS',`
INCLUDE sconfig.h
')

_dgroup

_assume

EXTRN(wm_heapbase,dword)
EXTRN(wm_b_reg,dword)

EXTRN(resolve_reference,near)
EXTRN(symuiacmp,near)
EXTRN(probe_token,near)
EXTRN(wm_try_me,near)
EXTRN(wm_trust_me,near)
EXTRN(wm_colon,near)
EXTRN(wm_docut,near)


_conststart 
_constend   


_datastart

	GLOBAL(UnifyPtr)
DD(GVAR(UnifyPtr),PROCADDR(wm_unify))

	GLOBAL(from_w_unify)
DD(GVAR(from_w_unify),0)


DD(GVAR(rungoal_success),0)

DD(GVAR(save_esp),0)



__; The following code is only included in the AI Arhitects VM Environment,
__; We had to use direct intersegment call in the AI Arhitects VM Environment
__; because indirect intersegment call had not worked in the AI Arhitects 
__; VM Environment. It is assumed that 'xseg_call_selector' is filled by
__; an initalization routine when ALS Prolog is started, and 'xseg_call_disp'
__; is filled before somebody jumps to the following routine.
__; Since the following code is going to be used in a specific environment
__; it is written in MASM syntax. (I didn't want to write M4 macros for them).
__; 		Ilyas 1/31/90

ifdef(`DOS',`
IFDEF Ergo

ret_addr	dd 	?

        ALIGN4
xseg_wme_gcc2:
	GCINFO(HEX(0),0)		_; mask=0, nargs=0, npv=0

			align 	4
			public	xseg_call_selector
xseg_call_inst label far
			pop 	eax
			mov 	ret_addr,eax
			db 		09Ah		_; direct intersegment call
xseg_call_disp 		dd ?
xseg_call_selector	dw ?
xseg_wme_gcc1:
    	MOVL    OPRS(EAX,IMM(EXPR(xseg_wme_gcc2 - xseg_wme_gcc1)))
			push 	ret_addr
			ret
ENDIF
')

_dataend


_bssstart
_bssend


_textstart


__;
__; wm_sw_const
__;
__; Search a table for a constant value and then jump to the associated 
__; address.
__;
__; Table looks like
__;
__;	Const0
__;	Address0
__;	Const1
__;	Address1
__;	.......
__;
__; where Addressi is where control should transfer if Consti is found. The
__; entries should be in ascending order as the code does a binary search. The
__; table is long word aligned.
__;
__; Calling convention:
__;
__;	EAX	Size of table in bytes.
__;
__; Should be called with a call instruction and the table should be located
__; immediately after the nop following the call instruction.
__;

		ALIGN4
		GLOBAL(swConstPtr)
DD(GVAR(swConstPtr),PROCADDR(wm_sw_const))

PROC(wm_sw_const)

	MOVL	OPRS(EBX,MADDR(12,ESP))	_; Get first argument

			_; See if the bloody thing is a UIA.
	ANDL	OPRS(EBX,IMM(MTP_CONSTMASK))
	CMPB	OPRS(BL,IMM(MTP_UIA))
	JNE	SDISP(notuia)

			_; Tis a UIA. Much work ahead.

			_; First get the pointer to the first character.
	MOVL	OPRS(EBX,MADDR(12,ESP))
	SHRL	OPRS(EBX,IMM(MTP_CONSTSHIFT))
	ADDL	OPRS(EBX,GVAR(wm_heapbase))
	ADDL	OPRS(EBX,IMM(4))

			_; Save the EAX register (which has the size of
			_; the switch table.
	PUSHL	EAX

			_; Push pointer to the string and call probe_token.
	PUSHL	EBX
	callCProc(probe_token)
	POPL	EBX

			_; See if wasn't there. If so, we fail. 
	CMPL	OPRS(CReturn,IMM(0))
	JNE	SDISP(gottavalue)

	Fail

gottavalue:
			_; Make probe into a symbol and put it into the
			_; correct register.
	MK_SYM(CReturn)
	MOVL	OPRS(EBX,CReturn)

			_; Get the size of the switch table back. Then begin
			_; the search.
	POPL	EAX
	JMP	SDISP(sw_begin)

notuia:
	MOVL	OPRS(EBX,MADDR(12,ESP))	_; Get first argument

sw_begin:
	XCHGL	OPRS(MADDR(ESP),EDX)	_; Get bottom of table and save EDX.
	PUSHL	ECX			_; Need another register.

	INCL	EDX			_; Move beyond NOP to first key entry.

					_; Calculate midpoint of table.
	MOVL	OPRS(ECX,EAX)		_; Get size of table
	SHRL	OPRS(ECX,IMM(1))	_; Divide by two.

			__; The 8 here will change if the size of the
			__; switchTableEntry structure changes.

constComp:
	CMPL	OPRS(EBX,DMADDR(EDX,ECX,8))	_; Compare key to entry

	JA	SDISP(constAbove)		_; It is greater
	JB	SDISP(constBelow)		_; It is less

	MOVL	OPRS(EAX,MADDR(4,EDX,ECX,8))	_; Get jump address

constJump:
	POPL	ECX				_; Restore registers
	POPL	EDX

	JMP	REGDISP(EAX)			_; Jump to address.

constAbove:
	LEA	OPRS(EDX,MADDR(EDX,ECX,8))	_; Move bottom of table 
						_; to new position
	SUBL	OPRS(EAX,ECX)			_; Correct size of table
	MOVL	OPRS(ECX,EAX)			_; Get table size
	SHRL	OPRS(ECX,IMM(1))		_; Divide by 2
	JNE	SDISP(constComp)		_; If not 0, there are still 
						_; items to search.

constFail:
	POPL	ECX				_; Get register back
	Fail					_; No more. Fail.

constBelow:
	MOVL	OPRS(EAX,ECX)			_; Move halfway point
	SHRL	OPRS(ECX,IMM(1))		_; Divide by 2
	JNE	SDISP(constComp)		_; If not 0, there are still
						_;  items to search.

					_; Have to check bottom entry.
	CMPL	OPRS(MADDR(EDX),EBX)	_; See if equal
	JNE	SDISP(constFail)	_; Nope. Have to fail.

	MOVL	OPRS(EAX,MADDR(4,EDX))	_; Jump to address part.
	JMP	SDISP(constJump)

ENDPROC(wm_sw_const)


__;
__; wm_sw_struct
__;
__; Switch on structure. See wm_sw_const for details.
__;

		ALIGN4
		GLOBAL(swStructPtr)

DD(GVAR(swStructPtr),PROCADDR(wm_sw_struct))

PROC(wm_sw_struct)

			_; Get f/a word from structure.
	MOVL	OPRS(EBX,MADDR(12,ESP))
	GetFunctor(EBX,EBX)

	JMP	SDISP(sw_begin)		_; Go use sw_const code

ENDPROC(wm_sw_struct)


__;
__; wm_exec is called from C to start out execution.
__;


        ALIGN4
wme_gcc2:
	GCINFO(HEX(0),0)					_; mask=0, nargs=0, npv=0

PROC(wm_exec)

	SaveCRegs

	MOVL	OPRS(EAX,ESP)				_; remember C stack pointer

	LoadPrologRegs						_; switch to prolog Stack

	INCL	GVAR(wm_regidx)

	MOVL	OPRS(EBX,DMADDR(36,EAX))	_; get thing to execute into EBX

ifdef(`DOS',	`
IFDEF Ergo
	MOVL	OPRS(xseg_call_disp,EBX)	_; special stuff for AI Architects
	LEA 	OPRS(EBX,xseg_call_inst)
	CALL 	REGDISP(EBX)
ELSE
	CALL	REGDISP(EBX)				_; Execute goal
wme_gcc1:
   	MOVL    OPRS(EAX,IMM(EXPR(wme_gcc2 - wme_gcc1)))
ENDIF
',`
	CALL	REGDISP(EBX)				_; Execute goal
wme_gcc1:
   	MOVL    OPRS(EAX,IMM(EXPR(wme_gcc2 - wme_gcc1)))
')

	MOVL 	OPRS(EAX,E)
	CALL 	PROCADDR(wm_docut)
	
	DECL	GVAR(wm_regidx)
										_; get new wm_H, wm_TR, wm_B
	MOVL	OPRS(EAX,GVAR(wm_regidx))
	SHLL	OPRS(EAX,IMM(6))
	ADDL	OPRS(EAX,OFFSET(GVAR(wm_regs)))

	MOVL	OPRS(MADDR(wm_H,EAX),H)
	MOVL	OPRS(MADDR(wm_TR,EAX),TR)
	MOVL	OPRS(ECX,GVAR(wm_b_reg))		_; B register
	MOVL	OPRS(MADDR(wm_B,EBX),ECX)


	LoadCRegs							_; switch to C stack

	RET

ENDPROC(wm_exec)


__;
__;	wm_execute_builtin is invoked from name table code
__;	area of a prolog procedure that is defined by a
__;	C procedure. The address of the C routine is in EAX.
__; This routine is entered via a JMP.
__;

PROC(wm_execute_builtin)

	SavePrologRegs

	MOVL 	OPRS(ESP,GVAR(save_esp)) 	_; switch to C stack

	CALL	REGDISP(EAX)

	LoadPrologRegs						_; switch to Prolog stack

	CMPL	OPRS(CReturn,IMM(0))
	JE		SDISP(fail_builtin)

	Proceed

fail_builtin :

	Fail

ENDPROC(wm_execute_builtin)


__;
__; wm_unify is called to perform those unifications which are potentially too
__;	long to do in line.
__;	
__; Register Usage:
__;	EAX and EBX contain the input arguments.  The rest of the registers
__;	may be used and we can not assume that they are free. In particular,
__;	HB, SPB, and TR will contain values necessary for
__;	doing the unification.
__;
__; In an effort to prevent saving EDX and EBP each time the unifier
__; is called recursively, we will say that the calling routine must save
__; them before the unifier is called. Makes the unifier more uniform and
__; faster.
__;

PROC(wm_unify)
			__; Dereference argument in EAX.

	Deref(EDX,EAX,AL,g1,unifyloop)

			__; Argument in EAX is a variable. Deref EBX.

	Deref(EDX,EBX,BL,v1g0,u1)

			__; both arguments are variables

	CMPL	OPRS(EBX,EAX)
	JE	SDISP(uret)						_; return if variables are equal
	JB	SDISP(u2)						_; branch taken if EAX > EBX

	XCHGL	OPRS(EBX,EAX)				_; swap roles of EAX and EBX

u2:
	CMPL	OPRS(EBX,GVAR(wm_heapbase))	_; see if EBX is in the heap
	JB	SDISP(u4)						_; branch if not
	Bind(EAX,EBX)						_; make EAX var point at EBX
	CMPL	OPRS(HB,EAX)				_; compare against HB
	JBE	SDISP(u3)						_; branch if HB smaller or equal
	Trail(EAX)							_; Trail it

u3:
	MOVL	OPRS(EAX,IMM(1))			_; unification returns success
	RET									_; return to caller

			__; EAX is in Arg/Env Stack

u4:
	Bind(EBX,EAX)						_; make EBX var point at EAX
	CMPL	OPRS(SPB,EBX)				_; compare against SPB
	JA	SDISP(uret)						_; branch if trailing not needed
	Trail(EBX)							_; Trail it

uret:
	MOVL	OPRS(EAX,IMM(1))			_; unfication returns success
	RET									_; return to caller

			_; thing in EDX is ground. Dereference arg in EBX.
g1:
	Deref(EAX,EBX,BL,g1g0,g1loop)

			_; EDX ground, EAX variable
v1g0:
	Bind(EAX,EDX)						_; Do binding.
	CMPL	OPRS(HB,EAX)				_; compare against HB
	JBE	SDISP(u6)						_; take branch if HB is smaller or equal
	CMPL	OPRS(SPB,EAX)				_; compare against SPB
	JA	SDISP(u6)						_; take branch if SPB is bigger
	Trail(EAX)							_; Trail it
u6:
	MOVL	OPRS(EAX,IMM(1))			_; unfication returns success
	RET									_; return to caller

g1g0:
	CMPL	OPRS(EDX,EAX)				_; see if objects are the same
	JE	SDISP(uret)						_; return if so

			__; Now check tags between the two objects. If not
			__; equal, we fail.
			__;
			__; Fortunately for us, BL has tag of EAX

	MOVL	OPRS(EBP,EDX)
	ANDB	OPRS(DL,IMM(MTP_TAGMASK))
	CMPB	OPRS(DL,BL)
	JNE	DISP(ufail)

			__; Tags are the same. see what we got and branch
			__; accordingly

	TagTest(BL)
	JList	SDISP(ulist)				_; branch if we got a list
	JConst	DISP(u7)					_; branch if we have a constant

	__;
	__; must have a structure
	__;

	MOVL	OPRS(EBX,EBP)				_; get the object back

 	SUBL	OPRS(EAX,IMM(MTP_STRUCT))	_; Nuke structure tags
	SUBL	OPRS(EBX,IMM(MTP_STRUCT))

	MOVL	OPRS(EDX,MADDR(EAX))		_; get one of the functors
	CMPL	OPRS(EDX,MADDR(EBX))		_; compare against the other
	JNE	DISP(ufail)						_; fail if not the same

			__; shift edx to get arity

	SHRL	OPRS(EDX,IMM(MTP_ARITYSHIFT))

	__; Check Big Structures ( Arity >= 255)

	CMPL 	OPRS(EDX,IMM(255)) 			_; compare arity against 255
	JNE 	DISP(usloop) 				_; it is not a big structure, continue
	ADDL	OPRS(EAX,IMM(4))			_; Make each point to big arity
	ADDL	OPRS(EBX,IMM(4))
	MOVL 	OPRS(EDX,MADDR(EAX)) 		_; get one of big arities 
	CMPL 	OPRS(EDX,MADDR(EBX)) 		_; compare against the other
	JNE		DISP(ufail)					_; fail if not the same
	SHRL 	OPRS(EDX,IMM(MTP_CONSTSHIFT)) 	_; shift edx to get arity

			__; Loop through each subterm, unifying them.

usloop:	
	ADDL	OPRS(EAX,IMM(4))			_; Make each point to first subterm
	ADDL	OPRS(EBX,IMM(4))
	DECL	EDX
	JE	PROCADDR(wm_unify)				_; Last argument is merely a jump
	PUSHL	EAX							_; Save argument positions
	PUSHL	EBX
	PUSHL	EDX
	CALL	PROCADDR(wm_unify)
	CMPL	OPRS(EAX,IMM(1))			_; check for success
	JNE		SDISP(subterm_fail)
	POPL	EDX
	POPL	EBX
	POPL	EAX
	JMP	SDISP(usloop)

			__; we will reach here only if wm_unify was called
			__; from _w_unify. This is because wm_unify does
			__; a return on failure only if called from _w_unify.
subterm_fail:
	ADDL	OPRS(ESP,IMM(12))			_; nuke 3 items from stack
	RET									_; EAX still has 0 (failure)

ulist:	
			__; Get rid of list tags. Will then have CAR addresses

	LEA	OPRS(EBX,MADDR(-MTP_LIST,EBP))
	SUBL	OPRS(EAX,IMM(MTP_LIST))

			__; Save CAR addresses on stack.

	PUSHL	EAX
	PUSHL	EBX

			__; Unify the buggers again.

	CALL	PROCADDR(wm_unify)
	CMPL	OPRS(EAX,IMM(1))			_; check for success
	JNE		SDISP(car_fail)

			__; Get addresses back, move to CDR, and unify them.

	POPL	EBX
	POPL	EAX
	ADDL	OPRS(EAX,IMM(4))
	ADDL	OPRS(EBX,IMM(4))
	JMP	PROCADDR(wm_unify)

car_fail:
	ADDL	OPRS(ESP,IMM(8))			_; nuke 2 items from stack
	RET									_; EAX still has 0 (failure)

			__; Both have constant tag. They are in EAX and EBP.
			__; See if EAX is not a symbol or UIA. If not,
			__; fail miserably. If a uia, go to next check.

u7:
	MOVL	OPRS(EBX,EAX)
	ANDB	OPRS(BL,IMM(MTP_CONSTMASK))
	CMPB	OPRS(BL,IMM(MTP_SYM))
	JB	SDISP(ufail)
	JA	SDISP(uuia0)

			__; EAX a symbol. See if EBP is a UIA. If not, fail.

	MOVL	OPRS(EBX,EBP)
	ANDB	OPRS(BL,IMM(MTP_CONSTMASK))
	CMPB	OPRS(BL,IMM(MTP_UIA))
	JNE	SDISP(ufail)

			__; EBP a UIA. Put in the correct register and call
			__; symuiacmp.

	MOVL	OPRS(EBX,EBP)
	JMP	PROCADDR(symuiacmp)

			__; EAX a UIA. See if EBP is a uia. If so, compare
			__; UIAs. Otherwise, see if not a symbol. If not,
			__; fail.

uuia0:
	MOVL	OPRS(EBX,EBP)
	ANDB	OPRS(BL,IMM(MTP_CONSTMASK))
	CMPB	OPRS(BL,IMM(MTP_SYM))
	JA	SDISP(uuiauia)
	JB	SDISP(ufail)

			__; EAX a UIA and EBP a symbol. Put them in the
			__; correct registers and go compare them.

	MOVL	OPRS(EBX,EAX)
	MOVL	OPRS(EAX,EBP)
	JMP	PROCADDR(symuiacmp)

uuiauia:
			__; Get the UIA pointed at by EAX into ESI so we
			__; can use the string match instruction

	XCHGL	OPRS(EAX,ESI)
	SHRL	OPRS(ESI,IMM(MTP_CONSTSHIFT))
	ADDL	OPRS(ESI,GVAR(wm_heapbase))

			__; Get the UIA pointed at by EBP into EDI so we can
			__; use the string match instruction.

	XCHGL	OPRS(EDI,EBP)
	SHRL	OPRS(EDI,IMM(MTP_CONSTSHIFT))
	ADDL	OPRS(EDI,GVAR(wm_heapbase))

			__; Now we need the ECX register

	PUSHL	ECX

			__; Figure out how big the silly things are.

	MOVL	OPRS(ECX,MADDR(ESI))
	SHRL	OPRS(ECX,IMM(MTP_CONSTSHIFT))

			__; Now we compare.

	REPE(CMPSD)

			__; Get those registers back.

	XCHGL	OPRS(ESI,EAX)
	XCHGL	OPRS(EDI,EBP)
	POPL	ECX

			__; Now we will check. We didn't have to do the
			__; check before since all MOV-type operations
			__; have no effect on the status flags.

	JNE	SDISP(ufail)

			__; It worked. Return.

	MOVL	OPRS(EAX,IMM(1))			_; return success
	RET

			__; Unification failed for some reason. Bye.

ufail:
	CMPL	OPRS(GVAR(from_w_unify),IMM(1))
	JE		SDISP(ufail_return)

	Fail

ufail_return:							_; called from w_unify, so return
	MOVL	OPRS(EAX,IMM(0))			_; with EAX set to 0
	RET

ENDPROC(wm_unify)

__;
__; w_unify is called by the C defined builtins when unification is needed.
__;	Control is returned to C if successful, else failure occurs in the
__;	usual manner.
__;

PROC(_w_unify)

	SaveCRegs

	MOVL	OPRS(EAX,ESP)				_; remember C sp

	LoadPrologRegs						_; switch to Prolog stack

	MOVL	OPRS(EBX,MADDR(40,EAX))		_; Get items to unify in EAX, EBX
	PUSHL	EBX
	XCHGL	OPRS(EAX,EBX)
	MOVL	OPRS(EAX,MADDR(36,EBX))
	POPL	EBX

	MOVL	OPRS(GVAR(from_w_unify),IMM(1))

	CALL	PROCADDR(wm_unify)

	MOVL	OPRS(GVAR(from_w_unify),EAX) _; save EAX in from_w_unify

	MOVL	OPRS(EAX,GVAR(wm_regidx))	_; index into register array
	SHLL	OPRS(EAX,IMM(6))			_; Each 4 words, 16 in each one
	ADDL	OPRS(EAX,OFFSET(GVAR(wm_regs)))	_; Get base of register array

	MOVL	OPRS(MADDR(wm_TR,EAX),TR)	_; May have trailed stuff

	LoadCRegs

	MOVL	OPRS(EAX,GVAR(from_w_unify)) _; return value from unify
	MOVL	OPRS(GVAR(from_w_unify),IMM(0))

	RET

ENDPROC(_w_unify)

__;
__; wm_resolve_ref	-- this code is called when an undefined procedure
__;			   is encountered in the execution of the prolog 
__;			   program.  Upon entry, the top of stack has a pointer
__;			   into the name entry.  This address is passed
__;			   of to some C code which links in the real code to
__;			   run [if it can find some] and returns the address
__;			   of the code to run in EAX.  If things went properly,
__;			   this will presumably be the original address.  It
__;			   may, however, be the failure code or ....
__;

PROC(wm_resolve_ref)

			_; Top of stack has return
			_; Get to name table entry.
	SUBL	OPRS(DMADDR(ESP),IMM(eval(SIZERESOLVECODE+NTBL_ENTRYSIZE)))

	callCProc(resolve_reference)

	ADDL	OPRS(ESP,IMM(4))

	JMP		REGDISP(CReturn)

ENDPROC(wm_resolve_ref)

__;
__; call_mod_closure
__;
__; Calling a routine needing the module information. The source procedure
__; will do a call to get here, and this information will be used to get
__; the module ID from the procedure table from where we are coming. This will
__; become the first argument on the call and the call will then be transfered
__; to the closure routine.
__;
__;	Top of stack contains pointer into name table entry of source
__;	EBX contains the address of the routine to jump to.
__;

PROC(call_mod_closure)

			__; Return address points into procedure table from
			__; whence we came.

	POPL	EAX

			__; Get mod id. We go back NTBL_ENTRYSIZE+SIZEMODCODE
			__; to get back to the top of the name table entry,
			__; and then go to offset NTBL_MODID.

	SUBL	OPRS(EAX,IMM(eval(NTBL_ENTRYSIZE+SIZEMODCODE-NTBL_MODID)))
	MOVW	OPRS(AX,WMADDR(EAX))

	ANDL	OPRS(EAX,IMM(HEX(ffff)))	_; Strip out anything in 
						_; the upper part of the word.
	
	MK_SYM(EAX)		_; make module into a symbol.

	XCHGL	OPRS(EAX,MADDR(4,EBP))	_; Put mod id where return address is.
	XCHGL	OPRS(EAX,MADDR(EBP))	_; Put return address where old E is.

	PUSHL	EAX			_; Put down Old E.

				_; And jump to destination.
	RegExecute(EBX)

ENDPROC(call_mod_closure)




__;
__; wm_rungoal is called from C to start out execution.
__;

        ALIGN4
rungoal_gcc2:
		GCINFO(HEX(2),3)				_; mask=3, nargs=2, npv=0

PROC(wm_rungoal)

	SaveCRegs
	MOVL	OPRS(EAX,ESP)				_; remember C sp

	LoadPrologRegs						_; switch to Prolog stack

	INCL	GVAR(wm_regidx)				_; increment register index

	PUSHL 	DMADDR(40,EAX)				_; set up a frame
	PUSHL 	DMADDR(36,EAX)
	PUSHL	OFFSET(rungoal_gcc1)
	PUSHL 	E
	MOVL 	OPRS(E,ESP)

	TryMe(rungoal_trust)				_; protect it with a chpt
		
										_; setup another copy of frame
	PUSHL	EAX							_; place for cutpt
	PUSHL	DMADDR(12,E)
	PUSHL	DMADDR(8,E)
	PUSHL	OFFSET(rungoal_gcc1)		_; push return address
	PUSHL	E							_; push OldE
	MOVL 	OPRS(E,ESP)
	MakeCutPt(EAX,E)
	MOVL	OPRS(MADDR(16,E),EAX)		_; put cutpt in right place
	JMP 	PROCADDR(wm_colon)

rungoal_gcc1:
    MOVL    OPRS(EAX,IMM(EXPR(rungoal_gcc2 - rungoal_gcc1)))

	MOVL 	OPRS(EAX,E)
	CALL 	PROCADDR(wm_docut)

	MOVL 	OPRS(GVAR(rungoal_success),IMM(1))

	JMP		SDISP(rungoal_leave)

rungoal_trust:

	TrustMe

	MOVL 	OPRS(GVAR(rungoal_success),IMM(0))

rungoal_leave:

	DECL	GVAR(wm_regidx) 			_; decrement register index

	__; Figure out where Prolog registers are

	MOVL	OPRS(EAX,GVAR(wm_regidx))  	_; index into register array
	SHLL	OPRS(EAX,IMM(6))			_; Each 4 words, 16 in each one
	ADDL	OPRS(EAX,OFFSET(GVAR(wm_regs)))  _; Get base of register array

	__; Save TR, and H

	MOVL	OPRS(MADDR(wm_TR,EAX),TR)	_; Set trail
	MOVL	OPRS(MADDR(wm_H,EAX),H)		_; Set H
	MOVL	OPRS(ECX,GVAR(wm_b_reg))	_; B register
	MOVL	OPRS(MADDR(wm_B,EBX),ECX)
	
	LoadCRegs

	MOVL 	OPRS(EAX,GVAR(rungoal_success))

	RET

ENDPROC(wm_rungoal)

_textend

_end
