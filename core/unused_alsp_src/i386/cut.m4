	FILE(cut) 	

__;
__; cut.m4			-- Cut related things.
__;	Copyright [c] 1987 Applied Logic Systems, Inc.
__;
__; Author: Kevin Buettner
__; Creation: 2/23/87
__;	1/88	kmh	rewritten for 386
__; 5/92    Ilyas Cicekli, new wm_cut 
__;

include(`assembly.m4')

EXTRN(wm_safety,dword)
EXTRN(wm_normal,dword)
EXTRN(wm_heapbase,dword)
EXTRN(wm_b_reg,dword)
EXTRN(wm_cutaddr,dword)

_dgroup

_assume

_datastart
__; Local variables used in wm_cut
DD(saveH,0)
DD(saveTR,0)
DD(saveEBX,0)
DD(saveEDX,0)
DD(CompactionRegion,0)
DD(temp,0)
_dataend

_bssstart
_bssend

_conststart
_constend

_textstart

__;
__; wm_docut	-- performs the cut operation
__;


__; This is the code which handles a cut exception. If a cut exception
__; occurs, we fake a 0 argument call. Otherwise, if this is a gc interrupt,
__; the GCMAGIC will not work, because when the WAM cut instructions are
__; executed, they do not put a GCMAGIC set of instructions down. Maybe
__; they should. Be that as it may, we then have to dispose of this
__; extra information. So rather than EXECUTING !/1, we have to CALL it.
__; Then we can come back to the right place to dispose of the garbage
__; frame.
__; 

	ALIGN4
gccut2:
	GCINFO(HEX(1),0)	_; mask=0, one arg, no pv

	ALIGN4
dummymask:
	GCINFO(HEX(1),0)	_; mask=0, one arg, no pv

dummyret:
	MOVL	OPRS(EAX,IMM(EXPR(dummyret-dummymask)))


cutexcept:
			__; Put down a fake return address for GCMAGIC.
	PUSHL	OFFSET(dummyret)
	PUSHL	E
	MOVL	OPRS(E, SP)

	MakeCutPt(EBX,EAX)

	__; Put cutpt argument down
	PUSHL	EBX

	__; Call !/1 predicate.
	CALL	REGDISP(GVAR(wm_cutaddr))

	__; GCMAGIC
gccut1:
	MOVL	OPRS(EAX, IMM(EXPR(gccut2-gccut1)))

	__; Collapse stack frame and get rid of old E value.
	MOVL	OPRS(SP,E)
	POPL	E
	POPL	EAX		_; Get rid of dummy address
	RET				_; And return from original cut call


PROC(wm_docut)

__; This code is used to handle the cut exception. We need to be able to
__; trap calls to cut, since it is a syntactic construct in Prolog, so that
__; we can decompile clauses with cut in them, etc.
__;
__; The cut exception make a a normal stack frame as though the call was made
__; to the predicate !/1 with the cutpt as an integer argument to the !/1 call.

			__; See if we are trapping
	MOVL	OPRS(EBX, TR)	_; Get TR
	SUBL	OPRS(EBX, H)	_; Subtract H
	CMPL	OPRS(EBX, GVAR(wm_safety))	_; See if we are triggered
	JA  	GVAR(docut)  	_; We are not triggered, do the cut
	CMPL 	OPRS(EBX,GVAR(wm_normal)) 	_; See if it is gc interrupt
						_; Skip gc.
	JA 	SDISP(cutexcept)	_; No it is not, jump to the exception.

__; We assume that eax has the environment to cut back to. After comparing to
__; SPB, we can tell whether we are determinant or not. If SPB is has a larger
__; value, then SPB is older than the current environment, and no cut
__; is necessary because we are already determinant. Otherwise, we have work
__; to do.

	GLOBAL(docut)
FNAMES(docut):
	CMPL	OPRS(EDI,EAX)		_; Compare EAX to SPB
	JBE	SDISP(wm_docut2)	_; jump if EAX >= SPB

	RET				_; nada to do

__; Just when you thought life might be easy, real work has to be done for
__; the cut.
__;
__; We have to search the choice point stack for the appropriate choice point
__; to cut back to. We scan the stack, looking at the SPB pointer in each
__; frame to see if this is the one that we want. We start at the choice
__; point below the top one, because the value of the SPB register is shadowed
__; in the top choice point.

wm_docut2:
	PUSHL	EDX

	MOVL	OPRS(EDX,GVAR(wm_b_reg))	_; Get old B

cut1:
	MOVL	OPRS(EBX,EDX)			_; Put it in a useful place
	MOVL	OPRS(EDX,DMADDR(chpt_B,EBX))	_; Get previous choice point

	MOVL	OPRS(SPB,DMADDR(chpt_SPB,EDX))	_; Get SPB
	ANDL	OPRS(SPB,IMM(HEX(fffffffe)))	_; Nuke compaction bit
	CMPL	OPRS(SPB,EAX)			_; Compare EAX to SPB
	JBE	SDISP(cut1)			_; jump if EAX >= SPB

__; Now we have the choice point which will become the top one in EDX, while
__; EBX points to the choice point before EDX.
__;
__; Between the top of the choice point stack [ECX] and the bottom of the
__; choice point EBX are a bunch of trail entries periodically interrupted
__; by a choice point. We want all of these trail entries to be compacted
__; into a contiguous list because we are cutting away the choice points in
__; between and they are no longer needed.

cutcommon:
	MOVL	OPRS(HB,DMADDR(chpt_HB,EDX))	_; Get HB value
	MOVL	OPRS(GVAR(wm_b_reg),EDX) 	_; Set B to new value.

			_; Point to just after penultimate choice point

	LEA	OPRS(EDX,MADDR(chpt_SIZE,EBX))

cut2:
			_; See if we are done (pointing at
			_; first location of penultimate
			_; choice point).
	CMPL	OPRS(TR,EBX)
	JNE	SDISP(cut3)

			_; Set TR to top of backtrack stack
	MOVL	OPRS(TR,EDX)
	POPL	EDX

	RET		_; And go back

cut3:
	SUBL	OPRS(EBX,IMM(4))	_; Point to next trail entry
	MOVL	OPRS(EAX,DMADDR(EBX))	_; Get trail entry
	CMPL	OPRS(EAX,EBX)		_; See if we've run into a Prev B
	JB	SDISP(cut4)		_; If EAX points into Heap/Env, jump 

__; We've found ourself pointing into the trail, so we've bumped into the PrevB
__; entry of a choice point and must skip over the bugger

	SUBL	OPRS(EBX,IMM(chpt_SIZE-4))	_; Move past the choice point
	JMP	SDISP(cut2)

__; The trail entry points to a variable. If the variable will be nuked by
__; failure, i.e. is greater than HB or less than SPB, we don't need it
__; in the trail any longer.

cut4:
	CMPL	OPRS(EAX,HB)		_; See if greater or equal to HB
	JAE	SDISP(cut2)		_; If so, don't save
	CMPL	OPRS(EAX,SPB)		_; See if less or equal to SPB
	JBE	SDISP(cut2)		_; If so, don't save

	SUBL	OPRS(EDX,IMM(4))	_; Store the trail entry in trail
	MOVL	OPRS(MADDR(EDX),EAX)	_; Save at top

	JMP	SDISP(cut2)		_; and continue

ENDPROC(wm_docut)



 


PROC(oldwm_cut)
		__; Pick up the cut point and extract it.
    MOVL    OPRS(EBX,MADDR(8,ESP))
    ExtractCutPt(EAX,EBX,BL)
       __; Do the cut and go from there.
    CutProceed
ENDPROC(oldwm_cut)


 
__;
__; wm_cut performs the cut from within call.  This procedure
__; is similar to the above procedure, but it is also responsible for
__; cleaning up the argument/environment stack
__;

PROC(wm_cut)

    __; Pick up the cut point and extract it.
    MOVL    OPRS(EBX,MADDR(8,ESP))
    ExtractCutPt(EAX,EBX,BL)

    POPL    E  		   			_; Get Old E off of stack

	CMPL	OPRS(SPB,EAX)		_; Compare EAX to SPB
	JBE		SDISP(wm_cut2)		_; jump if EAX >= SPB
	RET							_; nothing to do

wm_cut2:
	__; Save register H and TR in some variables (I didn't want to save
	__; them in the stack, since we are going to campact it.)
	__; We are going to use these registers together with HB as
	__; scratch registers (also EAX and EBX).
    MOVL    OPRS(saveH,H)	
	MOVL 	OPRS(saveTR,TR)

__;
__; We have to search the choice point stack for the appropriate choice point
__; to cut back to. We scan the stack, looking at the SPB pointer in each
__; frame to see if this is the one that we want. We start at the choice
__; point below the top one, because the value of the SPB register is shadowed
__; in the top choice point.
__;

    MOVL    OPRS(EDX,GVAR(wm_b_reg))    	_; Get old B
cut_chptloop:
    MOVL    OPRS(EBX,EDX)           		_; Put it in a useful place
    MOVL    OPRS(EDX,DMADDR(chpt_B,EBX))    _; Get previous choice point
    MOVL    OPRS(SPB,DMADDR(chpt_SPB,EDX))  _; Get SPB
    ANDL    OPRS(SPB,IMM(HEX(fffffffe)))    _; Nuke compaction bit
    CMPL    OPRS(SPB,EAX)           		_; Compare EAX to SPB
    JBE 	SDISP(cut_chptloop)         	_; jump if SPB <= EAX
 
	__; EDX contains the appropriate choice point to cut back to, and
	__; EBX contains the choice point below the top one.
	__; Save these values since they will be used by "cutcommon" code.
	MOVL 	OPRS(saveEDX,EDX)
	MOVL 	OPRS(saveEBX,EBX)

__;
__; We have found the choice point to cut back to.  This is the
__; value in EDX.  But before we cut back to this point, we must clean
__; up the argument/environment stack.  After the cleanup, the rest
__; of the cut operation is performed by branching to cutcommon.
__; Since we saved register H and TR, and HB will be set but cutcommon code,
__; we have registers EAX, EBX, H, TR, and HB free 
__; to use in whatever manner we desire in the argument/environment 
__; cleanup code. 
 
__; See if any compaction needs to be done
 
	CMPL 	OPRS(E,EAX)				_; see if E > EAX (cutpoint)
	JGE		DISP(jmpto_cutcommon) 	_; if so; No arg/env compaction necessary
__;	JG 		DISP(jmpto_cutcommon) 	_; if so; No arg/env compaction necessary

__;
__; A/E stack compaction -- Step 1:
__;
__; Traverse active environments back to "cutpt".  In the process of
__; traversing the active environments, we will record in a small table
__; the start and end of each environment.  A third slot in the table
__; will be used to store "adjustment" values for use in the compaction
__; phase.
__;
__; Register Usage
__;
__;   SPB -- stack frame corresponding to top choice point
__;          after cut has been performed.  This is value
__;          that we will traverse back to / beyond.
__;
__;   H   -- Top of size/fixup table. This entry will correspond
__;          to the oldest environment examined thus far.
__;
__;   EBX -- Return address from previous environment.  This
__;          value will be important in order to obtain
__;          the size information for the current environment
__;          being examined.
__;
__;   HB  -- current environment being examined.
__;
__;   TR  -- higher environment address from previous
__;          iteration. This address is used to determine
__;          whether to create a new table entry or to
__;          merge the result of the current iteration into
__;          the top table entry.
__;

	MOVL 	OPRS(H,saveH) 			_; Get original H value
	MOVL 	OPRS(HB,E) 				_; set HB to topmost environment
	MOVL 	OPRS(TR,IMM(-1)) 		_; set higher environment address to 
									_; an impossible value
	MOVL 	OPRS(EBX,DMADDR(SP)) 	_; Get return address

step1_L1:
	MOVL 	OPRS(EAX,DMADDR(1,EBX)) _; GCMAGICVal
	ADDL 	OPRS(EBX,EAX) 			_; pointer to GC info
	MOVL 	OPRS(EAX,DMADDR(EBX)) 	_; NArgs + Npv
	MOVL 	OPRS(EBX,EAX) 			_; same thing into EBX
	ANDL 	OPRS(EAX,IMM(HEX(ffff))) 	_; number of args into EAX
	SHRL 	OPRS(EBX,IMM(16))		_; number of env vars into EBX
	SHLL 	OPRS(EAX,IMM(2)) 		_; size of args in bytes
	SHLL 	OPRS(EBX,IMM(2)) 		_; size of env vars in bytes
	NEGL 	EBX 					_; negate the environment size
	ADDL 	OPRS(EAX,IMM(8)) 		_; add in CP, CE sizes to number of args
	ADDL 	OPRS(EAX,HB) 			_; EAX now points one beyond last arg
	ADDL 	OPRS(EBX,HB) 			_; EBX now points at last env var

	CMPL 	OPRS(EBX,TR) 			_; see if can merge these values 
									_; into last table entry
	JNE 	SDISP(step1_L2) 		_; branch if we can't
	
	MOVL 	OPRS(DMADDR(-8,H),EAX) 	_; modify high address in previous entry
	JMP 	SDISP(step1_L3)

step1_L2:
	MOVL 	OPRS(DMADDR(H),EBX) 	_; store low address in table
	MOVL 	OPRS(DMADDR(4,H),EAX) 	_; store high address in table
	ADDL 	OPRS(H,IMM(12)) 		_; advance table pointer

step1_L3:
	MOVL 	OPRS(TR,EAX) 			_; save high value for next iteration
	MOVL 	OPRS(EBX,DMADDR(4,HB)) 	_; get next return address
	MOVL 	OPRS(HB,DMADDR(HB)) 	_; follow environment link

	CMPL 	OPRS(SPB,HB) 			_; see if we are done
	JGE 	SDISP(step1_L1) 		_; branch if SPB >= HB (env pointer)

 
__;
__; A/E Compaction: Step 2
__;
__; We have now set up the table.  We must now determine the starting
__; address to slide the environment values up to.  We do this
__; by examining the high value determined by the last iteration.  This
__; will be one beyond the last argument of the last environment examined.
__; If this value is greater than that of the SPB,  then this is the
__; environment of the SPB and our initial starting value should be
__; the low address of that environment.  If the high value from the
__; last iteration is equal to SPB, then the environment abuts that of
__; SPB's environemnt and we again use the low address of this environment
__; as our starting value.   But if the high value is strictly less than
__; that of SPB, there is a gap between SPB and the last examined
__; environment, so we use SPB as our starting address to slide things
__; toward.
__;
 
	MOVL 	OPRS(TR,SPB) 		_; initialize TR with SPB value
	CMPL 	OPRS(EAX,SPB) 		_; see if high value from last iteration >= SPB
	JL  	SDISP(step3_L0) 	_; branch if  high value < SPB

	MOVL 	OPRS(EBX,DMADDR(-12,H)) 	_; load low address... this necessary
										_; because of possible merging
	CMPL 	OPRS(SPB,EBX) 		_; see if SPB < EBX
	JL 		SDISP(step3_L0) 	_; branch if SPB < EBX

	MOVL 	OPRS(TR,EBX)  		_; use low address
step2_L1:
	SUBL 	OPRS(H,IMM(12)) 	_; nuke last table entry

 
__;
__; A/E Compaction: Step 3
__;
__; TR now contains the address to slide the argument/environment
__; stack towards (we will be shifting things from smaller addresses
__; to larger addresses).
__;
__; H will be one above the last table entry.
__;
__; In this step we will pick up the table entries one by one and
__; move the range of addresses specified by these entries.  In addition,
__; we will store an offset as the third value in the table.  This
__; offset will be used to fix up pointers which point into regions of
__; the stack which we are moving.
__;
__; Register and local variable usage:
__;
__;   saveH  	-- bottom of size/fixup table
__;   H   		-- top of size/fixup table
__;   TR  		-- address into A/E stack to which we move values
__;   EBX  		-- address into A/E stack from which we get values
__;   CompactionRegion  -- initial value of TR; used to determine if a pointer
__;          			needs fixing or not
__;   HB  		-- fixup value for current environment
__;   EAX  		-- scratch
__;   temp  	-- scratch
__;


step3_L0:
	MOVL 	OPRS(HB,IMM(0)) 		_; initially, no fixup needed
	MOVL 	OPRS(CompactionRegion,TR) 	_; Top of compaction region

	CMPL 	OPRS(H,saveH) 			_; see if we are done
	JE 		SDISP(cutcompactiondone)  _; branch if we are done

step3_L1:
	SUBL 	OPRS(H,IMM(12)) 		_; go to previous table entry
	MOVL 	OPRS(EBX,DMADDR(4,H)) 	_; get high address
	MOVL 	OPRS(HB,TR) 			_; compute fixup value
	SUBL 	OPRS(HB,EBX) 			_; now HB has fixup value
	MOVL 	OPRS(DMADDR(8,H),HB) 	_; save fixup value in table entry

step3_L2:
	SUBL 	OPRS(EBX,IMM(4)) 		_; get environment value
	MOVL 	OPRS(EAX,DMADDR(EBX)) 	_; into EAX

	TESTB 	OPRS(AL,IMM(MTP_TAGMASK))	_; perform tag check
	JNE 	SDISP(step3_L4) 			_; branch if anything but reference
	CMPL 	OPRS(EAX,CompactionRegion) 	_; see our pointer >= our start point
	JGE 	SDISP(step3_L4) 		 	_; branch if so
	CMPL 	OPRS(EAX,DMADDR(H)) 		_; see our pointer < low address
	JL 		SDISP(step3_L4) 			_; branch if so

	__; EAX contains a pointer which must be fixed (in compaction region)
	MOVL 	OPRS(temp,H) 			_; save original table entry
step3_L3:
	ADDL 	OPRS(H,IMM(12)) 		_; set up H for next iteration
	CMPL 	OPRS(EAX,DMADDR(-8,H)) 	_; see our pointer < high address
	JGE 	SDISP(step3_L3) 		_; branch if not
	ADDL 	OPRS(EAX,DMADDR(-4,H)) 	_; add in fixup value
	MOVL 	OPRS(H,temp) 			_; restore original table entry
	
step3_L4:
	SUBL 	OPRS(TR,IMM(4)) 		_; move the environment value 
	MOVL 	OPRS(DMADDR(TR),EAX) 	_; into its new position

	CMPL 	OPRS(EBX,DMADDR(H)) 	_; see if more environment value to move
	JNE 	SDISP(step3_L2) 		_; branch if so

	CMPL 	OPRS(H,saveH) 		_; see if totally done
	JNE 	SDISP(step3_L1) 	_; branch if more to do

cutcompactiondone:
	ADDL 	OPRS(E,HB) 			_; add in fixup value 

jmpto_cutcommon:
	MOVL 	OPRS(H,saveH)
	MOVL 	OPRS(TR,saveTR)
	__; Save EDX (H) since cutcommon code uses this register as a temporary
	__; register. This register will be poped from register by "cutcommon"
	__; code in wm_docut routine. Also, restores values of EBX and EDX 
	__; which will be used cutcommon code.
	PUSHL 	H
	MOVL 	OPRS(EBX,saveEBX)
	MOVL 	OPRS(EDX,saveEDX)
	
	JMP 	DISP(cutcommon)			_; jump to cutcommon
	
ENDPROC(wm_cut)


_textend

_end

