	FILE(salloc)
__;
__; 	salloc.s	-- 	routines to change the stack and 
__;				dynamically allocate storage in the stack.
__;
__;	Copyright (c) Applied Logic Systems, Inc.
__;
__;	Author: Ilyas Cicekli
__;	Date  : 11/20/1989
__;
__;
__;	Routines:	
__;		stk_init_alloc
__;		stk_alloc
__;		stk_change
__;


_dgroup

_assume

_datastart

DD(tempebx,0)
DD(tempecx,0)
DD(tempedx,0)
DD(tempesi,0)
DD(tempedi,0)

_dataend

_bssstart
_bssend

_conststart
_constend


_textstart



__;**************************************************************************
__;
__;	stk_init_alloc(sizeofvars)
__;
__;	Return the address "lastvaraddr"  as the stack allocation
__;	location for the calling procedure if the "lastvaraddr" is a location 
__;	in the stack frame of the calling procedure.
__;	User should make sure that the "sizeofvars" is the size of
__;	variables declared in the calling procedure.
__;	This procedure will only check whether the given "lastvaraddr"
__;	is between locations pointed by registers "ebp" and "esp".
__;	This will only guarantee that the address given by the user points
__;	to a variable declared in the calling procedure or a temporay
__;	stack location used in the calling procedure.
__;	If the given address is not in the stack frame of the calling
__;	procedure, "0" will be returned; Otherwise the given address
__;	will be returned as dynamic stack allocation location.
__;
__;**************************************************************************


PROC(stk_init_alloc)

		__; Save registers 

		MOVL	OPRS(tempebx,EBX)
		MOVL	OPRS(tempecx,ECX)

		__; Get "lastvaraddr" into register eax
		   
		MOVL	OPRS(EBX,MADDR(4,ESP)) _; size variables (in long words)
		SHLL 	OPRS(EBX,IMM(2))       _; in bytes
		MOVL	OPRS(EAX,EBP)		_; stack frame of calling proc
		SUBL	OPRS(EAX,EBX)		_; last variable address

		ANDB	OPRS(AL,IMM(HEX(fc))) 	_; double word boundary
		   
		__; Get boundries of the stack frame of the calling procedure.
		__; Registers "ebx" and "ecx" will point to upper boundry, and 
		__; to lower boundary, respectively.
		   
		MOVL	OPRS(EBX,EBP) 		_; ebp - 4 = upper boundary
		SUBL	OPRS(EBX,IMM(4)) 	_; 4 ( a double word) is for saved ebp value
		MOVL	OPRS(ECX,ESP)		_; esp + 8 = lower boundary
		ADDL	OPRS(ECX,IMM(8))	_; 8 (two double words) is for return address 
						_; and argument of this procedure
		   
		__;	Check whether "lastvaraddr" is in the stack frame 
		__; 	of the calling procedure, i.e. the following condition
		__; 	should satisfy.
		__;			lower_boundry <= lastvaraddr <= upper_boundary
		   
		CMPL	OPRS(EAX,EBX)	_; if lastvaraddr > upper_boundary
		JG	SDISP(stk_init_alloc_error) _; go to "error"
		CMPL	OPRS(EAX,ECX)	_; if lastvaraddr < lower_boundary
		JL	SDISP(stk_init_alloc_error)  _; go to "error"
		JMP	SDISP(stk_init_alloc_return) _; it is in the stack frame

		__;	"lastvaraddr" is not in the stack frame of the calling
		__;	procedure, return "0"

stk_init_alloc_error:
		SUBL	OPRS(EAX,EAX)

		__;	Restore registers "ebx" and "ecx", and return

stk_init_alloc_return:
		MOVL	OPRS(EBX,tempebx)
		MOVL	OPRS(ECX,tempecx)
		RET

ENDPROC(stk_init_alloc)





__;**************************************************************************
__;
__;	stk_alloc(size,stk_allocloc)
__;
__; Dynamically allocate storage in the stack after the given
__;	stack allocation location of the calling procedure
__;	if the given "stk_allocloc" is in the stack frame of 
__;	the calling procedure. If the storage can be allocated,
__;	a pointer to the lowest double word of the newly allocated
__;	storage will be returned, otherwise "0" will be returned.
__; Although the given size must be in bytes, the allocation will be
__;	made in doble word boundaries.
__;
__;
__;	Stack at Beginning of This Procedure
__;
__;		+----------------------+
__;		| args of calling proc |
__;		+----------------------+
__;		| return address of    |
__;		| calling proc         |
__;		+----------------------+
__;		| saved ebp value      | <== ebp
__;		+----------------------+
__;		|   variables of       |  
__;		|   calling proc       | <== stk_allocloc (hopefully)
__;		+----------------------+
__;		|   pushed temporaries |
__;		|   of calling proc    | 
__; 		+----------------------+
__;		|   stk_allocloc       |
__;		+----------------------+
__;		|   size in bytes      |
__;		+----------------------+
__;		| return address of    |
__;		| this proc            | <== esp
__;		+----------------------+
__;
__;
__;	Stack at End of This Procedure
__;
__;		+----------------------+
__;		| args of calling proc |
__;		+----------------------+
__;		| return address of    |
__;		| calling proc         |
__;		+----------------------+
__;		| saved ebp value      | <== ebp
__;		+----------------------+
__;		|   variables of       |  
__;		|   calling proc       |
__;		+----------------------+
__;		|                      |
__;		|   newly allocated    |
__;		|   storage            | 
__;		|                      | <== return value from this procedure
__;		+----------------------+
__;		|   pushed temporaries |
__;		|   of calling proc    | 
__; 		+----------------------+
__;		|   stk_allocloc       |
__;		+----------------------+
__;		|   size in bytes      |
__;		+----------------------+
__;		| return address of    |
__;		| this proc            | <== esp
__;		+----------------------+
__;
__;**************************************************************************


PROC(stk_alloc)

		   
		__;	Save registers 
		   
		MOVL	OPRS(tempebx,EBX)
		MOVL	OPRS(tempecx,ECX)
		MOVL	OPRS(tempedx,EDX)
		MOVL	OPRS(tempesi,ESI)
		MOVL	OPRS(tempedi,EDI)
		   
		__;	Get "size" into register "edx", and 
		__;	"stk_allocloc" into register "eax"
		   
		MOVL	OPRS(EDX,MADDR(4,ESP)) 
		MOVL	OPRS(EAX,MADDR(8,ESP))
		ANDB	OPRS(AL,IMM(HEX(fc)))		_; double word boundary
		   
		__; Get boundries of the stack frame of the calling procedure.
		__; Registers "ebx" and "ecx" will point to upper boundry, and 
		__; to lower boundary, respectively.
		   
		MOVL	OPRS(EBX,EBP)		_; ebp - 4 = upper boundary
		SUBL	OPRS(EBX,IMM(4))	_; 4 ( a double word) is for saved ebp value
		MOVL	OPRS(ECX,ESP)		_; esp + 12 = lower boundary
		ADDL	OPRS(ECX,IMM(12))	_; 12 (three double words) is for return address 
						_; and two arguments of this procedure
		   
		__;	Check whether "stk_allocloc" is in the stack frame 
		__; 	of the calling procedure, i.e. the following condition
		__; 	should satisfy.
		__;	lower_boundry <= stk_allocloc <= upper_boundary
		   
		CMPL	OPRS(EAX,EBX)		_; if stk_allocloc > upper_boundary
		JG	SDISP(stk_alloc_error) 	_; go to "error"
		CMPL	OPRS(EAX,ECX)		_; if stk_allocloc < lower_boundary
		JL  	SDISP(stk_alloc_error) 	_; go to "error"
		   
		__; "stk_allocloc" is in the stack frame of the calling procedure
		__; Adjust allocation "size" (in register edx) if necessary.
		__; (allocation will be on double word boundries).
		   
		MOVL	OPRS(ECX,EDX) 		_; move "size" to ecx		
		ANDB   	OPRS(CL,IMM(HEX(3)))	_; check if it is a multiple of double words
		JZ 	SDISP(stk_alloc_continue)	_; if it is, continue
		ANDB	OPRS(DL,IMM(HEX(fc)))		_; Otherwise adjust it
		ADDL	OPRS(EDX,IMM(4))

stk_alloc_continue:
		   
		__; Shift stack segment between "stk_allocloc" (not included)
		__; and "esp" (included) "size" bytes. Contents of stack locations 
		__; between "stk_allocloc" and "stk_alloc" will be moved to
		__; their new locations between "stk_alloc-size" and "esp-size".
		   
		MOVL	OPRS(ECX,EAX)	_; move "stk_allocloc" to ecx
		SUBL	OPRS(ECX,ESP)	_; stk_alloc - esp = # of bytes to be copied
		MOVL	OPRS(ESI,ESP)	_; esp = copy source start address
		MOVL	OPRS(EDI,ESI)	_; esp - size = 
		SUBL	OPRS(EDI,EDX)	_;    copy destination start address

		__; allocate storage in the stack
		   
		SUBL	OPRS(ESP,EDX)	_; new esp = esp -size

		SHRL	OPRS(ECX,IMM(2))   _; # of double words to be copied

		CLD						_; clear direction bit
		REP(MOVSD)				_; copy starting from low memory to high memory
		   
       	SUBL    OPRS(EAX,EDX)   _; return value = stk_allocloc - size

		JMP	SDISP(stk_alloc_return)
		    
		__;	"stk_allocloc" is not in the stack frame of the calling
		__;	procedure, return "0"
		    
stk_alloc_error:
		SUBL	OPRS(EAX,EAX)
		    
		__;	Restore registers and return
		    
stk_alloc_return:
		MOVL	OPRS(EBX,tempebx)
		MOVL	OPRS(ECX,tempecx)
		MOVL	OPRS(EDX,tempedx)
		MOVL	OPRS(ESI,tempesi)
		MOVL	OPRS(EDI,tempedi)
		RET 

ENDPROC(stk_alloc)





__;**************************************************************************
__;
__;	stk_change(newstack,numargs)
__;
__; 	Copy the stack frame of the calling procedure into the new stack, 
__;	and update registers "ebp" and "esp" to point to the new stack.
__;
__;**************************************************************************


PROC(stk_change)

		   
		__;	Save registers 
		   
		MOVL	OPRS(tempebx,EBX)
		MOVL	OPRS(tempecx,ECX)
		MOVL	OPRS(tempedx,EDX)
		MOVL	OPRS(tempesi,ESI)
		MOVL	OPRS(tempedi,EDI)
		   
		__;	Get "numargs" into register "edx", and 
		__;	"newstack" into register "eax"
		   
		MOVL	OPRS(EAX,MADDR(4,ESP))
		MOVL	OPRS(EDX,MADDR(8,ESP))
		ANDB	OPRS(AL,IMM(HEX(fc)))	_; double word boundary
		   
		__; 	Get number of bytes in the stack frame of the calling procedure
		__; 	to be copied to the new stack.
		__;	Register "ecx" will hold that value.
		   
		MOVL	OPRS(ECX,EBP)		_; move "ebp" to ecx
		ADDL	OPRS(ECX,IMM(8))	_; two double words (saved ebp and return addr)
		SHLL	OPRS(EDX,IMM(2))
		ADDL	OPRS(ECX,EDX)		_; add # of arguments of calling procedures
		SUBL	OPRS(ECX,ESP)	_; (ebp + 8 + #ofArgs*4) - esp  = 
					_; 	# of bytes in the stack frame
					_;	of calling procedure
		   
		__;	Copy stack frame of the calling procedure into new stack
		__;	and save new "esp" value in "eax"
		   
		MOVL	OPRS(ESI,ESP)	_; esp = copy source start address
		MOVL	OPRS(EDI,EAX)	_; newstack - size of stack frame = 
		SUBL	OPRS(EDI,ECX) 	_;    copy destination start address
		MOVL	OPRS(EAX,EDI)	_; new esp value in"eax"
		SHRL	OPRS(ECX,IMM(2))  _; # of double words to be copied
		CLD			_; clear direction bit
		REP(MOVSD)	_; copy starting from low memory to high memory
		   
		__;	Finally, switch to the new stack by updating
		__;	"ebp" and "esp" registers
		   
		MOVL	OPRS(EBX,EBP)
		SUBL	OPRS(EBX,ESP)	_; ebx = old ebp - old esp
		MOVL	OPRS(ESP,EAX)	_; new esp
		MOVL	OPRS(EBP,ESP)
		ADDL	OPRS(EBP,EBX)	_; new ebp = new esp + (old ebp - old esp)
		   
		__; 	Restore registers, and return
		   
		MOVL	OPRS(EBX,tempebx)
		MOVL	OPRS(ECX,tempecx)
		MOVL	OPRS(EDX,tempedx)
		MOVL	OPRS(ESI,tempesi)
		MOVL	OPRS(EDI,tempedi)
		RET 

ENDPROC(stk_change)


_textend

_end

