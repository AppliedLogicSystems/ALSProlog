	TITLE	$int86
	.386 

;
; int86.s  - Execute an interrupt
;
; Author : Ilyas Cicekli 
; Date   : 4/18/89
;


DGROUP	GROUP CONST, _BSS, _DATA

ASSUME	CS:_TEXT, DS:DGROUP, SS:DGROUP, ES:DGROUP

;
; Locations of registers in REGS structure.
; (see dos.h)
;
REG_EAX 	equ 	0
REG_EBX 	equ 	4
REG_ECX 	equ 	8
REG_EDX 	equ 	12
REG_EBP 	equ 	16
REG_ESI 	equ 	20
REG_EDI 	equ 	24
REG_CFLAG 	equ 	28
REG_EFLAGS 	equ 	32

;
; Locations of segment registers in SREGS structure.
; (see dos.h)
;
SREG_ES 	equ 	0
SREG_CS 	equ 	2
SREG_SS 	equ 	4
SREG_DS 	equ 	6



_DATA	SEGMENT DWORD USE32 PUBLIC RW 'DATA'

inregs 	dd 	? 		; input registers address
outregs dd	? 		; output registers address
segregs dd	? 		; segment registers address

temp 	dd 	? 		; temporary location


		;
		;  Execute Interrupt Instruction
		;
intinst label far
		mov 	eax,4[esp]		; Restore eax register
		db 		0cdh			; opcode of interrupt instruction
intnum  db		?				; interrupt number
		mov  	4[esp],eax  	; Save eax register after interrupt
		ret

_DATA	ENDS

_BSS	SEGMENT	DWORD USE32 PUBLIC 'BSS'
_BSS	ENDS

CONST	SEGMENT DWORD USE32 PUBLIC 'CONST'
CONST	ENDS

_TEXT	SEGMENT DWORD USE32 PUBLIC 'CODE'

;
; int386 : 
; 
; 	This procedure makes MS-DOS system calls or BIOS system calls 
;   by using interrupts.
;
;   Arguments:
;      intno   : interrupt number 
;      inregs  : input registers  (union of regs)
;      outregs : output registers (union of regs)
;
;   Registers in 'inregs' and 'outregs' are stored in the following order.
;      EAX,EBX,ECX,EDX,EBP,ESI,EDI,CFLAG,EFLAGS
;


		align	4
		public	int386  
int386 proc near
		mov 	temp,eax		; save eax in temp

		;
		;  Move the interrupt number into 2nd byte of 
		;  the interrupt instruction
		;
		mov 	al,4[esp]
		mov 	intnum,al

		;
		;  Get addresses of 'inregs' and 'outregs'
		;
		mov 	eax,8[esp]		; address of input registers
		mov 	inregs,eax
		mov 	eax,12[esp]		; address of output registers
		mov 	outregs,eax

		mov 	eax,temp		; restore eax

		;
		;  Save Registers
		;
		pushad		

		;
		; Put given register values in 'inregs' into the corresponding
		; registers. We have to restore 'ebp' register as a last
		; register, because we are going to use it a base register
		; when we load other registers.
		;
		mov 	ebp,inregs 	   		; address of input registers
		mov 	eax,REG_EAX[ebp]	; load eax register
		mov 	ebx,REG_EBX[ebp]	; load ebx register
		mov 	ecx,REG_ECX[ebp]	; load ecx register
		mov 	edx,REG_EDX[ebp]	; load edx register
		mov 	esi,REG_ESI[ebp]	; load esi register
		mov 	edi,REG_EDI[ebp]	; load edi register
		mov 	ebp,REG_EBP[ebp]	; load ebp register

		;
		; 	Make an intersegment call to execute interrupt instruction
		;
		push 	eax					; Save eax register
		lea		eax,intinst
		call 	eax 				; jump to interrupt instruction
		pop 	eax					; restore eax register after interrupt

		;
		;  Put current values in registers into the corresponding
		;  registers in 'outregs'. (Note: all "mov" instructions
		;  don't affect the carry flag, i.e. the carry flag will
		;  have same value returned from interrupt)
		;
		mov 	temp,eax				; save eax in temp
		mov  	eax,ebp					; move ebp to a temporary location
		mov 	ebp,outregs				; address of output registers
		mov 	REG_EBP[ebp],eax   		; save ebp register
		mov 	eax,temp				; restore eax
		mov 	REG_EAX[ebp],eax		; save eax register
		mov 	REG_EBX[ebp],ebx		; save ebx register
		mov 	REG_ECX[ebp],ecx		; save ecx register
		mov 	REG_EDX[ebp],edx		; save edx register
		mov 	REG_ESI[ebp],esi		; save esi register
		mov 	REG_EDI[ebp],edi		; save edi register
		jb  	setcarry 				; go to set carry flag
		mov  	dword ptr REG_CFLAG[ebp],0	; clear carry flag
		jmp 	aftercarry
setcarry:
		mov 	dword ptr REG_CFLAG[ebp],1	; set carry flag
aftercarry:
		pushfd 								; push eflags register
		pop 	dword ptr REG_EFLAGS[ebp] 	; save eflags register

		;
		;  Restore C registers
		;
		popad				

		;
		;  Return to calling procedure
		;
		mov eax,esp
		ret

int386 endp


;
; int386x : 
; 
; 	This procedure makes MS-DOS system calls or BIOS system calls 
;   by using interrupts and segment registers.
;
;   Arguments:
;      intno   : interrupt number 
;      inregs  : input registers  (union of regs)
;      outregs : output registers (union of regs)
;      segregs : output registers (union of regs)
;
;   Registers in 'inregs' and 'outregs' are stored in the following order.
;      EAX,EBX,ECX,EDX,EBP,ESI,EDI,CFLAG,EFLAGS
;
;   Registers in 'segregs' are stored in the following order.
;	   ES,CS,SS,DS
;


		align	4
		public	int386x  
int386x proc near
		mov 	temp,eax		; save eax in temp

		;
		;  Move the interrupt number into 2nd byte of 
		;  the interrupt instruction
		;
		mov 	al,4[esp]
		mov 	intnum,al

		;
		;  Get addresses of 'inregs', 'outregs', 'segregs'
		;
		mov 	eax,8[esp]		; address of input registers
		mov 	inregs,eax
		mov 	eax,12[esp]		; address of output registers
		mov 	outregs,eax
		mov 	eax,16[esp]		; address of segment registers
		mov 	segregs,eax

		mov 	eax,temp		; restore eax

		;
		;  Save All Registers including segment registers
		;
		pushad		
		push 	es
		push 	ss
		push 	ds

		push 	fs				; save fs register
		mov 	ax,ds			; and move ds into fs
		mov 	fs,ax

		;
		; Put given register values in 'inregs' into the corresponding
		; registers. We have to restore 'ebp' register as a last
		; register, because we are going to use it a base register
		; when we load other registers.
		;
		mov 	ebp,fs:segregs			; address of segment registers
		mov 	ax,fs:SREG_DS[ebp]  		; load ds register
		mov		ds,ax
		mov 	ax,fs:SREG_ES[ebp]  		; load ds register
		mov		es,ax

		mov 	ebp,fs:inregs 	   		; address of input registers
		mov 	eax,fs:REG_EAX[ebp]		; load eax register
		mov 	ebx,fs:REG_EBX[ebp]		; load ebx register
		mov 	ecx,fs:REG_ECX[ebp]		; load ecx register
		mov 	edx,fs:REG_EDX[ebp]		; load edx register
		mov 	esi,fs:REG_ESI[ebp]		; load esi register
		mov 	edi,fs:REG_EDI[ebp]		; load edi register
		mov 	ebp,fs:REG_EBP[ebp]		; load ebp register

		;
		; 	Make an intersegment call to execute interrupt instruction
		;
		push 	eax					; Save eax register
		lea		eax,intinst
		call 	eax 				; jump to interrupt instruction
		pop 	eax					; restore eax register after interrupt

		;
		;  Put current values in registers into the corresponding
		;  registers in 'outregs'. (Note: all "mov" instructions
		;  don't affect the carry flag, i.e. the carry flag will
		;  have same value returned from interrupt)
		;
		mov 	fs:temp,eax				; save eax in temp

		mov 	ebp,fs:segregs			; address of segment registers
		mov		ax,ds
		mov 	fs:SREG_DS[ebp],ax  		; restore ds register
		mov		ax,es
		mov 	fs:SREG_ES[ebp],ax	  	; restore ds register

		mov  	eax,ebp					; move ebp to a temporary location
		mov 	ebp,fs:outregs			; address of output registers
		mov 	fs:REG_EBP[ebp],eax   	; save ebp register
		mov 	eax,fs:temp				; restore eax
		mov 	fs:REG_EAX[ebp],eax		; save eax register
		mov 	fs:REG_EBX[ebp],ebx		; save ebx register
		mov 	fs:REG_ECX[ebp],ecx		; save ecx register
		mov 	fs:REG_EDX[ebp],edx		; save edx register
		mov 	fs:REG_ESI[ebp],esi		; save esi register
		mov 	fs:REG_EDI[ebp],edi		; save edi register
		jb  	xsetcarry 				; go to set carry flag
		mov  	dword ptr fs:REG_CFLAG[ebp],0	; clear carry flag
		jmp 	xaftercarry
xsetcarry:
		mov 	dword ptr fs:REG_CFLAG[ebp],1	; set carry flag
xaftercarry:
		pushfd 								; push eflags register
		pop 	dword ptr REG_EFLAGS[ebp] 	; save eflags register

		;
		;  Restore C registers
		;
		pop  	fs				

		pop  	ds
		pop  	ss
		pop  	es

		popad				

		;
		;  Return to calling procedure
		;
		mov eax,esp
		ret

int386x endp



;
; segread386(&segregs)  -- read segment registers
;
;
		align 4
		public 	segread386
segread386 proc near

		; save address of SREGS structure
		mov 	temp,eax
		mov 	eax,4[esp]		; address of input registers
		mov 	segregs,eax
		mov 	eax,temp

		; save registers
		pushad

		; get segment registers
		mov 	ebp,segregs
		mov 	ax,es
		mov 	SREG_ES[ebp],ax
		mov 	ax,cs
		mov 	SREG_CS[ebp],ax
		mov 	ax,ss
		mov 	SREG_SS[ebp],ax
		mov 	ax,ds
		mov 	SREG_DS[ebp],ax

		; restore registers
		popad

		; return
		mov 	eax,esp
		ret

segread386 endp

;
; movedata(srcseg,src,destseg,dest,size) 
;
; move data from one segment to another segment
;
;
		align 4
		public 	movedata
movedata proc near

		mov 	temp,esp

		;
		; Save Registers
		;
		pushad
		push ds
		push es
		push fs

		;
		; Move ds into fs, get the original stack pointer into 'ebp'
		;
		mov  	ax,ds
		mov		fs,ax
		mov 	ebp,temp

		; Get segment and address of source
		mov 	ax,fs:4[ebp]
		mov 	ds,ax
		mov 	esi,fs:8[ebp]

		; Get segment and address of destination
		mov 	ax,fs:12[ebp]
		mov 	es,ax
		mov 	edi,fs:16[ebp]

		; Get size 
		mov 	ecx,fs:20[ebp]

		; Copy it
		rep 	movsb

		; Restore registers
		pop fs
		pop es
		pop ds
		popad

		; Return
		mov 	eax,esp
		ret

movedata endp

_TEXT	ends

END

