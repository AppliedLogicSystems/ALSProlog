	TITLE	$cntrlc
	.386p 

;
; cntrlc.s   -- Control/C Interrupt Handler for MS-DOS
;				(For both PharLap Environment and AI Arhitectures Environment)
;
;************ CONTROL/BREAK HANDLER, NOT CONTROL/C  ******************
;
; Author : Ilyas Cicekli 
; Date   : 6/21/89
;
;


INCLUDE sconfig.h


extrn	cntrlc:near



DGROUP	GROUP _TEXT, CONST, _BSS, _DATA


ASSUME	CS:DGROUP, DS:DGROUP, SS:DGROUP, ES:DGROUP


_BSS	SEGMENT	DWORD USE32 PUBLIC 'BSS'
_BSS	ENDS


CONST	SEGMENT DWORD USE32 PUBLIC 'CONST'
CONST	ENDS


_DATA	SEGMENT DWORD USE32 PUBLIC 'DATA'

;
; Original values of Control/C interrupt vectors in the real mode
; and in the protected mode.
;
real_vector		dd	?
prot_vector		dd 	?
prot_vector_seg	dw 	?

_DATA	ENDS



_TEXT	SEGMENT DWORD USE32 PUBLIC 'CODE'



IFDEF 	PharLap

;
;**********************************************************************
;			For Phar Lap Environment								  *
;**********************************************************************
;
;**********************************************************************
;*       THIS IS CONTROL/BREAK HANDLER, NOT CONTROL/C                 *
;**********************************************************************
;
; cntrlc_handler()
;
; New Control/C Interrupt handler.
;
		align 	4
		public	cntrlc_handler
cntrlc_handler	proc near

		;
		; 	Disable interrupts until the end of this procedure
		; 	And Save registers
		;
		cli						; clear interrupt flag
		pushad
		;
		; 	Now call the C function "cntrlc" to take care of
		; 	Control/C interrupt.
		;
		call	cntrlc			; Call the C function
		;
		; Restore registers
		; We are done. Enable interrupts.
		;
		popad
		sti						; set interrupt flag
		;
		; Now we can return from the Control/C interrupt handler.
		; Since when Control/C interrupts occurr, the 80386 processor
		; saves information in a 16-bit form in the real mode, and
		; in a 32-bit form in the protected mode.
		; So we have to choose the correct interrupt return instruction
		; to pop those values from the stack;
		; We have to check in which mode we are running now.
		;
		push 	eax 			; we need a temporary register
		mov 	eax,CR0			; Get Machine Status word
		cmp 	eax,01h			; Check Protection Enable bit (bit-0)
								; of Machine Status Word
		jz 		realmode		
			;
			; 	We are in the protected mode
			;
		pop 	eax 			; restore eax
		iretd					; interrupt return in the protected mode
			;
			; 	We are in the real mode
			;
realmode:
		pop 	eax				; restore eax
		iret					; interrupt return in the real mode

cntrlc_handler endp



;
; cntrlc_init()
;
; Initilaze Control/C interrupt vectors in the real mode and 
; the protected mode to take over that interrupt.
;
		align	4
		public	cntrlc_init
cntrlc_init	proc	near
		;
		;	Disable interrupts and
		; 	Save Registers
		;
		cli
		pushad
		;
		; Turn CTRL/Break checking off
		;
		mov 	ax,03301h
		mov 	dl,0
		int  	021h
		;
		; Put the number of Control/C interrupt into register "cl".
		; The register "cl" will be used following DOS-extender
		; system calls to get and to set interrupt vectors for
		; Control/C interrupt for both protected and real modes.
		;
		mov 	cl,01bh			; Number of Control/C Interrupt
			;
			; Get addresses of original protected and real mode 
			; interrupt vectors and save them, 
			; so that they can be restored later.
			;
		push 	es				; save register "es"
		mov 	ax,02502h 		; Function number to get protected 
								; mode interrupt vector
		int 	021h			; Get protected mode interrupt vector of
								; Control/C interrupt
		mov 	prot_vector,ebx 	; Save its address 
		mov 	prot_vector_seg,es
		pop 	es				; restore register "es"

		mov 	ax,02503h		; Function number to get real 
								; mode interrupt vector
		int 	021h			; Get real mode interrupt vector of
								; Control/C interrupt
		mov 	real_vector,ebx ; Save it
			;
			; Set addresses of new protected and real mode interrupt
			; handler routines. 
			; The function "cntrlc_handler" will be the new
			; interrupt handler routine for the Cotrol/C interrupt.
			;
		push 	ds				; save register ds
		push 	cs				; Get the segment of new interrupt vector
		pop 	ds				; and put into register "ds"
		mov 	edx,offset cntrlc_handler 	; New interrupt handler for 
											; protected and real mode
		mov 	ax,02506h		; Function number to set protected and
								; real mode interrupt vectors to always
								; gain control in protected mode
		int 	021h			; Set them
		pop 	ds				; restore register "ds"

		;
		; Turn CTRL/Break checking on 
		;
		mov 	ax,03301h
		mov 	dl,1
		int  	021h
		;
		; Restore Registers
		; Enable interrupts
		;
		popad
		sti 

		ret						; return

cntrlc_init endp


;
; cntrlc_reset()
;
; Reset Control/C interrupt vectors in the real mode and the protected
; mode to their original values.
;
		align 4
		public cntrlc_reset
cntrlc_reset	proc near
		;
		; Save Registers
		;
		pushad
		;
		; Reset addresses of new protected and real mode interrupt
		; handler routines for Control/C interrupt from saved values. 
		;
		mov 	cl,01bh			; Number of Control/C Interrupt
		mov 	edx,prot_vector		; Original interrupt handlers for protected
		mov		ebx,real_vector		; and real mode
		mov 	ax,02507h		; Function number to set protected and
								; real mode interrupt vectors
		push 	ds
		mov 	ds,prot_vector_seg
		int 	021h			; Set them
		pop 	ds
		;
		; Restore Registers
		;
		popad

		ret						; return

cntrlc_reset	endp


ENDIF

;
;*************	End of Phar Lap Environment 	***********************
;





IFDEF 	Ergo  

;**********************************************************************
;			For AI Architectures ENvironment						  *
;**********************************************************************
;
;**********************************************************************
;*       THIS IS CONTROL/BREAK HANDLER, NOT CONTROL/C                 *
;**********************************************************************
;
; cntrlc_handler()
;
; New Control/C Interrupt handler.
;
		align 	4
		public	cntrlc_handler
cntrlc_handler	proc near

		;
		;	Disable interrupts and save registers
		;
		cli
		pushad
		push 	ds
		;
		;	Make sure that we get correct data segment
		;
		mov 	ax,017h
		mov 	ds,ax
		;
		; 	Now call the C function "cntrlc" to take care of
		; 	Control/Break interrupt.
		;	
		call	cntrlc		
		;
		;	Enable interrupts, restore registers, and return
		;
		pop		ds
		popad
		sti
		iretd	

cntrlc_handler endp



;
; cntrlc_init()
;
; Initilaze Control/C interrupt vectors in the real mode and 
; the protected mode to take over that interrupt.
;
		align	4
		public	cntrlc_init
cntrlc_init	proc	near

		cli

		; Save Registers

		pushad

		; Turn CTRL/Break checking off
		mov 	ax,03301h
		mov 	dl,0
		int  	021h

		; get interrupt vector of Control/Break 

		push 	es					; save register "es"
		mov 	ax,0351bh			; Number of Control/Break Interrupt
		int 	021h
		mov 	prot_vector,ebx 	; Save its address 
		mov 	prot_vector_seg,es
		pop 	es				; restore register "es"

		push 	ds
		mov 	ax,0251bh			; Number of Control/C Interrupt
		lea 	edx,cntrlc_handler
		push 	cs
		pop 	ds
		int 	021h			; Set them
		pop 	ds

		; Turn CTRL/Break checking on 
		mov 	ax,03301h
		mov 	dl,1
		int  	021h

		popad

		sti 

		ret						; return

cntrlc_init endp


;
; cntrlc_reset()
;
; Reset Control/C interrupt vectors in the real mode and the protected
; mode to their original values.
;
		align 4
		public cntrlc_reset
cntrlc_reset	proc near

		; Save Registers

		pushad

		; Reset addresses of new protected and real mode interrupt
		; handler routines for Control/C interrupt from saved values. 

		push 	ds
		mov 	ax,0251bh			; Number of Control/C Interrupt
		mov 	edx,prot_vector		; Original interrupt handlers for protected
		mov 	ds,prot_vector_seg
		int 	021h			; Set them
		pop 	ds

		; Restore Registers

		popad

		ret						; return

cntrlc_reset	endp

;
;*************	End of AI Arhitectures Environment 	***********************
;



;**********************************************************************
;			For AI Architectures VM Environment						  *
;**********************************************************************
;

;
; cr_codeseg
;
;	Create a new code window in the data segment (segment 17).
;

extrn	xseg_call_selector:word

		align 4
		public cr_codeseg
cr_codeseg proc near
		pushad

		;
		; Get the size of the data segment
		;
		mov 	ah,0edh
		mov 	bx,ds
		int 	021h

		;
		; Create a new data window in the data segment,
		; Size of this new data window is same is it parent.
		;
		mov 	ax,0e801h
		mov 	si,0
		mov 	bx,0
		int 	021h
		mov 	xseg_call_selector,ax

		;
		; Convert it into a code window
		;
		mov 	bx,xseg_call_selector
		mov 	ax,0e901h
		int 	021h

		popad

		ret

cr_codeseg endp

;
;*************	End of AI Arhitectures VM Environment 	***********************
;

ENDIF


_TEXT 	ends

END
