	TITLE	INTHAND
	.386p 
;;;
;;; inthand.s -- Interrupt handlers and routines to install interrupt handlers 
;;;			     (In PharLap and Ergo Environments)
;;;
;;; Copyright (c) Applied Logic Systems, Inc.
;;;
;;; Author : Ilyas Cicekli 
;;; Date   : 10/24/90
;;;
;;;

;;
;; Order of segments
;;	
_TEXT	SEGMENT	BYTE PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT	DWORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT	DWORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT DWORD PUBLIC 'BSS'
_BSS	ENDS
STACK 	SEGMENT  PARA STACK 'STACK'
STACK 	ENDS

DGROUP	GROUP	_DATA, CONST, _BSS


;;
;; Get the configuration information
;;
INCLUDE sconfig.h

;;
;; Global Variables used in this file
;;
;;extrn	cntrlc:near

extrn 	wm_regidx:dword
extrn 	wm_safety:dword
extrn 	wm_trigger:dword
extrn	wm_interrupt_caught:dword

;;
;; Define Prolog interrupt number for Cntrl/Break interrupt
;; (see file sig.c)
;;
SIGINT 	EQU 	2

;;
;; Offsets in the structure to store previous interrupt handler
;;
PROT_VEC_ADDR 		EQU 	0
PROT_VEC_SEG 		EQU		4
REAL_VEC_ADDR		EQU 	6

;;
;; Segment Selectors
;; 
PROT_DATASEG	EQU 	word ptr cs:prot_data_seg_selector
PROT_CODESEG	EQU 	word ptr cs:prot_code_seg_selector

_TEXT	SEGMENT 
ASSUME	CS:_TEXT, DS:DGROUP, SS:DGROUP, ES:DGROUP 


IFDEF PharLap

;************************************************************************
;				For PharLap Environment									*
;************************************************************************

;;
;; Protected mode data segment
;;
;PROT_DATASEG	EQU 	014h
;PROT_DATASEG	EQU 	017h


;;
;; init_int_handler(intnum,inthand,previnthand)
;;
;; INPUTS:
;;		intnum	: Interrupt Number
;;		inthand	: Address of the interrupt handler routine
;;
;; OUTPUT:
;;		previnthand : address of the structure to store previous protected and
;;					  real mode interrupt vectors. This structure must be
;;					  defined as follows by the calling procedure (in C).
;;							struct {
;;								long 	prot_vector_addr;
;;								short 	prot_vector_seg;
;;								long	real_vector_addr
;;							}
;;
		align	4
		public	init_int_handler
init_int_handler	proc	near
		;;
		;; Disable interrupts, and save registers
		;;
		cli
		push 	ebp
		mov 	ebp, esp
		pushad
		;;
		;; Put the address of structure to store previous interrupt handler 
		;; into the register EDI.
		;;
		mov 	edi, dword ptr [ebp].16		;; Address of PrevIntHand Struc
		;;
		;; Get and store the address of protected mode interrupt vector
		;;
		push 	es		
		mov 	cl, byte ptr [ebp].8 		;; Interrupt Number
		mov 	ax, 02502h 		
		int 	021h	
		mov 	dword ptr [edi].PROT_VEC_ADDR, ebx 	
		mov 	word ptr [edi].PROT_VEC_SEG, es
		pop 	es				
		;;
		;; Get and store the address of real mode interrupt vector
		;;
		mov 	cl, byte ptr [ebp].8 		;; Interrupt Number
		mov 	ax, 02503h		
		int 	021h		
		mov 	dword ptr [edi].REAL_VEC_ADDR, ebx
		;;
		;; Set addresses of new protected and real mode interrupt
		;; handler routines. 
		;;
		push 	ds				
		mov 	cl, byte ptr [ebp].8 		;; Interrupt Number
		mov 	edx, dword ptr [ebp].12 	;; Interrupt Handler
		push 	cs		
		pop 	ds	
		mov 	ax, 02506h		
		int 	021h		
		pop 	ds				
		;;
		;; Restore registers, and enable interrupts
		;;
		popad
		mov 	esp,ebp
		pop	 	ebp
		sti 
		ret					
init_int_handler endp



;;
;; reset_int_handler(intnum,previnthand)
;;
;; INPUTS:
;;		intnum		: Interrupt Number
;;		previnthand : Address of the structure to store previous protected and
;;					  real mode interrupt vectors. This structure must be
;;					  defined as follows by the calling procedure (in C).
;;							struct {
;;								long 	prot_vector_addr;
;;								short 	prot_vector_seg;
;;								long	real_vector_addr
;;							}
;;
;;
		align 4
		public reset_int_handler
reset_int_handler	proc near
		;;
		;; Save Registers
		;;
		push 	ebp
		mov 	ebp, esp
		pushad
		;;
		;; Reset addresses of protected and real mode interrupt
		;; vectors of the given interrupt from given values. 
		;;
		mov 	cl, byte ptr [ebp].8		;; Interrupt number
		mov 	edi, dword ptr [ebp].12		;; Address of PrevIntHand Struc
		mov		ebx, dword ptr [edi].REAL_VEC_ADDR
		mov 	edx, dword ptr [edi].PROT_VEC_ADDR
		push 	ds
		mov 	ds, word ptr [edi].PROT_VEC_SEG 
		mov 	ax, 02507h	
		int 	021h
		pop 	ds
		;;
		;; Restore Registers
		;;
		popad
		mov 	esp,ebp
		pop 	ebp
		ret
reset_int_handler	endp

ENDIF

;*************  End of PharLap Environment  *****************************





IFDEF Ergo

;************************************************************************
;						For Ergo Environment							*
;************************************************************************

;;
;; Protected mode data segment
;;
;PROT_DATASEG	EQU 	017h


;;
;; init_int_handler(intnum,inthand,previnthand)
;;
;; INPUTS:
;;		intnum	: Interrupt Number
;;		inthand	: Address of the interrupt handler routine
;;
;; OUTPUT:
;;		previnthand : address of the structure to store previous protected and
;;					  real mode interrupt vectors. This structure must be
;;					  defined as follows by the calling procedure (in C).
;;							struct {
;;								long 	prot_vector_addr;
;;								short 	prot_vector_seg;
;;							}
;;
		align	4
		public	init_int_handler
init_int_handler	proc	near
		;;
		;; Disable interrupts, and save registers
		;;
		cli
		push 	ebp
		mov 	ebp, esp
		pushad
		;;
		;; Put the address of structure to store previous interrupt handler 
		;; into the register EDI.
		;;
		mov 	edi, dword ptr [ebp].16		;; Address of PrevIntHand Struc
		;;
		;; Get and store the address of protected mode interrupt vector
		;;
		push 	es		
		mov 	al, byte ptr [ebp].8
		mov 	ah, 035h 		
		int 	021h	
		mov 	dword ptr [edi].PROT_VEC_ADDR, ebx 	
		mov 	word ptr [edi].PROT_VEC_SEG, es
		pop 	es				
		;;
		;; Set the new interrupt handler. 
		;;
		push 	ds				
		mov 	al, byte ptr [ebp].8		;; Interrupt Number
		mov 	edx, dword ptr [ebp].12		;; Interrupt Handler
		push 	cs		
		pop 	ds	
		mov 	ah, 025h 		
		int 	021h		
		pop 	ds				
		;;
		;; Restore registers, and enable interrupts
		;;
		popad
		mov 	esp, ebp
		pop 	ebp
		sti 
		ret					
init_int_handler endp



;;
;; reset_int_handler(intnum,previnthand)
;;
;; INPUTS:
;;		intnum		: Interrupt Number
;;		previnthand : Address of the structure to store previous protected and
;;					  real mode interrupt vectors. This structure must be
;;					  defined as follows by the calling procedure (in C).
;;							struct {
;;								long 	prot_vector_addr;
;;								short 	prot_vector_seg;
;;							}
;;
;;
		align 4
		public reset_int_handler
reset_int_handler	proc near
		;;
		;; Save Registers
		;;
		push 	ebp
		mov  	ebp, esp
		pushad
		;;
		;; Restore the original interrupt vector. 
		;;
		mov 	al, byte ptr [ebp].8 		;; Interrupt Number
		mov 	edi, dword ptr [ebp].12		;; Address of PrevIntHand Struc
		mov 	edx, dword ptr [edi].PROT_VEC_ADDR
		push 	ds
		mov 	ds, word ptr [edi].PROT_VEC_SEG 
		mov 	ah, 025h 		
		int 	021h
		pop 	ds
		;;
		;; Restore Registers
		;;
		popad
		mov 	esp, ebp
		pop 	ebp
		ret
reset_int_handler	endp



ENDIF

;*************  End of Ergo Environment  ****************




;;
;; set_prolog_interrupt(IntNum)
;;
;; Set a Prolog interrupt so that the next goal can be interrupted
;;
		align 	4
		public	set_prolog_interrupt
set_prolog_interrupt proc near
		;;
		;; Save registers
		;;
		push 	ebp
		mov 	ebp, esp
		push 	fs
		push 	eax
		;;
		;; Put the protected mode data segment selector into FS
		;;
		mov 	ax, PROT_DATASEG
		mov 	fs, ax
		;;
		;; Set Prolog interrupt
		;;
		mov 	eax, DWORD PTR [ebp].8 					;; Interrupt Number
		mov 	DWORD PTR fs:wm_interrupt_caught, eax
;;		mov 	eax, DWORD PTR fs:wm_trigger
;;		mov 	DWORD PTR fs:wm_safety, eax
		mov 	DWORD PTR fs:wm_safety, -1
		;;
		;; Restore registers and return
		;;
		pop 	eax
		pop 	fs
		mov 	esp, ebp
		pop 	ebp
		ret
set_prolog_interrupt endp



;;
;; cntrlc_handler()
;;
;; Control/BREAK Interrupt handler.
;;
		align 	4
		public	cntrlc_handler
cntrlc_handler	proc near
		;;
		;;	Disable interrupts and save registers
		;;
		cli
		pushad
		push 	ds
		push 	es
		;;
		;;	Make sure that we get correct data segment
		;;
		mov 	ax, PROT_DATASEG
		mov 	ds, ax
		mov 	es, ax
		;;
		;; 	Now call the C function "cntrlc" to take care of
		;; 	Control/Break interrupt.
		;;  Set Prolog Interrupt her instead of at the function 
		;;  "cntrlc" 		-- Ilyas 5/20/91
		;;
		;;call	cntrlc		
		cmp 	DWORD PTR wm_regidx, 0 			;; In Prolog ?
		je  	cntrlc_not_in_prolog	;; Not in prolog, don't d anything	
		mov 	DWORD PTR wm_interrupt_caught, SIGINT
		mov 	eax, DWORD PTR wm_trigger
		mov 	DWORD PTR wm_safety, eax
cntrlc_not_in_prolog:
		;;
		;;	Enable interrupts, restore registers, and return
		;;
		pop		es
		pop		ds
		popad
		sti
		iretd	
cntrlc_handler endp



		align 	4
		public 	prot_data_seg_selector
prot_data_seg_selector	dw 	017h
		public 	prot_code_seg_selector
prot_code_seg_selector	dw 	07h

		;;
		;; Mark end of cntrlc_handler.
		;; We will use this information when we are locking interrupt
		;; handler in memory.
		;;
		public	endof_cntrlc_handler
endof_cntrlc_handler:



		align 4
		public save_seg_selectors
save_seg_selectors	proc near
		;;
		;; Save Registers
		;;
		push 	ebp
		mov 	ebp, esp
		pushad
		;;
		;; Save Segment Selectors
		;;
		mov 	word ptr ds:prot_data_seg_selector, ds
		mov 	word ptr ds:prot_code_seg_selector, cs
		;;
		;; Restore Registers
		;;
		popad
		mov 	esp,ebp
		pop 	ebp
		ret
save_seg_selectors	endp

_TEXT 	ENDS

END
