	TITLE	AICRSEG
	.386p 
;;;
;;; aicrseg.s -- Create code segments 
;;;			     (In Ergo Environment)
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
_TEXT	SEGMENT	DWORD PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT	DWORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT	DWORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT DWORD PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP	_TEXT, _DATA, CONST, _BSS


;;
;; Get the configuration information
;;
INCLUDE sconfig.h

;ASSUME	CS:_TEXT, DS:DGROUP, SS:DGROUP, ES:DGROUP 
ASSUME	CS:DGROUP, DS:DGROUP, SS:DGROUP, ES:DGROUP 

_TEXT	SEGMENT 

IFDEF Ergo

;************************************************************************
;						For Ergo Environment							*
;************************************************************************

;;
;; Protected mode data segment
;;
PROT_DATASEG	EQU 	017h


;;
;; cr_codeseg
;;
;;	Create a new code window in the data segment (segment 17).
;;

extrn	xseg_call_selector:word

		align 4
		public cr_codeseg
cr_codeseg proc near
		;;
		;; Save registers
		;;
		pushad
		;;
		;; Increment size of the data segment
		;; (16MB or maximum available)
		;;
		mov 	ah,0edh
		mov 	bx,ds
		int 	021h
		add 	cx,0100h
l1:
		mov 	bx,ds
		mov 	ax,0E905h
		int 	021h
		jc  	l1
		;;
		;; Get the size of the data segment
		;;
		mov 	ah,0edh
		mov 	bx,ds
		int 	021h
		;;
		;; Create a new data window in the data segment,
		;; Size of this new data window is same is it parent.
		;;
		mov 	ax,0e801h
		mov 	si,0
		mov 	bx,0
		int 	021h
		mov 	xseg_call_selector,ax
		;;
		;; Convert it into a code window
		;;
		mov 	bx,xseg_call_selector
		mov 	ax,0e901h
		int 	021h
		;;
		;; Restore registers and return
		;;
		popad
		ret
cr_codeseg endp


ENDIF

;*************  End of Ergo Environment  ****************


_TEXT 	ENDS

END
