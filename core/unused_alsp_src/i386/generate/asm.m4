divert(1)

define(_,	`
dnl')

define(__,	`dnl')

__;
__; Now that things are set up, we can say who we are.
__;
__; asm.m4
__;
__; Provide access to the proper assembler generation macros for the
__; ALS Prolog 386 system.
__;


ifdef(`DOS',	`define(`MASM',		1)')

ifdef(`SUNOS',	`define(`SUNOS',	1)')

ifdef(`SYSV',	`define(`SYSV',		1)')

ifdef(`XENIX',	`define(`MASM',		1)')

ifdef(`XENIX',	`define(`FLEXNAMES',	1)')


__;
__; Get the proper specific assembler macro file.
__;

ifdef(`MASM',	`include(`Generate/masm.m4')')

ifdef(`SYSV',	`include(`Generate/sysv.m4')')

ifdef(`SUNOS',	`include(`Generate/sunos.m4')')

__;
__; Say whether or not the C compiler insists on _s in assembler code.
__;

ifdef(`FLEXNAMES',
	`define(`FNAMES',	`_$1')',
	`define(`FNAMES',	`$1')' )

divert(0) dnl
