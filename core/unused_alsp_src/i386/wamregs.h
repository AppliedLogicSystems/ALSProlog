/*
	wamregs.h	--	Provide mapping between processor registers
				and Prolog registers.

	Written by Keith Hughes

	This file is specific for a given processor. This file must match
	wamregs.m4

	Processor: 386
*/

#include "machreg.h"

#define SP_REG	ESP
#define H_REG	EDX
#define TR_REG	ECX
#define E_REG	EBP
#define S_REG	EBX
#define SPB_REG	EDI
#define HB_REG	ESI

