/*
 * icmath.h	-- definitions for math instruction generation
 *	Copyright (c) 1992 Motorola Inc.
 *
 * Author: Scott Medeiros
 * Creation Date: 2/19/92
 *
 * Revision History:
 *   Revised:	mm/dd/yy	Who		What and Why
 *   Revised:	mm/dd/yy	Who		What and Why
 *
 *
 *
 * This file is included by icmath.c.  It gives definitions used in the 68k implementation
 * of NewMath.  The definitions here define the indexes into the 3 jump tables maintained
 * in domath.68k.  The definitions in this file obviously depend on the format of the 
 * jump tables (int_table, dbl_table, dcmp_table) in domath.68k.
 */

#define EQ_INDEX		0
#define LT_INDEX		1
#define GT_INDEX		2
#define LE_INDEX		3
#define GE_INDEX		4
#define NE_INDEX		5
#define GETNUM_INDEX		6
#define PUTNUM_INDEX		7
#define PUSHINT_INDEX		8
#define PUSHDBL_INDEX		9
#define ADD_INDEX		10
#define SUB_INDEX		11
#define MUL_INDEX		12
#define DIV_INDEX		13
#define FDIV_INDEX		14
#define NEG_INDEX		15
#define MOD_INDEX		16
#define BAND_INDEX		17
#define BOR_INDEX		18
#define BXOR_INDEX		19
#define NOT_INDEX		20
#define LSHFT_INDEX		21
#define RSHFT_INDEX		22
#define POWER_INDEX		23
#define ABS_INDEX		24
#define SIN_INDEX		25
#define COS_INDEX		26
#define TAN_INDEX		27
#define ASIN_INDEX		28
#define ACOS_INDEX		29
#define ATAN_INDEX		30
#define SQRT_INDEX		31
#define EXP_INDEX		32
#define EXP10_INDEX		33
#define LOG_INDEX		34
#define LOG10_INDEX		35
#define FLOOR_INDEX		36
#define ROUND_INDEX		37
#define TRUNC_INDEX		38
#define HEAPUSED_INDEX		39
#define CPUTIME_INDEX		40
#define REALTIME_INDEX		41
#define RANDOM_INDEX		42
#define ADDI_INDEX		43
#define SUBI_INDEX		44
#define CALLOUT_INIT_INDEX	45
#define CALLOUT_INDEX		46
#define SINH_INDEX		47
#define COSH_INDEX		48
#define TANH_INDEX		49
#define CEIL_INDEX		50
#define ERF_INDEX		51
#define ERFC_INDEX		52
#define GAMMA_INDEX		53
#define J0_INDEX		54
#define J1_INDEX		55
#define Y0_INDEX		56
#define Y1_INDEX		57
#define ATAN2_INDEX		58
#define FMOD_INDEX		59
#define HYPOT_INDEX		60
#define JN_INDEX		61
#define YN_INDEX		62
