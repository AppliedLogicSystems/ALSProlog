/*
 * domath_tables.c		-- C allocation of math jump tables.
 *						-- Table switching for SANE vs 68881
 *	Copyright (c) 1994 Applied Logic Systems, Inc.
 *
 * Author:	Chuck Houpt
 * Creation:	11/30/94
 * Revision History:
 *	Revised: mm/dd/yy	Who		-- Why and What
 */

#include "defs.h"

#ifdef MacOS
#include "icmath.h"
#include <GestaltEqu.h>

long has_fpu = 0;

    /* Do the actual allocation of int_table and dbl_table in C so that the tables
       contain pointers to the jump table.  The assembler won't do this.
    */
    
	void _mth_eq(void);	
	void _mth_lt(void);	
	void _mth_gt(void);
	void _mth_le(void);
	void _mth_ge(void);
	void _mth_ne(void);
	void _mth_getnum(void);
	void _mth_putnum(void);
	void _mth_pushint(void);
	void _mth_pushdbl(void);
	void _mth_add(void);
	void _mth_sub(void);
	void _mth_mul(void);
	void _mth_div(void);
	void _mth_fdiv(void);
	void _mth_neg(void);
	void _mth_mod(void);
	void _mth_band(void);
	void _mth_bor(void);
	void _mth_bxor(void);
	void _mth_not(void);
	void _mth_lshft(void);
	void _mth_rshft(void);
	void _mth_power(void);
	void _mth_abs(void);
	void _mth_sin(void);
	void _mth_cos(void);
	void _mth_tan(void);
	void _mth_asin(void);
	void _mth_acos(void);
	void _mth_atan(void);
	void _mth_sqrt(void);
	void _mth_exp(void);
	void _mth_exp10(void);
	void _mth_log(void);
	void _mth_log10(void);
	void _mth_floor(void);
	void _mth_round(void);
	void _mth_trunc(void);
	void _mth_heapused(void);
	void _mth_cputime(void);
	void _mth_realtime(void);
	void _mth_random(void);
	void _mth_addi(void);
	void _mth_subi(void);
	void _mth_callout_init(void);
	void _mth_callout(void);
	void _mth_sinh(void);
	void _mth_cosh(void);
	void _mth_tanh(void);
	void _mth_ceil(void);
	void _mth_erf(void);
	void _mth_erfc(void);
	void _mth_gamma(void);
	void _mth_j0(void);
	void _mth_j1(void);
	void _mth_y0(void);
	void _mth_y1(void);
	void _mth_atan2(void);
	void _mth_fmod(void);
	void _mth_hypot(void);
	void _mth_jn(void);
	void _mth_yn(void);

	void dbl_eq(void);
	void dbl_lt(void);
	void dbl_gt(void);
	void dbl_le(void);
	void dbl_ge(void);
	void dbl_ne(void);
	void dbl_getnum(void);
	void dbl_putnum(void);
	void dbl_pushint(void);
	void dbl_pushdbl(void);
	void dbl_add(void);
	void dbl_sub(void);
	void dbl_mul(void);
	void dbl_div(void);
	void dbl_fdiv(void);
	void dbl_neg(void);
	void dbl_mod(void);
	void dbl_band(void);
	void dbl_bor(void);
	void dbl_bxor(void);
	void dbl_not(void);
	void dbl_lshft(void);
	void dbl_rshft(void);
	void dbl_power(void);
	void dbl_abs(void);
	void dbl_sin(void);
	void dbl_cos(void);
	void dbl_tan(void);
	void dbl_asin(void);
	void dbl_acos(void);
	void dbl_atan(void);
	void dbl_sqrt(void);
	void dbl_exp(void);
	void dbl_exp10(void);
	void dbl_log(void);
	void dbl_log10(void);
	void dbl_floor(void);
	void dbl_round(void);
	void dbl_trunc(void);
	void dbl_heapused(void);
	void dbl_cputime(void);
	void dbl_realtime(void);
	void dbl_random(void);
	void dbl_addi(void);
	void dbl_subi(void);
	void dbl_callout_init(void);
	void dbl_callout(void);
	void dbl_sinh(void);
	void dbl_cosh(void);
	void dbl_tanh(void);
	void dbl_ceil(void);
	void dbl_erf(void);
	void dbl_erfc(void);
	void dbl_gamma(void);
	void dbl_j0(void);
	void dbl_j1(void);
	void dbl_y0(void);
	void dbl_y1(void);
	void dbl_atan2(void);
	void dbl_fmod(void);
	void dbl_hypot(void);
	void dbl_jn(void);
	void dbl_yn(void);

	void sane_dbl_eq(void);
	void sane_dbl_lt(void);
	void sane_dbl_gt(void);
	void sane_dbl_le(void);
	void sane_dbl_ge(void);
	void sane_dbl_ne(void);
	void sane_dbl_putnum(void);
	void sane_dbl_pushint(void);
	void sane_dbl_add(void);
	void sane_dbl_sub(void);
	void sane_dbl_mul(void);
	void sane_dbl_div(void);
	void sane_dbl_fdiv(void);
	void sane_dbl_neg(void);
	void sane_dbl_mod(void);
	void sane_dbl_band(void);
	void sane_dbl_bor(void);
	void sane_dbl_bxor(void);
	void sane_dbl_not(void);
	void sane_dbl_lshft(void);
	void sane_dbl_rshft(void);
	void sane_dbl_abs(void);
	void sane_dbl_sin(void);
	void sane_dbl_cos(void);
	void sane_dbl_tan(void);
	void sane_dbl_asin(void);
	void sane_dbl_acos(void);
	void sane_dbl_atan(void);
	void sane_dbl_sqrt(void);
	void sane_dbl_exp(void);
	void sane_dbl_exp10(void);
	void sane_dbl_log(void);
	void sane_dbl_log10(void);
	void sane_dbl_floor(void);
	void sane_dbl_round(void);
	void sane_dbl_trunc(void);
	void sane_dbl_heapused(void);
	void sane_dbl_addi(void);
	void sane_dbl_subi(void);
	void sane_dbl_sinh(void);
	void sane_dbl_cosh(void);
	void sane_dbl_tanh(void);

	void dcmp_eq(void);
	void dcmp_lt(void);
	void dcmp_gt(void);
	void dcmp_le(void);
	void dcmp_ge(void);
	void dcmp_ne(void);
	void dcmp_getnum(void);
	void dcmp_putnum(void);
	void dcmp_pushint(void);
	void dcmp_pushdbl(void);
	void dcmp_add(void);
	void dcmp_sub(void);
	void dcmp_mul(void);
	void dcmp_div(void);
	void dcmp_fdiv(void);
	void dcmp_neg(void);
	void dcmp_mod(void);
	void dcmp_band(void);
	void dcmp_bor(void);
	void dcmp_bxor(void);
	void dcmp_not(void);
	void dcmp_lshft(void);
	void dcmp_rshft(void);
	void dcmp_power(void);
	void dcmp_abs(void);
	void dcmp_sin(void);
	void dcmp_cos(void);
	void dcmp_tan(void);
	void dcmp_asin(void);
	void dcmp_acos(void);
	void dcmp_atan(void);
	void dcmp_sqrt(void);
	void dcmp_exp(void);
	void dcmp_exp10(void);
	void dcmp_log(void);
	void dcmp_log10(void);
	void dcmp_floor(void);
	void dcmp_round(void);
	void dcmp_trunc(void);
	void dcmp_heapused(void);
	void dcmp_cputime(void);
	void dcmp_realtime(void);
	void dcmp_random(void);
	void dcmp_addi(void);
	void dcmp_subi(void);
	void dcmp_callout_init(void);
	void dcmp_callout(void);
	void dcmp_sinh(void);
	void dcmp_cosh(void);
	void dcmp_tanh(void);
	void dcmp_ceil(void);
	void dcmp_erf(void);
	void dcmp_erfc(void);
	void dcmp_gamma(void);
	void dcmp_j0(void);
	void dcmp_j1(void);
	void dcmp_y0(void);
	void dcmp_y1(void);
	void dcmp_atan2(void);
	void dcmp_fmod(void);
	void dcmp_hypot(void);
	void dcmp_jn(void);
	void dcmp_yn(void);

void (*int_table[])(void) = {
	_mth_eq,
	_mth_lt,
	_mth_gt,
	_mth_le,
	_mth_ge,
	_mth_ne,
	_mth_getnum,
	_mth_putnum,
	_mth_pushint,
	_mth_pushdbl,
	_mth_add,
	_mth_sub,
	_mth_mul,
	_mth_div,
	_mth_fdiv,
	_mth_neg,
	_mth_mod,
	_mth_band,
	_mth_bor,
	_mth_bxor,
	_mth_not,
	_mth_lshft,
	_mth_rshft,
	_mth_power,
	_mth_abs,
	_mth_sin,
	_mth_cos,
	_mth_tan,
	_mth_asin,
	_mth_acos,
	_mth_atan,
	_mth_sqrt,
	_mth_exp,
	_mth_exp10,
	_mth_log,
	_mth_log10,
	_mth_floor,
	_mth_round,
	_mth_trunc,
	_mth_heapused,
	_mth_cputime,
	_mth_realtime,
	_mth_random,
	_mth_addi,
	_mth_subi,
	_mth_callout_init,
	_mth_callout,
	_mth_sinh,
	_mth_cosh,
	_mth_tanh,
	_mth_ceil,
	_mth_erf,
	_mth_erfc,
	_mth_gamma,
	_mth_j0,
	_mth_j1,
	_mth_y0,
	_mth_y1,
	_mth_atan2,
	_mth_fmod,
	_mth_hypot,
	_mth_jn,
	_mth_yn,
};

void (*dbl_table[])(void) = {
	dbl_eq,
	dbl_lt,
	dbl_gt,
	dbl_le,
	dbl_ge,
	dbl_ne,
	dbl_getnum,
	dbl_putnum,
	dbl_pushint,
	dbl_pushdbl,
	dbl_add,
	dbl_sub,
	dbl_mul,
	dbl_div,
	dbl_fdiv,
	dbl_neg,
	dbl_mod,
	dbl_band,
	dbl_bor,
	dbl_bxor,
	dbl_not,
	dbl_lshft,
	dbl_rshft,
	dbl_power,
	dbl_abs,
	dbl_sin,
	dbl_cos,
	dbl_tan,
	dbl_asin,
	dbl_acos,
	dbl_atan,
	dbl_sqrt,
	dbl_exp,
	dbl_exp10,
	dbl_log,
	dbl_log10,
	dbl_floor,
	dbl_round,
	dbl_trunc,
	dbl_heapused,
	dbl_cputime,
	dbl_realtime,
	dbl_random,
	dbl_addi,
	dbl_subi,
	dbl_callout_init,
	dbl_callout,
	dbl_sinh,
	dbl_cosh,
	dbl_tanh,
	dbl_ceil,
	dbl_erf,
	dbl_erfc,
	dbl_gamma,
	dbl_j0,
	dbl_j1,
	dbl_y0,
	dbl_y1,
	dbl_atan2,
	dbl_fmod,
	dbl_hypot,
	dbl_jn,
	dbl_yn,
};


void (*sane_dbl_table[])(void) = {
	sane_dbl_eq,
	sane_dbl_lt,
	sane_dbl_gt,
	sane_dbl_le,
	sane_dbl_ge,
	sane_dbl_ne,
	dbl_getnum,
	sane_dbl_putnum,
	sane_dbl_pushint,
	dbl_pushdbl,
	sane_dbl_add,
	sane_dbl_sub,
	sane_dbl_mul,
	sane_dbl_div,
	sane_dbl_fdiv,
	sane_dbl_neg,
	sane_dbl_mod,
	sane_dbl_band,
	sane_dbl_bor,
	sane_dbl_bxor,
	sane_dbl_not,
	sane_dbl_lshft,
	sane_dbl_rshft,
	dbl_power,
	sane_dbl_abs,
	sane_dbl_sin,
	sane_dbl_cos,
	sane_dbl_tan,
	sane_dbl_asin,
	sane_dbl_acos,
	sane_dbl_atan,
	sane_dbl_sqrt,
	sane_dbl_exp,
	sane_dbl_exp10,
	sane_dbl_log,
	sane_dbl_log10,
	sane_dbl_floor,
	sane_dbl_round,
	sane_dbl_trunc,
	sane_dbl_heapused,
	dbl_cputime,
	dbl_realtime,
	dbl_random,
	sane_dbl_addi,
	sane_dbl_subi,
	dbl_callout_init,
	dbl_callout,
	sane_dbl_sinh,
	sane_dbl_cosh,
	sane_dbl_tanh,
	dbl_ceil,
	dbl_erf,
	dbl_erfc,
	dbl_gamma,
	dbl_j0,
	dbl_j1,
	dbl_y0,
	dbl_y1,
	dbl_atan2,
	dbl_fmod,
	dbl_hypot,
	dbl_jn,
	dbl_yn,
};

void (*dcmp_table[])(void) = {
	dcmp_eq,
	dcmp_lt,
	dcmp_gt,
	dcmp_le,
	dcmp_ge,
	dcmp_ne,
	dcmp_getnum,
	dcmp_putnum,
	dcmp_pushint,
	dcmp_pushdbl,
	dcmp_add,
	dcmp_sub,
	dcmp_mul,
	dcmp_div,
	dcmp_fdiv,
	dcmp_neg,
	dcmp_mod,
	dcmp_band,
	dcmp_bor,
	dcmp_bxor,
	dcmp_not,
	dcmp_lshft,
	dcmp_rshft,
	dcmp_power,
	dcmp_abs,
	dcmp_sin,
	dcmp_cos,
	dcmp_tan,
	dcmp_asin,
	dcmp_acos,
	dcmp_atan,
	dcmp_sqrt,
	dcmp_exp,
	dcmp_exp10,
	dcmp_log,
	dcmp_log10,
	dcmp_floor,
	dcmp_round,
	dcmp_trunc,
	dcmp_heapused,
	dcmp_cputime,
	dcmp_realtime,
	dcmp_random,
	dcmp_addi,
	dcmp_subi,
	dcmp_callout_init,
	dcmp_callout,
	dcmp_sinh,
	dcmp_cosh,
	dcmp_tanh,
	dcmp_ceil,
	dcmp_erf,
	dcmp_erfc,
	dcmp_gamma,
	dcmp_j0,
	dcmp_j1,
	dcmp_y0,
	dcmp_y1,
	dcmp_atan2,
	dcmp_fmod,
	dcmp_hypot,
	dcmp_jn,
	dcmp_yn,
};


void init_math(void)
{
	OSErr err;
	long feature;
	int i;
	
	err = Gestalt(gestaltFPUType, &feature);
	if (err != noErr) feature = gestaltNoFPU;
	
	has_fpu = (feature != gestaltNoFPU);
	
	if (!has_fpu) {
		for (i = 0; i <= YN_INDEX; i++) 
			dbl_table[i] = sane_dbl_table[i];
	}
}

#endif