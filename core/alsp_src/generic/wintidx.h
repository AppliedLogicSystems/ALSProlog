/*
 * wintidx.h		-- indices of the Prolog registers in the wm_regs 
 *			   structure.
 *	Copyright (c) 1988-1993 by Applied Logic Systems, Inc.
 * Author:  Kevin A. Buettner
 * Creation: 7/1/88
 * Revision History:
 *
 */

#ifdef arch_m88k
#define wm_CP_idx 0
#define wm_TR_idx 1
#define wm_H_idx 2
#define wm_FAIL_idx 3
#define wm_SPB_idx 4
#define wm_HB_idx 5
#define wm_B_idx 6
#define wm_E_idx 8
#define wm_SP_idx 8
/* Note that E_idx and SP_idx are the same. */
 
/* wm_CSP is the place to put the C stack pointer */
#define wm_CSP_idx 10

#define wm_CP	(wm_regs[wm_regidx][wm_CP_idx])

#else /* arch_m88k */

#define wm_HB_idx	0
#define wm_SPB_idx	1
#define wm_FAIL_idx	2
#define wm_B_idx	3
#define wm_TR_idx	4
#define wm_H_idx	5
#define wm_E_idx	6
#define wm_SP_idx	7

#endif /* arch_m88k */

