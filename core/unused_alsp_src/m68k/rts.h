
|
| rts.h		-- include file for various of the assembler files
|	Copyright (c) 1987-1993 by Applied Logic Systems, Inc.
|

chpt_HB = 0
chpt_SPB = 4
chpt_NextClause = 8
chpt_B	= 12
chpt_size = 16

#define DELAY_4 (TK_DELAY*16+0x4000007)
#define NIL     (TK_NIL*16+7)

/*
 * Symbolic Register Definitions
 */

#define SAF d7
#define SPB d6
#define HB d5
#define SP a7
#define E a6
#define H a5
#define TR a4
#define B a3
#define Fail a2
