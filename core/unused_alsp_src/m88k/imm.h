/*
 * Immediates:
 *
 * The 88k indicates immediate values by 16-bit unsigned integers.
 * Need hi16 and lo16 for or.u instructions and the like on the 88k.
 *
 */

#define hi16(val) ((((int) (val)) >> 16) & 0xffff )
#define lo16(val) (((int) (val)) & 0xffff)
