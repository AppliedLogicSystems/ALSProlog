/*
 *	label.h
 *
 *	Written by Keith Hughes
 *
 * The following are label manipulation macros good for short and long
 * displacements
 */

/* Remember a point in the code stream by putting ic_ptr into a variable. */
#define LABEL(v) v = ic_ptr;

/* Calculate the offset from the current code pointer to the label v */
#define OFFSET(v) ((ic_ptr-(CodePtr)(v))*CodeSize)

/* These two macros calculate the distance from the code pointer back to a
   label in both SmallOffset size and BigOffset sizes */

/* used for backward short refs */
#define BDISP(v) ((SmallOffset)(-OFFSET(v)-sizeof(SmallOffset)))
/* used for backward long refs */
#define BLDISP(v) ((BigOffset)(-OFFSET(v)-sizeof(BigOffset)))

/* Patch an instruction already layed down */
#define PATCHDISP(v) *(((SmallOffset *)(v))-1) |= (SmallOffset)OFFSET(v);
#define PATCHLDISP(v) *(((BigOffset *)(v))-1) |=  (BigOffset)OFFSET(v);

/* Code has been shifted by offset code words. ANy labels found in the code
   must be shifted also. Do so. */
#define SHIFTLABEL(v,offset) v += (offset);

/* Move the code pointer back to some label. */
#define MOVETOLABEL(position) ic_ptr = (CodePtr)(position);

/* The equivalent of . in an assembler. Give the current code position. */
#define CURRENTPOSITION	ic_ptr


