#include "x87_fpsetround.h"

typedef struct {
   unsigned exception_masks:6;
   unsigned reserved2:2;
   unsigned percision:2;
   unsigned rounding:2;
   unsigned reserved1:4;
} x87_control_word;

fp_rnd fpsetround(fp_rnd mode)
{
    x87_control_word control;
    fp_rnd old_rounding;

    asm (fstcw control);

    old_rounding = (fp_rnd)control.rounding;

    control.rounding = mode;

    asm (fldcw control);

    return old_rounding;
}
