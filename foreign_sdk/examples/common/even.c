#include "alspi.h"

static int even(void)
{
    PWord arg;
    int arg_type;

    PI_getan(&arg, &arg_type, 1);
  
    if (arg_type == PI_INT && arg % 2 == 0) PI_SUCCEED;
    else PI_FAIL;
}

PI_BEGIN
    PI_DEFINE("even",1,even)
PI_END

void pi_init(void)
{
    PI_INIT;
}

#if defined(STATIC) || defined(__MWERKS__)

int main(int argc, char *argv[])
{
    return PI_main(argc, argv, pi_init);
}

#endif
