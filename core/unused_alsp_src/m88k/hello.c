#include "alspi.h"

static int i;
int hello();
int dummy();

PI_BEGIN
    PI_DEFINE("hello",0,hello)
    PI_DEFINE("dummy",0,dummy)
PI_END

dummy()
{
    printf("This is the dummy function!\n");
}

hello()
{
    printf("Hello world!\n");
    PI_SUCCEED;
}

helloinit()
{
    PI_INIT;
    printf("In helloinit\n");
}
