#include "alspi.h"

hello()
{
    PI_printf("Hello World!\n");
    PI_SUCCEED
}

kcall()
{
    PWord mv,gv;
    int mt,gt;
    int status;

    PI_getan(&mv,&mt,1);
    PI_getan(&gv,&gt,2);

    if (mt != PI_SYM) {
	printf("improper module supplied to kcall\n");
	PI_FAIL
    }

#ifdef notdef
    printf("In kcall\n");
#endif
    status = PI_rungoal(mv,gv,gt);
#ifdef notdef
    printf("After call to PI_rungoal\n");
#endif
    if (status)
	PI_SUCCEED
    else
	PI_FAIL
}

PI_BEGIN
    PI_DEFINE("hello",0,hello)
    PI_DEFINE("kcall",2,kcall)
PI_END

PI_main()
{
    PI_toplevel();
}

PI_main_init()
{
    PI_INIT
    return 1;
}

PI_App_init()
{
    return 1;
}
