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

kcallg()
{
/*	int gvn, gvnt;  */
	int user,usertype;
	int testgv,testgvtype;

/*
    PI_getan(&gvn,&gvnt,1);
    if (gvnt != PI_INT) {
	printf("improper arg supplied to kcallg\n");
	PI_FAIL
    }
*/


	PI_makesym(&user,&usertype,"user");
	PI_makesym(&testgv,&testgvtype,"testgv");

	if (PI_rungoal(user,testgv,testgvtype))
		PI_printf("Run goal(testgv) succeeded.\n");
	else
		PI_printf("Run goal(testgv) failed.\n");

	PI_SUCCEED
}


PI_BEGIN
    PI_DEFINE("kcallg",0,kcallg)
    PI_DEFINE("kcall",2,kcall)
PI_END

PI_main()
{
    PI_toplevel();
}

PI_main_init()
{
/*  #include "fi_inits.h"  */
    PI_INIT
}

PI_App_init()
{
    return 1;
}
