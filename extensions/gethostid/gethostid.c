#if defined(SUNOS)
#include <sys/systeminfo.h>
#elif defined(HPUX)
#include <sys/utsname.h>
#else
#include <unistd.h>
#endif
#include <stdio.h>
#include "alspi.h"

static int prolog_gethostid(void)
{
    PWord arg, id;
    int arg_type, id_type;
    char s[256];
#if defined(HPUX)
    struct utsname name;
#endif

    PI_getan(&arg, &arg_type, 1);

#if defined(SUNOS)
    sysinfo(SI_HW_SERIAL, s, 256);
    PI_makeuia(&id, &id_type, s);
#elif defined(HPUX)
    uname(&name);
    PI_makeuia(&id, &id_type, name.idnumber);
#else
    sprintf(s, "%ld", gethostid());
    PI_makeuia(&id, &id_type, s);
#endif

    return PI_unify(arg, arg_type, id, id_type);
}

PI_BEGIN
    PI_DEFINE("gethostid", 1, prolog_gethostid)
PI_END

void pi_init(void)
{
    PI_INIT;
}
