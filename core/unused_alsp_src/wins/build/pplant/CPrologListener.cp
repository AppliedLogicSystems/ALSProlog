/*

	
	user:predicate(message, ioParam)

*/


#include "alspi.h"
#include "cinterf.h"

#include "CPrologListener.h"

CPrologListener::CPrologListener(const char *pred)
{
	predicate = pred;
}

void CPrologListener::ListenToMessage(MessageT inMessage, void *ioParam)
{
	PWord mod; int modtype;
	PWord goal; int goaltype;
	PWord func; int functype;
	PWord arg; int argtype;
	PWord v; int t;

	PI_makesym(&mod,&modtype,"user");

	PI_makesym(&func,&functype,predicate);
	PI_makestruct(&goal,&goaltype,func,2);

	PI_getargn(&arg,&argtype,goal,1);
	PI_makedouble(&v,&t,(double)(long)inMessage);
	PI_unify(arg,argtype,v,t);

	PI_getargn(&arg,&argtype,goal,2);
	PI_makedouble(&v,&t,(double)(long)ioParam);
	PI_unify(arg,argtype,v,t);

	PI_rungoal_with_update(mod,&goal,&goaltype);
}
