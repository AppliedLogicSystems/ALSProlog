/*---------------------------------------------------------*
 |			xaux.c
 |		Copyright (c) 1992-96 Applied Logic Systems, Inc.
 |
 |	 afterfunc() : invokes prolog goal user:$after(display)
 |	 errrorfunc(): invokes prolog goal user:$error(display,event)
 *---------------------------------------------------------*/

#include "alspi.h"
#include "cinterf.h"
#include "x.h"

afterfunc(display)
     Display *display;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$after");
  PI_makestruct(&goal,&goaltype,func,1);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)display);
  PI_unify(arg,argtype,v,t);

  PI_rungoal(mod,&goal,&goaltype);
}
  
errorfunc(display, event)
     Display *display;
     XErrorEvent *event;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$error");
  PI_makestruct(&goal,&goaltype,func,2);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)display);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,2);
  PI_makedouble(&v,&t,(double)(long)event);
  PI_unify(arg,argtype,v,t);

  PI_rungoal(mod,&goal,&goaltype);
}

xaux_init()
{
    CI_RCONST("afterfunc",(long)afterfunc)
    CI_RCONST("errorfunc",(long)errorfunc)
}

