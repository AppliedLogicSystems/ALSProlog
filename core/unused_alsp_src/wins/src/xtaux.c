/*
 * xtaux.c   --  collection xt auxilliary foreign predicate defs
 *
 * Author : P.raman
 * Date   : 4/20/92
 *
 * callbackfunc()   -- invokes prolog goal 
 *                     user:$callback(w,client_data,call_data).
 * actionfunc()     -- invokes prolog goal
 *                     user:$action(w,event,params,num_params).
 * workproc()       -- invokes prolog goal
 *                     user:$workproc(client_data)
 * timercallbackproc() -- invokes prolog goal
 *                     user:$timercallbackproc(client_data,interval_id)
 */

#include "alspi.h"
#include "cinterf.h"
#include <X11/Intrinsic.h>

callbackfunc(w, client_data, call_data)
     Widget w;
     caddr_t client_data, call_data;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

/* printf("Enter callbackfunc\n"); */

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$callback");
  PI_makestruct(&goal,&goaltype,func,3);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)w);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,2);
  PI_makedouble(&v,&t,(double)(long)client_data);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,3);
  PI_makedouble(&v,&t,(double)(long)call_data);
  PI_unify(arg,argtype,v,t);

/* printf("callbackfunc before PI_rungoal\n"); */

/*  PI_rungoal(mod,&goal,&goaltype);  */
  PI_rungoal_with_update(mod,&goal,&goaltype);
}


actionfunc(w,event,params,num_params)
     Widget w;
     XButtonEvent *event;
     String *params;
     Cardinal num_params;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$action");
  PI_makestruct(&goal,&goaltype,func,4);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)w);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,2);
  PI_makedouble(&v,&t,(double)(long)event);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,3);
  PI_makedouble(&v,&t,(double)(long)params);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,4);
  PI_unify(arg,argtype,num_params,PI_INT);

/*  PI_rungoal(mod,&goal,&goaltype);  */
  PI_rungoal_with_update(mod,&goal,&goaltype);
}


Boolean workproc(client_data)
     caddr_t client_data;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$workproc");
  PI_makestruct(&goal,&goaltype,func,1);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)client_data);
  PI_unify(arg,argtype,v,t);

/*  if( PI_rungoal(mod,&goal,&goaltype) ) */
  if( PI_rungoal_with_update(mod,&goal,&goaltype) )
    return( TRUE );
  else
    return( FALSE );
}


timercallbackproc(client_data, id)
     caddr_t client_data;
     XtIntervalId *id;
{
  PWord mod; int modtype;
  PWord goal; int goaltype;
  PWord func; int functype;
  PWord arg; int argtype;
  PWord v; int t;

  PI_makesym(&mod,&modtype,"user");

  PI_makesym(&func,&functype,"$timercallbackproc");
  PI_makestruct(&goal,&goaltype,func,2);

  PI_getargn(&arg,&argtype,goal,1);
  PI_makedouble(&v,&t,(double)(long)client_data);
  PI_unify(arg,argtype,v,t);

  PI_getargn(&arg,&argtype,goal,2);
  PI_makedouble(&v,&t,(double)(long)id);
  PI_unify(arg,argtype,v,t);

/*   PI_rungoal(mod,&goal,&goaltype); */
  PI_rungoal_with_update(mod,&goal,&goaltype);
}



xtaux_init()
{
  CI_RCONST("callbackfunc",(long)callbackfunc);
  CI_RCONST("actionfunc",(long)actionfunc);
  CI_RCONST("workproc",(long)workproc);
  CI_RCONST("timercallbackproc",(long)timercallbackproc);
}
