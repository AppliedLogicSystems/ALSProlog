/*
 * nextaux.c -- collection of auxilliary next interface predicates
 *
 */

#include "alspi.h"
#include "cinterf.h"
#include "nextstep.h"

#define MAXNARGS 30

sendMessage()              /* usage : $sendMessage( id, SEL, arglist, retval ) */
{
  PWord v1; int t1;
  PWord v2; int t2;
  PWord v3; int t3;
  PWord v4; int t4;
  PWord head; int headtype;
  void *args[MAXNARGS+1];   /* array for saving type of arguments */
  int nargs = 0;            /* #input arguments */
  id retval;
  id target;
  char *method_name;
  SEL method;
  id (*method_addr)();
  register int i;

  PI_getan(&v1,&t1,1);         /* get the target */
  if( t1 != PI_INT )
	if( !CI_get_integer(&v1,t1) )
	  PI_FAIL;

  target = (id) v1;

  PI_getan(&v2,&t2,2);        /* get the method name */
  if( t2 == PI_SYM )
	method_name = PI_getsymname(0,v2,0);
  else
	if( CI_get_integer(&v2,t2) )
	  method_name = (char *)v2;
	else
	  PI_FAIL;

  method = sel_getUid(method_name);
  method_addr = [target methodFor:method];

  PI_getan(&v3,&t3,3);         /* get the arguments */
  PI_getan(&v4,&t4,4);         /* get the return argument */

  while( t3 == PI_LIST ){
    PI_gethead(&head,&headtype,v3);
	if( headtype == PI_SYM )
	  args[nargs++] = PI_getsymname(0,head,0);
	else
	  if( CI_get_integer(&head,headtype) )
		args[nargs++] = (void *)head;
	  else PI_FAIL;
    PI_gettail(&v3,&t3,v3);
  }

  switch( nargs ){
  case 0 : retval = (*method_addr)(target,method); break;
  case 1 : retval = (*method_addr)(target,method,args[0]); break;
  case 2 : retval = (*method_addr)(target,method,args[0],args[1]); break;
  case 3 : retval = (*method_addr)(target,method,args[0],args[1],args[2]); break;
  case 4 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3]); break;
  case 5 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4]); break;
  case 6 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4],args[5]); break;
  case 7 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4],args[5],args[6]); break;
  case 8 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4],args[5],
				   args[6],args[7]); break;
  case 9 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4],args[5],
				   args[6],args[7],args[8]); break;
  case 10 : retval = (*method_addr)(target,method,args[0],args[1],args[2],args[3],args[4],args[5],
				   args[6],args[7],args[8],args[9]); break;
  default : PI_FAIL;
  }

  if( !PI_unify(v4,t4,(long)retval,PI_INT) ) PI_FAIL;
  PI_SUCCEED;
}


PI_BEGIN
  PI_DEFINE("$sendMessage",4,sendMessage)
PI_END

nextaux_init()
{
  PI_INIT;
}
