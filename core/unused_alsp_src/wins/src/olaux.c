/*---------------------------------------------------------*
 |			olaux.c
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

#include "alspi.h"
#include "cinterf.h"
#include "ol.h"

/*
 * slistadditem(FuncPtr,Widget,Parent,Ref,Item,RetVal)
 *      use to add an item to a scrolling list
 *      Note : we could not use c_call because Item is a struct
 */

slistadditem()
{
  OlListToken (*fptr)(), retval;
  PWord v1; int t1;
  PWord v2; int t2;
  PWord v3; int t3;
  PWord v4; int t4;
  PWord v5; int t5;
  PWord v6; int t6;

  PI_getan(&v1,&t1,1);
  PI_getan(&v2,&t2,2);
  PI_getan(&v3,&t3,3);
  PI_getan(&v4,&t4,4);
  PI_getan(&v5,&t5,5);
  PI_getan(&v6,&t6,6);

  if( !CI_get_integer(&v1,t1) ||
	  !CI_get_integer(&v2,t2) ||
	  !CI_get_integer(&v3,t3) ||
	  !CI_get_integer(&v4,t4) ||
	  !CI_get_integer(&v5,t5) ) PI_FAIL;

  fptr = (OlListToken (*)()) v1;

  retval = (*fptr)((char *)v2,(char *)v3,
				   (char *)v4,*(OlListItem *)v5);

  if( !PI_unify(v6,t6,retval,PI_INT) ) PI_FAIL;
  PI_SUCCEED;
}


PI_BEGIN
  PI_PDEFINE("slistadditem",6,slistadditem,"_slistadditem")
PI_END


olaux_init()
{
  PI_INIT;
}

