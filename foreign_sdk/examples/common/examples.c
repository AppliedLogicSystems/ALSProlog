#include "alspi.h"

static int speeding(void) 
{
	PWord val;
	int type;
 
	PI_getan(&val,&type,1);   /* argument retrieval */

	if (type != PI_INT)       /* type checking */ 
		PI_FAIL;
 
	if (val <= 55)            /* computation */ 
		PI_FAIL;

	PI_SUCCEED; 
}

#define BUFSIZE     256 

static int printsym(void) 
{ 
	PWord val;
	int type; 
	char buf[BUFSIZE]; 

	PI_getan(&val,&type,1);     /* retrieve argument */ 

	switch(type) { 
		case PI_SYM: 
			PI_getsymname(buf,val,BUFSIZE); 
			PI_printf("Symbol in Buffer: %s\n",buf); 
			break; 
		case PI_UIA: 
			PI_getuianame(buf,val,BUFSIZE); 
			PI_printf(
			  "Uninterned Atom in Buffer: %s\n",buf); 
			break; 
		default: 
			PI_FAIL;      /* fail if not symbol */ 
			break; 
	} 
	PI_SUCCEED; 
} 

#define TABLESIZE   7 

struct table { 
	int type; 
	const char *buf; 
}; 

struct table typetable[] = { 
	{PI_VAR, "unbound variable"}, 
	{PI_LIST, "list"}, 
	{PI_STRUCT, "structure"}, 
	{PI_SYM, "regular symbol"}, 
	{PI_INT, "integer"}, 
	{PI_UIA, "uninterned atom"}, 
	{PI_DOUBLE, "double precision real"} 
}; 

static const char *typename(int type) 
{ 
	int i; 
	for (i=0; i < TABLESIZE; i++) 
		if (type == typetable[i].type) 
			return(typetable[i].buf); 
        
	return(0);              /* shouldnÕt get here */ 
} 

static int printstruct(void) 
{ 
	PWord val, functor, arg;
	int type, arity, argtype, i; 
	char buf[BUFSIZE]; 

	PI_getan(&val,&type,1);    /* retrieve argument */ 
	if (type != PI_STRUCT) 
		PI_FAIL; 
            
	PI_getstruct(&functor,&arity,val); 
	PI_getsymname(buf,functor,BUFSIZE); 
	PI_printf("Structure: %s/%d:\n",buf,arity); 
	for (i=1; i <= arity; i++) { 
		PI_getargn(&arg,&argtype,val,i); 
		PI_printf(
	      "Argument %d is a %s\n",i,typename(argtype)); 
	} 
	PI_SUCCEED; 
} 

static int printlist(void) 
{ 
	PWord val, head, tail;
	int type, headtype, tailtype; 
	int i; 

	PI_getan(&val,&type,1);    /* retrieve argument */ 
	if (type != PI_LIST) 
		PI_FAIL; 
	PI_printf("List:\n"); 
            
	for (i=1; type == PI_LIST; i++) { 
		PI_gethead(&head,&headtype,val); 
		PI_gettail(&tail,&tailtype,val); 
		PI_printf("List element %d is a %s\n",
				i,typename(headtype)); 
		val = tail;         /* move down the list */ 
		type = tailtype; 
	}; 
	PI_SUCCEED; 
} 

static int unifytest(void) 
{ 
	PWord val;
	int type;

	PI_getan(&val,&type,1); 
	if (!PI_unify(val,type,42,PI_INT)) 
		PI_FAIL; 
	PI_printf("integer value: %d, type: %s\n", val, 
			type == PI_INT ? "integer" : "other"); 
	PI_SUCCEED; 
}

static PWord nil_sym;

static int checknil(void)
{
	PWord val;
	int type;

	PI_getan(&val,&type,1);
	if (type == PI_SYM && val == nil_sym)
		PI_printf("The object was the nil symbol.\n");
	else
		PI_FAIL;
	PI_SUCCEED;
} 

#define STRLEN		20
#define MAX			50

static int current = 2; 
static char table[MAX][STRLEN] = {"molsons","coors"}; 

static int enter(void) 
{ 
	PWord val;
	int type;
 
	if (current >= MAX) { 
		PI_printf("Error: table full\n"); 
		PI_FAIL; 
	} 
	PI_getan(&val,&type,1); 
	switch(type) { 
		case PI_SYM: 
			if (!PI_getsymname(table[current],val,STRLEN)) 
   				PI_FAIL; 
			current++; 
			break; 
		case PI_UIA: 
			if (!PI_getuianame(table[current],val,STRLEN)) 
				PI_FAIL; 
			current++; 
			break; 
		default: 
			PI_FAIL; 
			break; 
	} 
	PI_SUCCEED; 
} 

static int collect(void) 
{
	PWord val,			/* original variable */
		list,		/* temporary list */
		head,		/* head of list */
		sym;		/* uia matched with head */
	int index;			/* index into table */
	int type, listtype, headtype, symtype;

	PI_getan(&val,&type,1); 
		/* 
		 * Loop through all the strings in the table, 
		 * creating a Prolog list with elements unified 
		 * with the strings in the table. 
		 */ 
	for (index=0; index < current; index++) { 
		PI_makelist(&list,&listtype); 
		if (!PI_unify(val,type,list,listtype)) 
			PI_FAIL; 
		PI_gethead(&head,&headtype,list); 
		PI_makeuia(&sym,&symtype,table[index]); 
		if (!PI_unify(head,headtype,sym,symtype)) 
			PI_FAIL; 
		PI_gettail(&val,&type,list); 
	} 
	if (!PI_unify(val,type,nil_sym,PI_SYM)) 
		PI_FAIL; 
	PI_SUCCEED; 
} 

static int getinfo(void) 
{ 
	PWord val, info, struc, arg;
	int type, infotype, structype, argtype, i; 

	PI_getan(&val,&type,1); 
	PI_makesym(&info,&infotype,"info"); 
	PI_makestruct(&struc,&structype,info,3); 
	if(!PI_unify(val,type,struc,structype))
		PI_FAIL; 

	for (i=1; i <= 3; i++) { 
		PI_getargn(&arg,&argtype,struc,i); 
		if (!PI_unify(arg,argtype,i,PI_INT)) 
			PI_FAIL; 
		} 
	PI_SUCCEED; 
}

static int counter;

static int init(void) 
{ 
	PWord val;
	int type;
 
	PI_getan(&val,&type,1); 
	if (type != PI_INT) 
		PI_FAIL; 
	counter = val; 
	PI_SUCCEED; 
} 

static int inc(void) 
{ 
	counter++; 
	PI_SUCCEED; 
} 

static int decr(void) 
{ 
	counter--; 
	PI_SUCCEED; 
}

static int value(void) 
{ 
	PWord val;
	int type;
 
	PI_getan(&val,&type,1); 
	if (!PI_unify(val,type,counter,PI_INT)) 
		PI_FAIL; 
	PI_SUCCEED; 
} 


PI_BEGIN
    PI_DEFINE("speeding",1,speeding)

    PI_DEFINE("printsym",1,printsym)

    PI_DEFINE("printstruct",1,printstruct)

    PI_DEFINE("printlist",1,printlist)

    PI_DEFINE("unifytest",1,unifytest)

    PI_DEFINE("checknil",1,checknil)

    PI_DEFINE("enter",1,enter)
    PI_DEFINE("collect",1,collect)

    PI_DEFINE("getinfo",1,getinfo)

	PI_MODULE("pizza")
	PI_DEFINE("init",1,init) 
	PI_DEFINE("inc",0,inc) 
	PI_DEFINE("decr",0,decr) 
	PI_DEFINE("value",1,value) 
PI_END

/* 
 * This is more efficient than calling PI_makesym() 
 * everytime you want to check for the nil symbol. 
 */ 

void pi_init(void) 
{           
	int niltype; /* we donÕt use this anywhere else */ 

	PI_makesym(&nil_sym,&niltype,"[]");

	PI_INIT;
} 
