/*
 * bint86.c 	-- $$int86/9 and $$int86/10 builtins 
 *
 * Copyright (c) Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 11/7/89
*/

#include "config.h"


/*
 * Include this file for only DOS version of the system
 */
#ifdef als_i386
#ifdef DOS

#include "types.h"
#include "mtypes.h"
#include "built.h"
#include "winter.h"
#include "tokens.h"
#include "parser.h"
#include "dos386.h"

/*
 * $$int86(IntNum,AX,BX,CX,DX,RetAX,RetBX,RetCX,RetDX)
 *
 * Gives Prolog access to the int86 call.
*/

pbi_int86_9arg()
{
	PWord v1, v2, v3, v4, v5, v6, v7, v8, v9;
	int t1, t2, t3, t4, t5, t6, t7, t8, t9;
	union REGS386 in;

	w_get_An(&v1,&t1,1);
	w_get_An(&v2,&t2,2);
	w_get_An(&v3,&t3,3);
	w_get_An(&v4,&t4,4);
	w_get_An(&v5,&t5,5);
	w_get_An(&v6,&t6,6);
	w_get_An(&v7,&t7,7);
	w_get_An(&v8,&t8,8);
	w_get_An(&v9,&t9,9);

	if (t1 == WTP_INTEGER && t2 == WTP_INTEGER && t3 == WTP_INTEGER && 
	    t4 == WTP_INTEGER && t5 == WTP_INTEGER) {
		in.x.ax=(short)v2;
		in.x.bx=(short)v3;
		in.x.cx=(short)v4;
		in.x.dx=(short)v5;

		int386((short)v1,&in,&in);

		if (w_unify(v6,t6,(PWord)in.x.ax,WTP_INTEGER) &&
		    w_unify(v7,t7,(PWord)in.x.bx,WTP_INTEGER) &&
		    w_unify(v8,t8,(PWord)in.x.cx,WTP_INTEGER) &&
		    w_unify(v9,t9,(PWord)in.x.dx,WTP_INTEGER))
			SUCCEED
	}

	FAIL
}


/*
 * $$int86(IntNum,AX,BX,CX,DX,RetAX,RetBX,RetCX,RetDX,RetEFlags)
 *
 * Gives Prolog access to the int86 call.
*/

pbi_int86_10arg()
{
	PWord v1, v2, v3, v4, v5, v6, v7, v8, v9, v10;
	int t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;
	union REGS386 in;

	w_get_An(&v1,&t1,1);
	w_get_An(&v2,&t2,2);
	w_get_An(&v3,&t3,3);
	w_get_An(&v4,&t4,4);
	w_get_An(&v5,&t5,5);
	w_get_An(&v6,&t6,6);
	w_get_An(&v7,&t7,7);
	w_get_An(&v8,&t8,8);
	w_get_An(&v9,&t9,9);
	w_get_An(&v10,&t10,10);

	if (t1 == WTP_INTEGER && t2 == WTP_INTEGER && t3 == WTP_INTEGER && 
	    t4 == WTP_INTEGER && t5 == WTP_INTEGER) {
		in.x.ax=(short)v2;
		in.x.bx=(short)v3;
		in.x.cx=(short)v4;
		in.x.dx=(short)v5;

		int386((short)v1,&in,&in);

		/*
		 * make sure that eflags can be a Prolog integer.
		 * (we only need low 18 bits eflags register)
		 */
		in.l.eflags &= 0x03ffff;

		if (w_unify(v6,t6,(PWord)in.x.ax,WTP_INTEGER) &&
		    w_unify(v7,t7,(PWord)in.x.bx,WTP_INTEGER) &&
		    w_unify(v8,t8,(PWord)in.x.cx,WTP_INTEGER) &&
		    w_unify(v9,t9,(PWord)in.x.dx,WTP_INTEGER) &&
		    w_unify(v10,t10,(PWord)in.l.eflags,WTP_INTEGER))
			SUCCEED
	}

	FAIL
}




#define GET_REGISTER(REG,V,T,BUF)										\
	switch (T) {														\
		case WTP_INTEGER:												\
			in.l.REG=(long)V;											\
			break;														\
		case WTP_SYMBOL:												\
			strcpy(BUF,TOKNAME(V));										\
			in.l.REG=(long)(&BUF[0]);									\
			break;														\
		case WTP_UIA:													\
			in.l.REG=(long)(M_FIRSTUIAWORD(V));							\
			break;														\
		case WTP_STRUCTURE:												\
			w_get_arity(&arity,V);										\
			w_get_functor(&functor,V);									\
			if (arity == 4 && functor == TK_DOUBLE) {					\
				for (i=0; i<4; i++) {									\
					w_get_argn(&val,&type,V,i+1);						\
					*(((short *)&dval)+i) = val;						\
				}														\
				in.l.REG=(long)dval;									\
			}															\
			else														\
				FAIL													\
			break;														\
		default:														\
			FAIL														\
	}
		

#define UNIFY_RET_REGISTER(REG,V,T,INV,INT)								\
	if (INT == WTP_UIA) {												\
		val = INV;														\
		type = INT;														\
	}																	\
	else																\
		make_number(&val,&type,(double)(in.l.REG));						\
	if (!w_unify(V,T,val,type))											\
		FAIL



/*
 * $$int86(IntNum,EAX,EBX,ECX,EDX,EBP,ESI,EDI,
 *		   RetEAX,RetEBX,RetECX,RetEDX,RetEBP,RetESI,RetEDI,RetEFlags)
 *
 * Gives Prolog access to the int86 call.
*/

pbi_int86_16arg()
{
	PWord v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16;
	int   t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16;
	PWord val, functor;
	int   type, arity, i;
	double dval;
	char buf_eax[UIA_MAX];
	char buf_ebx[UIA_MAX];
	char buf_ecx[UIA_MAX];
	char buf_edx[UIA_MAX];
	char buf_ebp[UIA_MAX];
	char buf_esi[UIA_MAX];
	char buf_edi[UIA_MAX];

	union REGS386 in;

	w_get_An(&v1,&t1,1);
	w_get_An(&v2,&t2,2);
	w_get_An(&v3,&t3,3);
	w_get_An(&v4,&t4,4);
	w_get_An(&v5,&t5,5);
	w_get_An(&v6,&t6,6);
	w_get_An(&v7,&t7,7);
	w_get_An(&v8,&t8,8);
	w_get_An(&v9,&t9,9);
	w_get_An(&v10,&t10,10);
	w_get_An(&v11,&t11,11);
	w_get_An(&v12,&t12,12);
	w_get_An(&v13,&t13,13);
	w_get_An(&v14,&t14,14);
	w_get_An(&v15,&t15,15);
	w_get_An(&v16,&t16,16);

	if (t1 != WTP_INTEGER) 
		FAIL

	GET_REGISTER(eax,v2,t2,buf_eax)
	GET_REGISTER(ebx,v3,t3,buf_ebx)
	GET_REGISTER(ecx,v4,t4,buf_ecx)
	GET_REGISTER(edx,v5,t5,buf_edx)
	GET_REGISTER(ebp,v6,t6,buf_ebp)
	GET_REGISTER(esi,v7,t7,buf_esi)
	GET_REGISTER(edi,v8,t8,buf_edi)

	int386((short)v1,&in,&in);

	/*
	 * make sure that eflags can be a Prolog integer.
	 * (we only need low 18 bits eflags register)
	 */
	in.l.eflags &= 0x03ffff;

	UNIFY_RET_REGISTER(eax,v9,t9,v2,t2)
	UNIFY_RET_REGISTER(ebx,v10,t10,v3,t3)
	UNIFY_RET_REGISTER(ecx,v11,t11,v4,t4)
	UNIFY_RET_REGISTER(edx,v12,t12,v5,t5)
	UNIFY_RET_REGISTER(ebp,v13,t13,v6,t6)
	UNIFY_RET_REGISTER(esi,v14,t14,v7,t7)
	UNIFY_RET_REGISTER(edi,v15,t15,v8,t8)

	if (!w_unify(v16,t16,(PWord)in.l.eflags,WTP_INTEGER))
		FAIL

	SUCCEED
}


#endif 	/* DOS */
#endif 	/* als_i386 */

