#include "new_alspi.h"

#include "alspi.h"

AP_API(AP_Result) AP_Unify(AP_World *w, AP_Obj a, AP_Obj b)
{
	if (PI_unify(a.p, a.t, b.p, b.t)) return AP_SUCCESS;
	else return AP_FAIL;
}

AP_API(AP_Obj) AP_MakeUIAFromStr(AP_World *w, const char *s)
{
	AP_Obj r;
	
	PI_makeuia(&r.p, &r.t, s);
	
	return r;
}

AP_API(AP_Obj) AP_NewNumberFromLong(AP_World *w, long n)
{
	AP_Obj r;
	
	r.p = n;
	r.t = PI_INT;	
	
	return r;
}

AP_API(AP_Obj) AP_NewFloatFromDouble(AP_World *w, double d)
{
	AP_Obj r;
	
	PI_makedouble(&r.p, &r.t, d);
	
	return r;
}

AP_API(AP_Obj) AP_NullList(AP_World *w)
{
	AP_Obj r;
	
	PI_makesym(&r.p, &r.t, "[]");
	
	return r;
}

AP_API(AP_Obj) AP_NewList(AP_World *w, AP_Obj head, AP_Obj tail)
{
	AP_Obj r, car, cdr;
	
	PI_makelist(&r.p, &r.t);
	PI_gethead(&car.p, &car.t, r.p);
	PI_unify(car.p, car.t, head.p, head.t);
	PI_gettail(&cdr.p, &cdr.t, r.p);
	PI_unify(cdr.p, cdr.t, tail.p, tail.t);
	
	return r;
}

AP_API(AP_Type) AP_ObjType(AP_World *w, AP_Obj obj)
{
	switch (obj.t) {
	case PI_VAR:
		return AP_VARIABLE;
		break;
	case PI_LIST:
		return AP_LIST;
		break;
	case PI_STRUCT:
		return AP_STRUCTURE;
		break;
	case PI_SYM:
	case PI_UIA:
		return AP_ATOM;
		break;
	case PI_INT:
		return AP_INTEGER;
		break;
	case PI_DOUBLE:
		return AP_FLOAT;
		break;
	}
	
	return AP_VARIABLE;
}

AP_API(long) AP_GetLong(AP_World *w, AP_Obj obj)
{
	return obj.p;
}

AP_API(double) AP_GetDouble(AP_World *w, AP_Obj obj)
{
	double d;
	
	PI_getdouble(&d, obj.p);
	
	return d;	
}

AP_API(const char *) AP_GetAtomStr(AP_World *w, AP_Obj obj)
{
	const char *s;
	
	switch (obj.t) {
	case PI_SYM:
		s = PI_getsymname(NULL, obj.p, obj.t);
		break;
	case PI_UIA:
		s = PI_getuianame(NULL, obj.p, obj.t);
		break;
	}

	return s;
}

AP_API(int) AP_IsNullList(AP_World *w, AP_Obj obj)
{
	
	return AP_Unify(w, obj, AP_NullList(w)) == AP_SUCCESS;
}

AP_API(AP_Obj) AP_ListHead(AP_World *w,  AP_Obj list)
{
	AP_Obj r;
	
	PI_gethead(&r.p, &r.t, list.p);
	
	return r;
}

AP_API(AP_Obj) AP_ListTail(AP_World *w,  AP_Obj list)
{
	AP_Obj r;
	
	PI_gettail(&r.p, &r.t, list.p);
	
	return r;
}

AP_API(AP_Obj) AP_MakeStructure(AP_World *w, AP_Obj functor, int arity, ...)
{
	va_list ap;
	AP_Obj r, arg, arg_value;
	int i;
	
	PI_makestruct(&r.p, &r.t, functor.p, arity);

	va_start(ap, arity);
	for (i = 0; i < arity; i++) {
		arg_value = va_arg(ap, AP_Obj);
		if (arg_value.p != AP_UNBOUND_OBJ.p || arg_value.t != AP_UNBOUND_OBJ.t) {
			PI_getargn(&arg.p, &arg.t, r.p, i+1);
			PI_unify(arg.p, arg.t, arg_value.p, arg_value.t);
		}
	}
	va_end(va);
	
	return r;
}

static AP_Obj exception;
AP_API(AP_Result) AP_Call(AP_World *w, AP_Obj module, AP_Obj *term)
{
	AP_Obj catch_term, caught;

	catch_term = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "catch"), 3,
						*term, AP_UNBOUND_OBJ, AP_MakeSymbolFromStr(w, "true")); 
	if (PI_rungoal_with_update(module.p, &catch_term.p, &catch_term.t)) {
		*term = AP_GetArgument(w, catch_term, 1);
		caught = AP_GetArgument(w, catch_term, 2);
		if (AP_ObjType(w, caught) == AP_VARIABLE) return AP_SUCCESS;
		else {
			exception = caught;
			return AP_EXCEPTION;
		}
	} else {
		return AP_FAIL;
	} 
}

AP_API(AP_Obj) AP_MakeSymbolFromStr(AP_World *w, const char *s)
{
	AP_Obj r;
	PI_makesym(&r.p, &r.t, s); 
	return r;
}

AP_API(AP_Obj) AP_GetArgument(AP_World *w, AP_Obj structure, int index)
{
	AP_Obj r;
	PI_getargn(&r.p, &r.t, structure.p, index); 
	return r;
}

AP_Obj AP_UNBOUND_OBJ = {0, 0};

AP_API(AP_Result) AP_SetException(AP_World *w, AP_Obj obj)
{
	PI_throw(obj.p, obj.t);
	return AP_EXCEPTION;
}

AP_API(AP_Result) AP_SetError(AP_World *w, AP_Obj error_term)
{
	AP_Obj error;
	
	error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "error"), 2,
					error_term, AP_NullList(w));
	
	return AP_SetException(w, error);
}

AP_API(AP_Result) AP_SetStandardError(AP_World *w, AP_StandardError error_type, ...)
{
	va_list ap;
	AP_Obj error, obj1, obj2, obj3;
	
	va_start(ap, error_type);

	switch (error_type) {
	case AP_INSTANTIATION_ERROR:
	case AP_SYSTEM_ERROR:
		break;
	case AP_REPRESENTATION_ERROR:
	case AP_EVALUATION_ERROR:
	case AP_RESOURCE_ERROR:
	case AP_SYNTAX_ERROR:
		obj1 = va_arg(ap, AP_Obj);
		break;
	case AP_TYPE_ERROR:
	case AP_DOMAIN_ERROR:
	case AP_EXISTENCE_ERROR:
		obj1 = va_arg(ap, AP_Obj);
		obj2 = va_arg(ap, AP_Obj);
		break;
	case AP_PERMISSION_ERROR:
		obj1 = va_arg(ap, AP_Obj);
		obj2 = va_arg(ap, AP_Obj);
		obj3 = va_arg(ap, AP_Obj);
		break;
	}

	switch (error_type) {
	case AP_INSTANTIATION_ERROR:
		error = AP_MakeSymbolFromStr(w, "instantiation_error");
		break;
	case AP_TYPE_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "type_error"), 2, obj1, obj2);
		break;
	case AP_DOMAIN_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "domain_error"), 2, obj1, obj2);
		break;
	case AP_EXISTENCE_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "existence_error"), 2, obj1, obj2);
		break;
	case AP_PERMISSION_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "permission_error"), 3, obj1, obj2, obj3);
		break;
	case AP_REPRESENTATION_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "representation_error"), 1, obj1); 
		break;
	case AP_EVALUATION_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "evaluation_error"), 1, obj1); 
		break;
	case AP_RESOURCE_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "resource_error"), 1, obj1); 
		break;
	case AP_SYNTAX_ERROR:
		error = AP_MakeStructure(w, AP_MakeSymbolFromStr(w, "syntax_error"), 1, obj1); 
		break;
	case AP_SYSTEM_ERROR:
	default:
		error = AP_MakeSymbolFromStr(w, "system_error");
		break;
	}
	va_end(va);
	
	return AP_SetError(w, error);
}

AP_API(AP_Obj) AP_GetException(AP_World *w)
{
	return exception;
}

typedef AP_Result (*call0)(AP_World *);
typedef AP_Result (*call1)(AP_World *, AP_Obj);
typedef AP_Result (*call2)(AP_World *, AP_Obj, AP_Obj);
typedef AP_Result (*call3)(AP_World *, AP_Obj, AP_Obj, AP_Obj);
typedef AP_Result (*call4)(AP_World *, AP_Obj, AP_Obj, AP_Obj, AP_Obj);

int AP_OldToNewCall(AP_Result (*new_func)(), int arity)
{
	AP_Obj arg[10];
	int i;
	AP_Result r;
	call0 c0;
	call1 c1;
	call2 c2;
	call3 c3;

	
	for (i = 0; i < arity; i++)
		PI_getan(&arg[i].p, &arg[i].t, i+1);

	switch (arity) {
	case 0: c0 = new_func; break;
	case 1: c1 = new_func; break;
	case 2: c2 = new_func; break;
	case 3: c3 = new_func; break;
	}

	switch (arity) {
	case 0: r = c0(NULL); break;
	case 1: r = c1(NULL, arg[0]); break;
	case 2: r = c2(NULL, arg[0], arg[1]); break;
	case 3: r = c3(NULL, arg[0], arg[1], arg[2]); break;
	}
	
	if (r == AP_SUCCESS) PI_SUCCEED;
	else PI_FAIL;
}

#if 0
Someday I will convert to this mode:

AP_API(int) AP_IsAtom(AP_World *w, AP_Obj obj)
{
	int tag =  MTP_TAG(obj);
	return tag == WTP_SYMBOL || tag == WTP_UIA;
}

AP_API(AP_Obj) AP_ListHead(AP_World *w, AP_Obj list)
{
	PWord *p = (PWord *) list;
	return (AP_Obj) deref(*list);
}
AP_API(AP_Obj) AP_ListTail(AP_World *w, AP_Obj list)
{
	PWord *p = (PWord *) list;
	return (AP_Obj) deref(*(list+1));
}
#endif
