/*
 * ALS Prolog and Tcl/Tk 8.0 Interface
 * Copyright (c) 1997 by Applied Logic Systems, Inc.
 *
 */

#include <stdlib.h>
#include <string.h>

#ifdef macintosh
#define MAC_TCL 1
#include <Types.h>
#include <MacWindows.h>
#endif

#include <tcl.h>
#include <tk.h>

#include "alspi.h"
#include "new_alspi.h"

#include "version.h"

#ifdef UNIX
char *version[2] = {
  "@(#)(c) 1997 Applied Logic Systems, Inc.",
  "@(#)ALS Prolog Tcl-Tk Interface " VERSION_STRING " for " UNIX_NAME,
  };
#endif

static Tcl_ObjType *tcl_integer_type, *tcl_double_type, *tcl_list_type;

static AP_Obj TclToPrologObj(Tcl_Interp *interp, Tcl_Obj *tcl_obj, AP_World *w, AP_Obj *vars);
static Tcl_Obj *PrologToTclObj(AP_World *w, AP_Obj prolog_obj, Tcl_Interp *interp);


static AP_Obj AddVarList(AP_World *w, AP_Obj *vars, const char *name)
{
	AP_Obj i, pair;
	
	for (i = *vars; !AP_IsNullList(w, i); i = AP_ListTail(w, i)) {
		pair = AP_ListHead(w, i);
		if (!strcmp(name, AP_GetAtomStr(w, AP_GetArgument(w, pair, 1)))) break;
	}
	
	if (AP_IsNullList(w, i)) {
		pair = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "{}"), 2,
			AP_NewSymbolFromStr(w, name), AP_UNBOUND_OBJ);
		*vars = AP_NewInitList(w, pair, *vars);
	}
	
	return AP_GetArgument(w, pair, 2);
}

static AP_Obj TclToPrologObj0(Tcl_Interp *interp, Tcl_Obj *tcl_obj, AP_World *w, AP_Obj *vars)
{
	AP_Obj prolog_obj;
	
	if        (tcl_obj->typePtr == tcl_integer_type) {
		prolog_obj = AP_NewNumberFromLong(w, tcl_obj->internalRep.longValue);
	} else if (tcl_obj->typePtr == tcl_double_type) {
		prolog_obj = AP_NewFloatFromDouble(w, tcl_obj->internalRep.doubleValue);
	} else if (tcl_obj->typePtr == tcl_list_type) {
		int i, objc;
		AP_Obj list;
		Tcl_Obj **objv;
		
		Tcl_ListObjGetElements(interp, tcl_obj, &objc, &objv);
		
		for (i = objc-1, list = AP_NullList(w); i >= 0; i--) {
			list = AP_NewInitList(w, TclToPrologObj0(interp, objv[i], w, vars), list);
		}
		prolog_obj = list;
	} else {
		prolog_obj = AP_NewUIAFromStr(w, Tcl_GetStringFromObj(tcl_obj, NULL));
	}
	
	return prolog_obj;
}


static AP_Obj TclToPrologObj(Tcl_Interp *interp, Tcl_Obj *tcl_obj, AP_World *w, AP_Obj *vars)
{
	if (vars) *vars = AP_NullList(w);
	
	return TclToPrologObj0(interp, tcl_obj, w, vars);
}

static Tcl_Obj *PrologToTclObj(AP_World *w, AP_Obj prolog_obj, Tcl_Interp *interp)
{
	Tcl_Obj *tcl_obj;
	AP_Obj i;
	
	switch (AP_ObjType(w, prolog_obj)) {
	case AP_INTEGER:
		tcl_obj = Tcl_NewIntObj(AP_GetLong(w, prolog_obj));
		break;	
	case AP_FLOAT:
		tcl_obj = Tcl_NewDoubleObj(AP_GetDouble(w, prolog_obj));
		break;	
	case AP_ATOM:
		if (AP_IsNullList(w, prolog_obj)) {
			tcl_obj = Tcl_NewStringObj("", -1);		
		} else {
			tcl_obj = Tcl_NewStringObj((char *)AP_GetAtomStr(w, prolog_obj), -1);
		}
		break;
	case AP_LIST:
		tcl_obj = Tcl_NewListObj(0, NULL);
		for (i = prolog_obj; !AP_IsNullList(w, i); i = AP_ListTail(w, i)) {
			Tcl_ListObjAppendElement(interp, tcl_obj, PrologToTclObj(w, AP_ListHead(w, i), interp));
		}
		break;
	case AP_STRUCTURE:
		tcl_obj = Tcl_NewStringObj("structure", -1);
		break;
	case AP_VARIABLE:
		tcl_obj = Tcl_NewStringObj("variable", -1);
		break;
	}
	
	return tcl_obj;
}

/* Command Procedures for Tcl to Prolog Interface */

static AP_Obj tcltk_module;

static int
PrologToTclResult(Tcl_Interp *interp, AP_World *w, AP_Result prolog_result)
{
	switch (prolog_result) {
	case AP_SUCCESS:
	default:
		Tcl_SetObjResult(interp, Tcl_NewIntObj(1));
		return TCL_OK;
		break;
	case AP_FAIL:
		Tcl_SetObjResult(interp, Tcl_NewIntObj(0));
		return TCL_OK;
		break;
	case AP_EXCEPTION: {
		AP_Obj term_to_string, string;
		AP_Result r;
		term_to_string = AP_NewInitStructure(w,
						AP_NewSymbolFromStr(w, "term_to_string"),
						2,
						AP_GetException(w),
						AP_UNBOUND_OBJ);
		r = AP_Call(w, tcltk_module, &term_to_string);
		string = AP_GetArgument(w, term_to_string, 2);
		
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp,
			"prolog exception: ",
			AP_GetAtomStr(w, string),
			NULL);
		return TCL_ERROR;
		break;
		}
	}
}

static AP_Result
TclToPrologResult(AP_World *w, AP_Obj *value, Tcl_Interp *interp, int tcl_result)
{
	AP_Obj result = TclToPrologObj(interp, Tcl_GetObjResult(interp), w, NULL);
	/* error check */
	
	if (tcl_result == TCL_OK) {
		if (value) return AP_Unify(w, *value, result);
		else return AP_SUCCESS;
	} else {
		return AP_SetException(w,
			AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "error"), 2,
				AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "tcl_error"), 1, result),
				AP_UNBOUND_OBJ));
	}
}

static int
Tcl_ALS_Prolog_Read_Call(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
	AP_World *w = prolog_world;
	const char *s;
	AP_Obj call;
	AP_Result result;

	if (objc < 3) {
		Tcl_WrongNumArgs(interp, 1, objv, "termString ?varName ...?");
		return TCL_ERROR;
	}
	
	s = Tcl_GetStringFromObj(objv[2], NULL);
	
	call = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "read_eval_results"), 2,
					AP_NewUIAFromStr(w, s), AP_UNBOUND_OBJ);
			
	result = AP_Call(w, tcltk_module, &call);
	
	if (result == AP_SUCCESS) {
		int i;
		AP_Obj j, vars = AP_GetArgument(w, call, 2);
		
		for (i = 3, j = vars; !AP_IsNullList(w, j) && i < objc; i++, j = AP_ListTail(w, j)) {
			Tcl_ObjSetVar2(interp, objv[i], NULL, PrologToTclObj(w, AP_ListHead(w, j), interp), 0);
		}

		if (i < objc) {
			int k;
			Tcl_ResetResult(interp);
			Tcl_AppendResult(interp, "unset variables: ", NULL);
			for (k = i; k < objc; k++) {
				Tcl_AppendResult(interp,
					Tcl_GetStringFromObj(objv[k], NULL),
					NULL);
			}
			return TCL_ERROR;
		}
	}
	
	return PrologToTclResult(interp, w, result);
}

static int
Tcl_ALS_Prolog_Call(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
	AP_World *w = prolog_world;
	const char *name;
	AP_Obj module, functor, call, vars, wrap_call;
	AP_Result result;

	if (objc < 4 || (objc%2) != 0) {
		Tcl_WrongNumArgs(interp, 1, objv, "call module functor ?-type arg ...?");
		return TCL_ERROR;
	}
	
	module = AP_NewSymbolFromStr(w, Tcl_GetStringFromObj(objv[2], NULL));
	functor = AP_NewSymbolFromStr(w, Tcl_GetStringFromObj(objv[3], NULL));
		
	if (objc == 4) {
		call = functor;
		vars = AP_NullList(w);
	} else {
		int i, a, argc, option;
		AP_Obj arg;
		
		enum {NUMBER, ATOM, LIST, VAR};
		char *callOptions[] = {"-number", "-atom", "-list", "-var", NULL};
		argc = (objc-4)/2;

		call = AP_NewStructure(w, functor, argc);
		
		for (a = 0, i = 4, vars = AP_NullList(w); a < argc; a++, i+=2) {
			if (Tcl_GetIndexFromObj(NULL, objv[i], callOptions, "", TCL_EXACT, &option) == TCL_OK) {
				switch (option) {
				case NUMBER:
					if (Tcl_ConvertToType(interp, objv[i+1], tcl_integer_type) == TCL_OK
						|| Tcl_ConvertToType(interp, objv[i+1], tcl_double_type) == TCL_OK)
						arg = TclToPrologObj0(interp, objv[i+1], w, &vars);
					else return TCL_ERROR;
					break;
				case ATOM:
					arg = AP_NewUIAFromStr(w, Tcl_GetStringFromObj(objv[i+1], NULL));
					break;
				case LIST:
					if (Tcl_ConvertToType(interp, objv[i+1], tcl_list_type) != TCL_OK)
						return TCL_ERROR;
					arg = TclToPrologObj0(interp, objv[i+1], w, &vars);
					break;
				case VAR:
					name = Tcl_GetStringFromObj(objv[i+1], NULL);
					if (!strcmp(name, "_")) arg = AP_UNBOUND_OBJ;
					else arg = AddVarList(w, &vars, name);
					break;
				}
			} else {
				Tcl_WrongNumArgs(interp, 2, objv,
					"module functor ?-type arg ...?"
				);
				return TCL_ERROR;
			}
			
			AP_Unify(w, arg, AP_GetArgument(w, call, a+1));
		}
	}
		
	/* Wrap up call and variable list, so it will be updated in
	   the event of a gc. */
	wrap_call = AP_NewInitStructure(w,
					AP_NewSymbolFromStr(w, "eval_results"), 3,
					module, call, vars);

	result = AP_Call(w, tcltk_module, &wrap_call);
	
	if (result == AP_SUCCESS) {
		AP_Obj v, pair, value;
		
		/* Get the new value of vars */
		vars = AP_GetArgument(w, wrap_call, 3);

		for (v = vars; !AP_IsNullList(w, v); v = AP_ListTail(w, v)) {
			pair = AP_ListHead(w, v);
			name = AP_GetAtomStr(w, AP_GetArgument(w, pair, 1));
			if (*name) {
				value = AP_GetArgument(w, pair, 2);
				Tcl_ObjSetVar2(interp, Tcl_NewStringObj((char *)name, -1), NULL, PrologToTclObj(w, value, interp), 0);
			}
		}
	}
	
	return PrologToTclResult(interp, w, result);
}

static int
Tcl_ALS_Prolog_ObjCmd(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
	enum {PROLOG_CALL, PROLOG_READ_CALL, PROLOG_INTERRUPT};
	char *prologOptions[] = {"call", "read_call", "interrupt", NULL};
	int option;

	if (objc < 2) {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}
	
	if (Tcl_GetIndexFromObj(interp, objv[1], prologOptions, "option", TCL_EXACT, &option)
		!= TCL_OK) {
		return TCL_ERROR;
	}
	
	switch (option) {
	case PROLOG_CALL:
	default:
		return Tcl_ALS_Prolog_Call(prolog_world, interp, objc, objv);
		break;
	case PROLOG_READ_CALL:
		return Tcl_ALS_Prolog_Read_Call(prolog_world, interp, objc, objv);
		break;
	case PROLOG_INTERRUPT:
		/* PI_interrupt();*/
		return TCL_OK;
		break;
	}
}

static int
Tcl_DoOneEventCmd(ClientData data, Tcl_Interp *interp, int objc, Tcl_Obj *const *objv)
{
#pragma unused(data)

	int index, result;
	
	enum {EVENT_WAIT, EVENT_DONT_WAIT};
    static char *eventOptions[] = {"wait", "dont_wait", (char *) NULL};

	if (objc != 2) {
		Tcl_WrongNumArgs(interp, 1, objv, "option");
		return TCL_ERROR;
	}
	
    if (Tcl_GetIndexFromObj(interp, objv[1], eventOptions, "option", 0, &index)
	    != TCL_OK) {
    	return TCL_ERROR;
    }

	switch (index) {
	case EVENT_WAIT:
		result = Tcl_DoOneEvent(0);
		break;
	case EVENT_DONT_WAIT:
		result = Tcl_DoOneEvent(TCL_DONT_WAIT);
		break;
	}
	
	Tcl_SetObjResult(interp, Tcl_NewIntObj(result));
	return TCL_OK;
}

/* Predicates for Prolog to Tcl Interface */

static Tcl_HashTable tcl_interp_name_table;

static int interp_count = 0;

#ifdef macintosh
/* In order to control the event and menu handling, we need these internal
   variables and functions. Someday I hope there will be a public interface
   for these. */

/* From tclMacInt.h */
typedef int (*TclMacConvertEventPtr) _ANSI_ARGS_((EventRecord *eventPtr));
void 	Tcl_MacSetEventProc _ANSI_ARGS_((TclMacConvertEventPtr procPtr));

/* From tkMacInt.h */
extern int		TkMacConvertEvent _ANSI_ARGS_((EventRecord *eventPtr));
extern void		TkMacInitAppleEvents _ANSI_ARGS_((Tcl_Interp *interp));
extern void 		TkMacInitMenus _ANSI_ARGS_((Tcl_Interp 	*interp));
extern QDGlobalsPtr tcl_macQdPtr;

static int MyConvertEvent(EventRecord *event)
{
	if (SIOUXIsAppWindow(FrontWindow())) {
		if (SIOUXHandleOneEvent(event)) return 0;
		return TkMacConvertEvent(event);
	} else {
		if (TkMacConvertEvent(event)) return 1;
		SIOUXHandleOneEvent(event);
		return 0;
	}
}

static short MyHandleOneEvent(EventRecord *)
{
	Tcl_DoOneEvent(TCL_ALL_EVENTS|TCL_DONT_WAIT);
	return 1;
}
#endif

static int ALSProlog_Package_Init(Tcl_Interp *interp, AP_World *w)
{
  if (!Tcl_PkgRequire(interp, "Tcl", "8.0", 0)
      || !Tcl_CreateObjCommand(interp, "prolog", Tcl_ALS_Prolog_ObjCmd, w, NULL)
      || !Tcl_CreateObjCommand(interp, "dooneevent", Tcl_DoOneEventCmd, w, NULL))
    {
      return TCL_ERROR;
    }
  
  return Tcl_PkgProvide(interp, "ALSProlog", VERSION_STRING);
}

static AP_Result built_interp(AP_World *w, Tcl_Interp **interpretor, AP_Obj *interp_name)
{
	Tcl_Interp *interp;
	char name[128];
	const char *namep;
	Tcl_HashEntry *entry;
	int is_new, pre_named;
	AP_Type type;
	int r;

	type = AP_ObjType(w, *interp_name);
	
	if (type != AP_VARIABLE && type != AP_ATOM) { 
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom_or_variable"), *interp_name);
		goto error;
	}
	
	pre_named = (type == AP_ATOM);

#ifdef macintosh
//	Tcl_MacSetEventProc(MyConvertEvent);
//	SIOUXSetEventVector(MyHandleOneEvent);
#endif

	interp = Tcl_CreateInterp();
	if (!interp) {
		AP_SetStandardError(w, AP_RESOURCE_ERROR, AP_NewSymbolFromStr(w, "tcl_memory"));
		goto error;
	}

	{
		Tcl_DString path;
		char *elements[3];
		Tcl_DStringInit(&path);
		elements[0] = library_dir;
#ifdef macintosh
		elements[1] = "Tool Command Language";
#else
		elements[1] = "lib";
#endif
		elements[2] = "tcl" TCL_VERSION;
		Tcl_JoinPath(3, elements, &path);
		Tcl_SetVar(interp, "tcl_library", path.string, TCL_GLOBAL_ONLY);
		Tcl_DStringSetLength(&path, 0);
		Tcl_JoinPath(2, elements, &path);
		Tcl_SetVar(interp, "tcl_pkgPath", path.string, TCL_GLOBAL_ONLY|TCL_LIST_ELEMENT);
		Tcl_SetVar(interp, "autopath", "", TCL_GLOBAL_ONLY);
		Tcl_DStringFree(&path);
	}
	
	r = Tcl_Init(interp);

	if (r != TCL_OK) {
		TclToPrologResult(w, NULL, interp, r);
		goto error_delete;
	}

	if (pre_named) {
		namep = AP_GetAtomStr(w, *interp_name);
	} else {
		interp_count++;
		sprintf(name, "tcl_interp%d", interp_count);
		/* handle error */
		namep = name;
	}
	
	entry = Tcl_CreateHashEntry(&tcl_interp_name_table, namep, &is_new);
	if (!entry) {
		AP_SetStandardError(w, AP_RESOURCE_ERROR, AP_NewSymbolFromStr(w, "tcl_memory"));
		goto error_delete;
	}
	
	if (!is_new) {
		AP_SetStandardError(w, AP_PERMISSION_ERROR,
					AP_NewSymbolFromStr(w, "create"),
					AP_NewSymbolFromStr(w, "tcl_interpreter"), *interp_name);
		goto error_delete;
	} 			
	
	Tcl_SetHashValue(entry, interp);


	if (ALSProlog_Package_Init(interp, w) != TCL_OK) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "tcl_create_command_error"));
		goto error_delete;
	}

	*interpretor = interp;

	return (pre_named) ? AP_SUCCESS : AP_Unify(w, *interp_name, AP_NewUIAFromStr(w, namep));
	
error_delete:
	Tcl_DeleteInterp(interp);
error:
	return AP_EXCEPTION;
}

static AP_Result tcl_new(AP_World *w, AP_Obj interp_name)
{
	Tcl_Interp *interp;
	
	return built_interp(w, &interp, &interp_name);
}

static AP_Result tk_new(AP_World *w, AP_Obj interp_name)
{
	AP_Result result;
	Tcl_Interp *interp;
	
	result = built_interp(w, &interp, &interp_name);
	
	{
		Tcl_DString path;
		char *elements[3];
		Tcl_DStringInit(&path);
		elements[0] = library_dir;
#ifdef macintosh
		elements[1] = "Tool Command Language";
#else
		elements[1] = "lib";
#endif
		elements[2] = "tk" TK_VERSION;
		Tcl_JoinPath(3, elements, &path);
		Tcl_SetVar(interp, "tk_library", path.string, TCL_GLOBAL_ONLY);
		Tcl_DStringFree(&path);
	}

	if (result == AP_SUCCESS) {
		int r = Tk_Init(interp);
		if (r != TCL_OK) {
			TclToPrologResult(w, NULL, interp, r);
			return AP_EXCEPTION;
		}

		/*Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);*/

	#ifdef macintosh
		//TkMacInitAppleEvents(interp);
		//TkMacInitMenus(interp);

		//Tcl_SetVar(interp, "tcl_rcRsrcName", "tclshrc", TCL_GLOBAL_ONLY);
	#endif		
	}
	
	return result;
}


static Tcl_Interp *GetInterp(AP_World *w, AP_Obj interp_name)
{
	Tcl_HashEntry *entry;

	if (AP_ObjType(w, interp_name) != AP_ATOM) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
				AP_NewSymbolFromStr(w, "atom"), interp_name);
		return NULL;
	}
	
	entry = Tcl_FindHashEntry(&tcl_interp_name_table, AP_GetAtomStr(w, interp_name));
	
	if (!entry) {
		AP_SetStandardError(w, AP_DOMAIN_ERROR,
			AP_NewSymbolFromStr(w, "tcl_interpreter"), interp_name);
		return NULL;
	}
	
	return Tcl_GetHashValue(entry);
}

typedef enum {one_arg, arg_list} EvalOption;

static AP_Result tcl_eval0(AP_World *w, AP_Obj interp_name, AP_Obj command, AP_Obj result,
					EvalOption option)
{
	Tcl_Interp *interp;
	Tcl_Obj *tcl_command, *eval_string;
	int r;

	interp = GetInterp(w, interp_name);
	if (!interp) return AP_EXCEPTION;
	
	tcl_command = PrologToTclObj(w, command, interp);
	if (option == arg_list) {
		eval_string = Tcl_NewStringObj("eval", -1);
		if (!tcl_command || !eval_string) {
			return AP_SetStandardError(w, AP_RESOURCE_ERROR, AP_NewSymbolFromStr(w, "tcl_memory"));
		}
		Tcl_ListObjReplace(interp, tcl_command, 0, 0, 1, &eval_string);
	}

	r = Tcl_EvalObj(interp, tcl_command);
	/* error check */
	
	/* Hack to refresh result */
	PI_getan(&result.p, &result.t, 3);

	return TclToPrologResult(w, &result, interp, r);
}

static AP_Result tcl_call(AP_World *w, AP_Obj interp_name, AP_Obj command, AP_Obj result)
{
	return tcl_eval0(w, interp_name, command, result, one_arg);
}

static AP_Result tcl_eval(AP_World *w, AP_Obj interp_name, AP_Obj command, AP_Obj result)
{
	return tcl_eval0(w, interp_name, command, result, arg_list);
}

static AP_Result tcl_delete(AP_World *w, AP_Obj interp_name)
{
	Tcl_HashEntry *entry;
	Tcl_Interp *interp;

	if (AP_ObjType(w, interp_name) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), interp_name);
	}
	
	entry = Tcl_FindHashEntry(&tcl_interp_name_table, AP_GetAtomStr(w, interp_name));
	
	if (!entry) {
		return AP_SetStandardError(w, AP_DOMAIN_ERROR,
					AP_NewSymbolFromStr(w, "tcl_interpreter"), interp_name);
	}
	
	interp = Tcl_GetHashValue(entry);
	
	Tcl_DeleteInterp(interp);
	
	Tcl_DeleteHashEntry(entry);
	
	return AP_SUCCESS;
}

static AP_Result tcl_delete_all(AP_World *ignore)
{
#pragma unused(ignore)
	Tcl_HashEntry *entry;
	Tcl_HashSearch search;

	for (entry = Tcl_FirstHashEntry(&tcl_interp_name_table, &search);
		entry; entry = Tcl_NextHashEntry(&search)) {
		Tcl_DeleteInterp(Tcl_GetHashValue(entry)); 
	}
	
	Tcl_DeleteHashTable(&tcl_interp_name_table);
	
	Tcl_InitHashTable(&tcl_interp_name_table, TCL_STRING_KEYS);

	return AP_SUCCESS;
}

static AP_Result tk_main_loop(AP_World *ignore)
{
#pragma unused(ignore)

	Tk_MainLoop();
	return AP_SUCCESS;
}

static AP_Result tcl_coerce_number(AP_World *w, AP_Obj interp_name, AP_Obj item, AP_Obj atom)
{	
	Tcl_Interp *interp;
	AP_Obj result;
	
	interp = GetInterp(w, interp_name);
	if (!interp) return AP_EXCEPTION;
	
	if (AP_ObjType(w, item) == AP_INTEGER
		|| AP_ObjType(w, item) == AP_FLOAT) result = item;
	else {
		Tcl_Obj *tcl_obj = PrologToTclObj(w, item, interp);
		int r;
		r = Tcl_ConvertToType(interp, tcl_obj, tcl_integer_type);
		if (r != TCL_OK)
			r = Tcl_ConvertToType(interp, tcl_obj, tcl_double_type);
		if (r != TCL_OK)
			return AP_SetException(w,
				AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "error"), 2,
					AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "tcl_error"), 1,
						TclToPrologObj(interp, Tcl_GetObjResult(interp), w, NULL)),
					AP_UNBOUND_OBJ));
		result = TclToPrologObj(interp, tcl_obj, w, NULL);
		Tcl_DecrRefCount(tcl_obj);
	}

	return AP_Unify(w, result, atom);
}

static AP_Result tcl_coerce_atom(AP_World *w, AP_Obj interp_name, AP_Obj item, AP_Obj atom)
{	
	Tcl_Interp *interp;
	AP_Obj result;
	
	interp = GetInterp(w, interp_name);
	if (!interp) return AP_EXCEPTION;
	
	if (AP_ObjType(w, item) == AP_ATOM) result = item;
	else {
		Tcl_Obj *tcl_obj = PrologToTclObj(w, item, interp);
		result = AP_NewUIAFromStr(w, Tcl_GetStringFromObj(tcl_obj, NULL));
		Tcl_DecrRefCount(tcl_obj);
	}

	return AP_Unify(w, result, atom);
}

static AP_Result tcl_coerce_list(AP_World *w, AP_Obj interp_name, AP_Obj item, AP_Obj list)
{
	Tcl_Interp *interp;
	AP_Obj result;

	interp = GetInterp(w, interp_name);
	if (!interp) return AP_EXCEPTION;
	
	if (AP_ObjType(w, item) == AP_LIST || AP_IsNullList(w, item)) result = item;
	else {
		Tcl_Obj *tcl_obj = PrologToTclObj(w, item, interp);
		int r = Tcl_ConvertToType(interp, tcl_obj, tcl_list_type);
		if (r != TCL_OK)
			return AP_SetException(w,
				AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "error"), 2,
					AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "tcl_error"), 1,
						TclToPrologObj(interp, Tcl_GetObjResult(interp), w, NULL)),
					AP_UNBOUND_OBJ));
		result = TclToPrologObj(interp, tcl_obj, w, NULL);
		Tcl_DecrRefCount(tcl_obj);
	}
	
	return AP_Unify(w, result, list);
}

/* Glue routines to translate from old to new ALS Prolog C interfaces. */

static int glue_tcl_new(void) {return AP_OldToNewCall(tcl_new, 1);}
static int glue_tk_new(void) {return AP_OldToNewCall(tk_new, 1);}
static int glue_tcl_delete(void) {return AP_OldToNewCall(tcl_delete, 1);}
static int glue_tcl_delete_all(void) {return AP_OldToNewCall(tcl_delete_all, 0);}
static int glue_tcl_call(void) {return AP_OldToNewCall(tcl_call, 3);}
static int glue_tcl_eval(void) {return AP_OldToNewCall(tcl_eval, 3);}
static int glue_tcl_coerce_number(void) {return AP_OldToNewCall(tcl_coerce_number, 3);}
static int glue_tcl_coerce_atom(void) {return AP_OldToNewCall(tcl_coerce_atom, 3);}
static int glue_tcl_coerce_list(void) {return AP_OldToNewCall(tcl_coerce_list, 3);}
static int glue_tk_main_loop(void) {return AP_OldToNewCall(tk_main_loop, 0);}

PI_BEGIN
	PI_MODULE("tcltk")

	PI_DEFINE("tcl_new",1,glue_tcl_new)
	PI_DEFINE("tk_new",1,glue_tk_new)

	PI_DEFINE("tcl_delete",1,glue_tcl_delete)
	PI_DEFINE("tcl_delete_all",0,glue_tcl_delete_all)

	PI_DEFINE("tcl_call",3,glue_tcl_call)
	PI_DEFINE("tcl_eval",3,glue_tcl_eval)

	PI_DEFINE("tcl_coerce_number", 3, glue_tcl_coerce_number)
	PI_DEFINE("tcl_coerce_atom", 3, glue_tcl_coerce_atom)
	PI_DEFINE("tcl_coerce_list", 3, glue_tcl_coerce_list)

	PI_DEFINE("tk_main_loop",0,glue_tk_main_loop)
PI_END


void pi_init(void)
{
#ifdef macintosh
	tcl_macQdPtr = &qd /*GetQD()*/;
#endif

#ifdef UNIX
	Tcl_FindExecutable((char *)executable_path);
#endif

	Tcl_InitHashTable(&tcl_interp_name_table, TCL_STRING_KEYS);
	
	/* Get pointers to the standard Tcl types. */
	tcl_integer_type = Tcl_GetObjType("int");
	tcl_double_type = Tcl_GetObjType("double");
	tcl_list_type = Tcl_GetObjType("list");
		
	tcltk_module = AP_NewSymbolFromStr(NULL, "tcltk");
	
	PI_INIT;
}

#if 0
#pragma export on
int Alsprolog_Init(Tcl_Interp *interp);
int Alsprolog_Init(Tcl_Interp *interp)
#pragma export reset
{
	AP_Obj consult;
  printf("about to pi_prolog_init\n");
  PI_prolog_init(0, NULL);
  printf("about to pi_init\n");
  pi_init();
  consult = 
  	AP_NewInitStructure(NULL, AP_NewSymbolFromStr(NULL, "xconsult"), 1,
  		AP_NewSymbolFromStr(NULL, "tcltk.pro"), AP_UNBOUND_OBJ);
  AP_Call(NULL, AP_NewSymbolFromStr(NULL, "builtins"), &consult);
  return ALSProlog_Package_Init(interp, NULL);
}
#endif
