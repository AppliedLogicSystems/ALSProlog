#ifdef macintosh
#define MAC_TCL 1
#endif

#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>

#include "new_alspi.h"

static Tcl_ObjType *tcl_integer_type, *tcl_double_type, *tcl_list_type;

static AP_Obj TclToPrologObj(Tcl_Interp *interp, Tcl_Obj *tcl_obj, AP_World *w, AP_Obj *vars);
static Tcl_Obj *PrologToTclObj(AP_World *w, AP_Obj prolog_obj, Tcl_Interp *interp);

static void FreeStructInternalRep(Tcl_Obj *structPtr);
static void DupStructInternalRep(Tcl_Obj *srcPtr, Tcl_Obj *copyPtr);
static void UpdateStringOfStruct(Tcl_Obj *structPtr);
static int SetStructFromAny(Tcl_Interp *interp, Tcl_Obj *structPtr);

static void FreeVarInternalRep(Tcl_Obj *structPtr);
static void DupVarInternalRep(Tcl_Obj *srcPtr, Tcl_Obj *copyPtr);
static void UpdateStringOfVar(Tcl_Obj *structPtr);
static int SetVarFromAny(Tcl_Interp *interp, Tcl_Obj *structPtr);

static Tcl_ObjType
	tclStructType = {
		"structure",
		FreeStructInternalRep,
		DupStructInternalRep,
		UpdateStringOfStruct,
		SetStructFromAny
		},
	tclVarType = {
		"variable",
		FreeVarInternalRep,
		DupVarInternalRep,
		UpdateStringOfVar,
		SetVarFromAny
	};

typedef struct {
	int elemCount;			/* Current number of elements. */
	Tcl_Obj **elements;		/* Array of pointers to element objects. */
} StructRep;

static Tcl_Obj *Tcl_NewStructObj(int objc, Tcl_Obj **objv)
{
	Tcl_Obj *structPtr, **elemPtrs;
	StructRep *structRepPtr;
	int i;

	structPtr = Tcl_NewObj();
	
	elemPtrs = (Tcl_Obj **) ckalloc((unsigned) (objc * sizeof(Tcl_Obj *)));
	
	for (i = 0; i < objc; i++) {
	    elemPtrs[i] = objv[i];
	    Tcl_IncrRefCount(elemPtrs[i]);
	}
	
	structRepPtr = (StructRep *) ckalloc(sizeof(StructRep));
	structRepPtr->elemCount    = objc;
	structRepPtr->elements     = elemPtrs;
	
	structPtr->internalRep.otherValuePtr = (VOID *) structRepPtr;
	structPtr->typePtr = &tclStructType;
	
    return structPtr;
}

static Tcl_Obj *Tcl_NewStructObjFromAP_Obj(Tcl_Interp *interp, AP_World *w, AP_Obj struc)
{
	Tcl_Obj *structPtr, **elemPtrs;
	StructRep *structRepPtr;
	AP_Obj functor;
	int arity, i;

	functor = AP_GetStructureFunctor(w, struc);
	arity = AP_GetStructureArity(w, struc);
	
	structPtr = Tcl_NewObj();
	
	elemPtrs = (Tcl_Obj **) ckalloc((arity+1) * sizeof(Tcl_Obj *));
	
	elemPtrs[0] = PrologToTclObj(w, functor, interp);
	
	for (i = 1; i <= arity; i++) {
	    elemPtrs[i] = PrologToTclObj(w, AP_GetArgument(w, struc, i), interp);
	    Tcl_IncrRefCount(elemPtrs[i]);
	}
	
	structRepPtr = (StructRep *) ckalloc(sizeof(StructRep));
	structRepPtr->elemCount    = arity+1;
	structRepPtr->elements     = elemPtrs;
	
	structPtr->internalRep.otherValuePtr = (VOID *) structRepPtr;
	structPtr->typePtr = &tclStructType;
	
    return structPtr;
}


#if 0
static int Tcl_StuctObjSetArgument(Tcl_Interp *interp, Tcl_Obj *structPtr, int c, Tcl_Obj *objPtr)
{
	StructRep *structRepPtr;
	Tcl_Obj **elemPtrs;
	
	if (structPtr->typePtr != &tclStructType) {
		int result = SetStructFromAny(interp, structPtr);
		if (result != TCL_OK) {
		    return result;
		}
	}
    
	structRepPtr = (StructRep *) structPtr->internalRep.otherValuePtr;
	elemPtrs = structRepPtr->elements;
		
	Tcl_DecrRefCount(elemPtrs[c]);
	Tcl_IncrRefCount(objPtr);
	elemPtrs[c] = objPtr;
	
	return TCL_OK;
}
#endif

static int Tcl_StructObjGetElements(Tcl_Interp *interp, Tcl_Obj *structPtr,
									 int *objcPtr, Tcl_Obj ***objvPtr)
{
	StructRep *structRepPtr;
	
	if (structPtr->typePtr != &tclStructType) {
		int result = SetStructFromAny(interp, structPtr);
		if (result != TCL_OK) {
			return result;
		}
	}
	structRepPtr = (StructRep *) structPtr->internalRep.otherValuePtr;
	
	*objcPtr = structRepPtr->elemCount;
	*objvPtr = structRepPtr->elements;

	return TCL_OK;
}

static void FreeStructInternalRep(Tcl_Obj *structPtr)
{
	StructRep *structRepPtr = (StructRep *) structPtr->internalRep.otherValuePtr;
	Tcl_Obj **elemPtrs = structRepPtr->elements;
	Tcl_Obj *objPtr;
	int numElems = structRepPtr->elemCount;
	int i;

	for (i = 0;  i < numElems;  i++) {
		objPtr = elemPtrs[i];
		Tcl_DecrRefCount(objPtr);
	}
	ckfree((char *) elemPtrs);
	ckfree((char *) structRepPtr);
}

static void DupStructInternalRep(Tcl_Obj *srcPtr, Tcl_Obj *copyPtr)
{
	StructRep *structRepPtr = (StructRep *) srcPtr->internalRep.otherValuePtr;
	int numElems = structRepPtr->elemCount;
	Tcl_Obj **srcElemPtrs = structRepPtr->elements;
	Tcl_Obj **copyElemPtrs;
	StructRep *copyStructRepPtr;
	int i;

	copyElemPtrs = (Tcl_Obj **)
		ckalloc((unsigned) numElems * sizeof(Tcl_Obj *));
	for (i = 0;  i < numElems;  i++) {
		copyElemPtrs[i] = srcElemPtrs[i];
		Tcl_IncrRefCount(copyElemPtrs[i]);
	}

	copyStructRepPtr = (StructRep *) ckalloc(sizeof(StructRep));
	copyStructRepPtr->elemCount    = numElems;
	copyStructRepPtr->elements     = copyElemPtrs;

	copyPtr->internalRep.otherValuePtr = (VOID *) copyStructRepPtr;
	copyPtr->typePtr = &tclStructType;
}

static void UpdateStringOfStruct(Tcl_Obj *structPtr)
{
	structPtr->bytes = "structure";
	structPtr->length = 9;
}

static int SetStructFromAny(Tcl_Interp *interp, Tcl_Obj *structPtr)
{
	return TCL_ERROR;
}

static Tcl_Obj *Tcl_NewVarObj(const char *name)
{
	Tcl_Obj *varPtr;
	int length;
	char *varName;
	
	varPtr = Tcl_NewObj();
	
	length = strlen(name)+1;
	varName = ckalloc(length);
	memcpy(varName, name, length);
	
	varPtr->typePtr = &tclVarType;
	varPtr->internalRep.otherValuePtr = varName;
	
	return varPtr;
}

static void FreeVarInternalRep(Tcl_Obj *varPtr)
{
	char *varName = (char *) varPtr->internalRep.otherValuePtr;
	ckfree((char *) varName);
}

static void DupVarInternalRep(Tcl_Obj *srcPtr, Tcl_Obj *copyPtr)
{
	int length;
	
	length = strlen(srcPtr->internalRep.otherValuePtr)+1;
	
	copyPtr->typePtr = &tclVarType;
	copyPtr->internalRep.otherValuePtr = ckalloc(length);
	memcpy(copyPtr->internalRep.otherValuePtr, srcPtr->internalRep.otherValuePtr, length);
}

static void UpdateStringOfVar(Tcl_Obj *structPtr)
{
	structPtr->bytes = "variable";
	structPtr->length = 9;
}

static int SetVarFromAny(Tcl_Interp *interp, Tcl_Obj *structPtr)
{
	return TCL_ERROR;
}


static AP_Obj TclToPrologObj0(Tcl_Interp *interp, Tcl_Obj *tcl_obj, AP_World *w, AP_Obj *vars)
{
	AP_Obj prolog_obj;
	
	
	if        (tcl_obj->typePtr == tcl_integer_type) {
		prolog_obj = AP_NewNumberFromLong(w, tcl_obj->internalRep.intValue);
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
	} else if (tcl_obj->typePtr == &tclStructType) {
		int i, argc;
		Tcl_Obj **argv;
		
		Tcl_StructObjGetElements(interp, tcl_obj, &argc, &argv);
		
		prolog_obj = AP_NewStructure(w,
			AP_NewSymbolFromStr(w, Tcl_GetStringFromObj(argv[0], NULL)), argc-1);
		
		for (i = 1; i < argc; i++) {
			AP_Unify(w, AP_GetArgument(w, prolog_obj, i), TclToPrologObj0(interp, argv[i], w, vars)); 
		}
	} else if (tcl_obj->typePtr == &tclVarType) {
		char *name = tcl_obj->internalRep.otherValuePtr;
		int length = strlen(name);
		if (length == 0 || !vars) prolog_obj = AP_UNBOUND_OBJ;
		else {
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
			
			prolog_obj = AP_GetArgument(w, pair, 2);
		}
	} else {
		if ((Tcl_ConvertToType(NULL, tcl_obj, tcl_integer_type) == TCL_OK) 
			|| (Tcl_ConvertToType(NULL, tcl_obj, tcl_double_type) == TCL_OK)) {
			prolog_obj = TclToPrologObj0(interp, tcl_obj, w, vars);
		} else {
			prolog_obj = AP_NewUIAFromStr(w, Tcl_GetStringFromObj(tcl_obj, NULL));
		}
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
		tcl_obj = Tcl_NewStringObj((char *)AP_GetAtomStr(w, prolog_obj), -1);
		break;
	case AP_LIST:
		tcl_obj = Tcl_NewListObj(0, NULL);
		for (i = prolog_obj; !AP_IsNullList(w, i); i = AP_ListTail(w, i)) {
			Tcl_ListObjAppendElement(interp, tcl_obj, PrologToTclObj(w, AP_ListHead(w, i), interp));
		}
		break;
	case AP_STRUCTURE:
		tcl_obj = Tcl_NewStructObjFromAP_Obj(interp, w, prolog_obj);
		break;
	case AP_VARIABLE:
		tcl_obj = Tcl_NewVarObj("");
		break;
	}
	
	return tcl_obj;
}

/* Command Procedures for Tcl to Prolog Interface */

static int
Tcl_ALS_PrologObjCmd(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj **objv)
{
	AP_World *w = prolog_world;
  	const char *s;
  	AP_Obj call, results, j;
  	Tcl_Obj *error;
  	int i;

	if (objc < 2) {
		error = Tcl_NewStringObj("wrong # args: should be \"", -1);
		Tcl_StringObjAppendObj(error, objv[0]);
		Tcl_StringObjAppend(error, " prologTerm ?varName varName ...?\"", -1);
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
	}
	
	s = Tcl_GetStringFromObj(objv[1], NULL);
	
	call = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "read_eval_results"), 2,
					     AP_NewUIAFromStr(w, s), AP_UNBOUND_OBJ);
				
			
	switch (AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &call)) {
	case AP_SUCCESS:
		results = AP_GetArgument(w, call, 2);
		
		for (i = 2, j = results; !AP_IsNullList(w, j) && i < objc; i++, j = AP_ListTail(w, j)) {
			Tcl_ObjSetVar2(interp, objv[i], NULL, PrologToTclObj(w, AP_ListHead(w, j), interp), 0);
		}
		
		Tcl_SetObjResult(interp, Tcl_NewIntObj(1));
		return TCL_OK;
		break;
	case AP_FAIL:
		Tcl_SetObjResult(interp, Tcl_NewIntObj(0));
		return TCL_OK;
		break;
	case AP_EXCEPTION:
	default:
		error = Tcl_NewStringObj("prolog exception: ", -1);
		Tcl_StringObjAppendObj(error, PrologToTclObj(w, AP_GetException(w), interp));
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
		break;
	}
}

static int
Tcl_ALS_Prolog_CallObjCmd(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj **objv)
{
	AP_World *w = prolog_world;
  	const char *name;
  	AP_Obj call, i, vars, value, pair, wrap_call;
  	Tcl_Obj *error;

	if (objc < 2) {
		error = Tcl_NewStringObj("wrong # args: should be \"", -1);
		Tcl_StringObjAppendObj(error, objv[0]);
		Tcl_StringObjAppend(error, " prologTerm\"", -1);
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
	}
	
	call = TclToPrologObj(interp, objv[1], w, &vars);
	
	/* Wrap up call and variable list, so it will be updated in
	   the event of a gc. */
	wrap_call = AP_NewInitStructure(w,
		       AP_NewSymbolFromStr(w, "eval_results"), 2,
		       call, vars);

	switch (AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &wrap_call)) {
	case AP_SUCCESS:
	        /* Get the new value of vars */
	        vars = AP_GetArgument(w, wrap_call, 2);

		for (i = vars; !AP_IsNullList(w, i); i = AP_ListTail(w, i)) {
			pair = AP_ListHead(w, i);
			name = AP_GetAtomStr(w, AP_GetArgument(w, pair, 1));
			if (*name) {
				value = AP_GetArgument(w, pair, 2);
				Tcl_ObjSetVar2(interp, Tcl_NewStringObj((char *)name, -1), NULL, PrologToTclObj(w, value, interp), 0);
			}
		}
		
		Tcl_SetObjResult(interp, Tcl_NewIntObj(1));
		return TCL_OK;
		break;
	case AP_FAIL:
		Tcl_SetObjResult(interp, Tcl_NewIntObj(0));
		return TCL_OK;
		break;
	case AP_EXCEPTION:
	default:
		error = Tcl_NewStringObj("prolog exception: ", -1);
		Tcl_StringObjAppendObj(error, PrologToTclObj(w, AP_GetException(w), interp));
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
		break;
	}
}

static int
Tcl_StructObjCmd(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj **objv)
{
  	Tcl_Obj *error;

	if (objc < 3) {
		error = Tcl_NewStringObj("wrong # args: should be \"", -1);
		Tcl_StringObjAppendObj(error, objv[0]);
		Tcl_StringObjAppend(error, " functor argValue ?argValue argValue ...?\"", -1);
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
	}
	
	
	Tcl_SetObjResult(interp, Tcl_NewStructObj(objc-1, objv+1));
	
	return TCL_OK;
}

static int
Tcl_VarObjCmd(ClientData prolog_world, Tcl_Interp *interp, int objc, Tcl_Obj **objv)
{
  	Tcl_Obj *error;

	if (objc != 2) {
		error = Tcl_NewStringObj("wrong # args: should be \"", -1);
		Tcl_StringObjAppendObj(error, objv[0]);
		Tcl_StringObjAppend(error, " varName\"", -1);
		Tcl_SetObjResult(interp, error);
		return TCL_ERROR;
	}
	
	
	Tcl_SetObjResult(interp, Tcl_NewVarObj(Tcl_GetStringFromObj(objv[1], NULL)));
	
	return TCL_OK;
}


/* Predicates for Prolog to Tcl Interface */

static Tcl_HashTable tcl_interp_name_table;

static int interp_count = 0;

static AP_Result tcl_new(AP_World *w, AP_Obj interp_name)
{
	Tcl_Interp *interp;
	char name[128];
	const char *namep;
	Tcl_HashEntry *entry;
	int is_new, pre_named;
	AP_Type type;
#ifndef macintosh
	int r;
#endif
	type = AP_ObjType(w, interp_name);
	
	if (type != AP_VARIABLE && type != AP_ATOM) { 
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom_or_variable"), interp_name);
		goto error;
	}
	
	pre_named = (type == AP_ATOM);
	
	interp = Tcl_CreateInterp();
	if (!interp) {
		AP_SetStandardError(w, AP_RESOURCE_ERROR, AP_NewSymbolFromStr(w, "tcl_memory"));
		goto error;
	}

#ifndef macintosh
	r = Tk_Init(interp);
	if (r != TCL_OK) {
		printf("%s\n", interp->result);
		/* return AP_FAIL; */
	}

    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);	
#endif
	
	if (pre_named) {
		namep = AP_GetAtomStr(w, interp_name);
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
					AP_NewSymbolFromStr(w, "tcl_interpreter"), interp_name);
		goto error_delete;
	} 			
	
	Tcl_SetHashValue(entry, interp);

	if (!Tcl_CreateObjCommand(interp, "prolog", -1, Tcl_ALS_PrologObjCmd, w, NULL)) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "tcl_create_command_error"));
		goto error_delete;
	}
	if (!Tcl_CreateObjCommand(interp, "prolog_call", -1, Tcl_ALS_Prolog_CallObjCmd, w, NULL)) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "tcl_create_command_error"));
		goto error_delete;
	}
	if (!Tcl_CreateObjCommand(interp, "struct", -1, Tcl_StructObjCmd, w, NULL)) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "tcl_create_command_error"));
		goto error_delete;
	}
	if (!Tcl_CreateObjCommand(interp, "var", -1, Tcl_VarObjCmd, w, NULL)) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "tcl_create_command_error"));
		goto error_delete;
	}


	return (pre_named) ? AP_SUCCESS : AP_Unify(w, interp_name, AP_NewUIAFromStr(w, namep));
	
error_delete:
	Tcl_DeleteInterp(interp);
error:
	return AP_EXCEPTION;
}




static Tcl_Interp *GetInterp(AP_World *w, AP_Obj interp_name)
{
	Tcl_HashEntry *entry;
	
	entry = Tcl_FindHashEntry(&tcl_interp_name_table, AP_GetAtomStr(w, interp_name));
	
	return entry ? Tcl_GetHashValue(entry) : NULL;
}

#include "alspi.h"

static AP_Result tcl_eval(AP_World *w, AP_Obj interp_name, AP_Obj command, AP_Obj result)
{
	Tcl_Interp *interp;
	Tcl_Obj *tcl_command;
	AP_Obj pro_result;
	int r;
	
	if (AP_ObjType(w, interp_name) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), interp_name);
	}

	interp = GetInterp(w, interp_name);
	if (!interp) {
		return AP_SetStandardError(w, AP_DOMAIN_ERROR,
					AP_NewSymbolFromStr(w, "tcl_interpreter"), interp_name);
	}
	
	tcl_command = PrologToTclObj(w, command, interp);
	if (!tcl_command) {
		return AP_SetStandardError(w, AP_RESOURCE_ERROR, AP_NewSymbolFromStr(w, "tcl_memory"));
	}
	
	r = Tcl_EvalObj(interp, tcl_command);
	pro_result = TclToPrologObj(interp, Tcl_GetObjResult(interp), w, NULL);
	/* error check */
	
	/* Hack to refresh result */
	PI_getan(&result.p, &result.t, 3);

	if (r == TCL_OK) {	
		return AP_Unify(w, pro_result, result);
	} else {
		return AP_SetException(w,
			AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "error"), 2,
				AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "tcl_error"), 1, pro_result),
				AP_UNBOUND_OBJ));
	}
}

static AP_Result tcl_delete(AP_World *w, AP_Obj interp_name)
{
	Tcl_HashEntry *entry;
	Tcl_Interp *interp;

	if (AP_ObjType(w, interp_name) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), interp_name);
	}
	
	interp = GetInterp(w, interp_name);
	
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

static AP_Result tcl_delete_all(AP_World *w)
{
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

static AP_Result tk_main_loop(AP_World *w)
{
	Tk_MainLoop();
	return AP_SUCCESS;
}




static int glue_tcl_new(void)
{
	return AP_OldToNewCall(tcl_new, 1);
}

static int glue_tcl_eval(void)
{
	return AP_OldToNewCall(tcl_eval, 3);
}

static int glue_tcl_delete(void)
{
	return AP_OldToNewCall(tcl_delete, 1);
}

static int glue_tcl_delete_all(void)
{
	return AP_OldToNewCall(tcl_delete_all, 0);
}

static int glue_tk_main_loop(void)
{
	return AP_OldToNewCall(tk_main_loop, 0);
}


PI_BEGIN
  PI_DEFINE("tcl_new",1,glue_tcl_new)
  PI_DEFINE("tcl_eval",3,glue_tcl_eval)
  PI_DEFINE("tcl_delete",1,glue_tcl_delete)
  PI_DEFINE("tcl_delete_all",0,glue_tcl_delete_all)
  PI_DEFINE("tk_main_loop",0,glue_tk_main_loop)
PI_END


void pi_init(void);
void pi_init(void)
{
	Tcl_InitHashTable(&tcl_interp_name_table, TCL_STRING_KEYS);
	
	/* Get pointers to the standard Tcl types. */
	tcl_integer_type = Tcl_GetObjType("int");
	tcl_double_type = Tcl_GetObjType("double");
	tcl_list_type = Tcl_GetObjType("list");
	
	/* Register prolog related Tcl types. */
	
	Tcl_RegisterObjType(&tclStructType);
	Tcl_RegisterObjType(&tclVarType);
		
    PI_INIT;
}
