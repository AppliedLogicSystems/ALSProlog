/*

ALS Prolog to Python Interface

See als_python_guide.html for documentation.

5/3 1
5/4 8-9 3-7 10-12
5/5 12-130
5/5 7-11 1-6

5/16 2-7
*/

#include "Python.h"

#include "alspi.h"
#include "new_alspi.h"

static AP_Obj PythonToPrologObj(PyObject *py, AP_World *w)
{
	AP_Obj pro;
	
	if (PyString_Check(py)) {
		pro = AP_NewUIAFromStr(w, PyString_AsString(py));

	} else if (PyTuple_Check(py)) {
		int i, length = PyTuple_Size(py);
		
		if (length == 0) {
			pro = AP_NewSymbolFromStr(w, "()");
		} else {
			for (i = length-1, pro = PythonToPrologObj(PyTuple_GetItem(py,length-1),w) ;
				 i; i--) {
				pro = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, ","), 2,
						PythonToPrologObj(PyTuple_GetItem(py, i-1), w), pro);
			}
		}

	} else if (PyList_Check(py)) {
		int i, length = PyList_Size(py);		
		for (i = length, pro = AP_NullList(w); i; i--) {
			pro = AP_NewInitList(w, PythonToPrologObj(PyList_GetItem(py, i-1), w), pro);
		}

	} else if (PyInt_Check(py)) {
		pro = AP_NewNumberFromLong(w, PyInt_AsLong(py));

	} else if (PyLong_Check(py)) {
		pro = AP_NewNumberFromLong(w, PyLong_AsLong(py));

	} else if (PyFloat_Check(py)) {
		pro = AP_NewFloatFromDouble(w, PyFloat_AsDouble(py));

	} else {
		pro = AP_UNBOUND_OBJ;
	}
	
	return pro;
}

static int IsNullTuple(AP_World *w, AP_Obj pro)
{
	return AP_Unify(w, pro, AP_NewSymbolFromStr(w, "()")) == AP_SUCCESS;
}

static PyObject *PrologToPythonObj(AP_World *w, AP_Obj pro)
{
	PyObject *py;
	AP_Obj pi;
	
	switch (AP_ObjType(w, pro)) {

	case AP_INTEGER:
		py = PyLong_FromLong(AP_GetLong(w, pro));
		break;	

	case AP_FLOAT:
		py = PyFloat_FromDouble(AP_GetDouble(w, pro));
		break;	

	case AP_ATOM:
		if (AP_IsNullList(w, pro)) {
			py = PyList_New(0);
		} else if (IsNullTuple(w, pro)) {
			py = PyTuple_New(0);		
		} else {
			py = PyString_FromString(AP_GetAtomStr(w, pro));
		}
		break;

	case AP_LIST:
		py = PyList_New(0);
		for (pi = pro; !AP_IsNullList(w, pi); pi = AP_ListTail(w, pi)) {
			PyList_Append(py, PrologToPythonObj(w, AP_ListHead(w, pi)));
		}
		break;

	case AP_STRUCTURE: {
		AP_Obj comma = AP_NewSymbolFromStr(w, ",");
		if (AP_Unify(w, comma, AP_GetStructureFunctor(w, pro)) == AP_SUCCESS) {
			int i, length;
			
			for (pi = pro, length = 0 ;
				 (AP_Unify(w, comma, AP_GetStructureFunctor(w, pro)) == AP_SUCCESS) ;
				 pi = AP_GetArgument(w, pi, 2)) {
				length++;
			}
				
			py = PyTuple_New(length);
			for (pi = pro, i = 0; i < length; i++, pi = AP_GetArgument(w, pi, 2)) {
				PyTuple_SetItem(py, i, PrologToPythonObj(w, AP_GetArgument(w, pi, 1)));
			}
			
			PyTuple_SetItem(py, length, PrologToPythonObj(w, AP_GetArgument(w, pi, 2)));
		} else {
			py = NULL;
		}
		}
		break;

	default:
	  py = NULL;
	}
	
	return py;
}

static AP_Result PythonToPrologException(AP_World *w)
{
	PyObject *type, *value, *traceback;
	
	PyErr_Fetch(&type, &value, &traceback); 

	AP_SetException(w,
		AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "error"), 2,
			AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "python_error"), 3,
				PythonToPrologObj(((PyClassObject*)type)->cl_name, w),
				PythonToPrologObj(value, w),
				AP_UNBOUND_OBJ),
			AP_UNBOUND_OBJ));
				
	Py_XDECREF(type);
	Py_XDECREF(value);
	Py_XDECREF(traceback);
	
	return AP_EXCEPTION;
}

static AP_Result py_exec(AP_World *w, AP_Obj command)
{	
	PyObject *m, *d, *r;
	
	if (AP_ObjType(w, command) != AP_ATOM) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), command);
		return AP_EXCEPTION;
	}
	
	m = PyImport_AddModule("__main__");
	d = PyModule_GetDict(m);
	r = PyRun_String((char *)AP_GetAtomStr(w, command), Py_file_input, d, d);

	if (r == NULL) {
		return PythonToPrologException(w);
	}
		
	return AP_SUCCESS;
}

static AP_Result py_eval(AP_World *w, AP_Obj command, AP_Obj result)
{
	PyObject *m, *d, *r;
	
	if (AP_ObjType(w, command) != AP_ATOM) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), command);
		return AP_EXCEPTION;
	}
	
	m = PyImport_AddModule("__main__");
	d = PyModule_GetDict(m);
	r = PyRun_String((char *)AP_GetAtomStr(w, command), Py_eval_input, d, d);

	if (r == NULL) {
		return PythonToPrologException(w);
	}
	
	return AP_Unify(w, result, PythonToPrologObj(r, w));	
}

static int IsKeywordArg(AP_World *w, AP_Obj call, int i)
{
	AP_Obj item = AP_GetArgument(w, call, i);
	AP_Obj equal = AP_NewSymbolFromStr(w, "=");
	
	return AP_ObjType(w, item) == AP_STRUCTURE &&
		   AP_GetStructureArity(w, item) == 2 &&
		   AP_Unify(w, equal, AP_GetStructureFunctor(w, item)) == AP_SUCCESS;
}

static AP_Result py_call3(AP_World *w, AP_Obj module_name, AP_Obj call, AP_Obj result)
{
	PyObject *module, *func, *args, *key_args, *r, *apply, *apply_args;
	int arity, i;

	if (AP_ObjType(w, module_name) != AP_ATOM) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), module_name);
		return AP_EXCEPTION;
	}
	// is this correct?
	if (AP_ObjType(w, call) != AP_ATOM && AP_ObjType(w, call) != AP_STRUCTURE) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "compond"), call);
		return AP_EXCEPTION;
	}
	
	apply = PyObject_GetAttrString(PyImport_ImportModule("__builtin__"), "apply");
	
	module = PyImport_ImportModule((char *)AP_GetAtomStr(w, module_name));
	if (!module) {
		return PythonToPrologException(w);	
	}
	
	if (AP_ObjType(w, call) == AP_ATOM) {
		func = PyObject_GetAttrString(module, (char *)AP_GetAtomStr(w, call));
		arity = 0;
	} else {
		func = PyObject_GetAttrString(module, (char *)AP_GetAtomStr(w, AP_GetStructureFunctor(w, call)));
		arity = AP_GetStructureArity(w, call);
	}
	

	key_args = PyDict_New();
	for ( ; arity > 0 && IsKeywordArg(w, call, arity) ; arity--) {
		AP_Obj key, value;
		key = AP_GetArgument(w, AP_GetArgument(w, call, arity), 1);
		value = AP_GetArgument(w, AP_GetArgument(w, call, arity), 2);
		
		PyDict_SetItem(key_args, PrologToPythonObj(w, key), PrologToPythonObj(w, value));
	}

	args = PyTuple_New(arity);
	for (i = 0; i < arity; i++) {
		PyTuple_SetItem(args, i, PrologToPythonObj(w, AP_GetArgument(w, call, i+1)));
	}
	
	
	apply_args = PyTuple_New(3);
	PyTuple_SetItem(apply_args, 0, func);
	PyTuple_SetItem(apply_args, 1, args);
	PyTuple_SetItem(apply_args, 2, key_args);
	
	r = PyEval_CallObject(apply, apply_args);

	if (r == NULL) {
		return PythonToPrologException(w);
	}
	
	return AP_Unify(w, result, PythonToPrologObj(r, w));
}

static AP_Result py_call2(AP_World *w, AP_Obj call, AP_Obj result)
{
	AP_Obj module;
	
	module = AP_NewSymbolFromStr(w, "__main__");
	
	return py_call3(w, module, call, result);
}

static AP_Result py_call1(AP_World *w, AP_Obj call)
{
	AP_Obj module, result;
	
	module = AP_NewSymbolFromStr(w, "__main__");
	result = AP_UNBOUND_OBJ;
	
	return py_call3(w, module, call, result);
}

static int glue_py_exec(void) {return AP_OldToNewCall(py_exec, 1);}
static int glue_py_eval(void) {return AP_OldToNewCall(py_eval, 2);}
static int glue_py_call1(void) {return AP_OldToNewCall(py_call1, 1);}
static int glue_py_call2(void) {return AP_OldToNewCall(py_call2, 2);}
static int glue_py_call3(void) {return AP_OldToNewCall(py_call3, 3);}

PI_BEGIN
	PI_MODULE("python")

	PI_DEFINE("py_exec",1,glue_py_exec)
	PI_DEFINE("py_eval",2,glue_py_eval)
	PI_DEFINE("py_call",1,glue_py_call1)
	PI_DEFINE("py_call",2,glue_py_call2)
	PI_DEFINE("py_call",3,glue_py_call3)
PI_END

void pi_init(void)
{
    PI_INIT;

	Py_Initialize();
}
