#include "alspi.h"

#include "Python.h"

static int py_call(void)
{
    PWord arg1, arg2, val;
    int arg1_type, arg2_type, val_type;
    char *command;

	PyObject *m, *d, *r;

    PI_getan(&arg1, &arg1_type, 1);
    PI_getan(&arg2, &arg2_type, 2);

	switch(arg1_type) { 
		case PI_SYM: 
			command = PI_getsymname(NULL,arg1,0); 
			break; 
		case PI_UIA: 
			command = PI_getuianame(NULL,arg1,0); 
			break; 
		default: 
			PI_FAIL; 
			break; 
	} 
	
	m = PyImport_AddModule("__main__");
	if (m == NULL) {
		PI_FAIL;
	}
	d = PyModule_GetDict(m);
	r = PyRun_String(command, Py_single_input, d, d);
	if (r == NULL) {
		PyErr_Print();
		PyErr_Clear();
		PI_FAIL;
	}
	
		d = PyObject_Type(r);
		printf("%s\n", ((PyTypeObject *)d)->tp_name);
	if (PyInt_Check(r)) {
		val = PyInt_AsLong(r); val_type = PI_INT;
	} else if (PyLong_Check(r)) {
		val = PyLong_AsLong(r); val_type = PI_INT;	
	} else if (PyFloat_Check(r)) {
		double num;
		num = PyFloat_AsDouble(r);
		PI_makedouble(&val, &val_type, num);
	} else if (PyString_Check(r)) {
		PI_makeuia(&val, &val_type, PyString_AsString(r));
	} else {
		printf("bad type\n");
		PI_FAIL;
	}
	
	return PI_unify(val, val_type, arg2, arg2_type);

}

PI_BEGIN
    PI_DEFINE("py_call",2,py_call)
PI_END

void pi_init(void)
{
    PI_INIT;

	Py_Initialize();
}
