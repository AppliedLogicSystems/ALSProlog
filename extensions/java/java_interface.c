#include "alspi.h"
#include "new_alspi.h"

#include <string.h>
#include <jni.h>

#pragma mark GLOBALS

static JavaVM *jvm = NULL;
static JNIEnv *env = NULL;
static jclass object_class = NULL;
static jclass integer_class = NULL;
static jclass double_class = NULL;
static jclass string_class = NULL;
static jmethodID integer_init = NULL;
static jmethodID double_init = NULL;
static jmethodID integer_intValue = NULL;
static jmethodID double_doubleValue = NULL;

// add common global classes and methods for quick lookup.

#ifdef _WIN32
#define PATH_SEPARATOR ';'
#else /* UNIX */
#define PATH_SEPARATOR ':'
#endif

#pragma mark -
#pragma mark PROLOG/JAVA OBJECT CONVERSION

static jobject PrologToJObject(AP_World *w, AP_Obj o, JNIEnv *env)
{
	jobject jo, t;
	AP_Obj l;
	int length,i;
	
	switch (AP_ObjType(w, o)) {
	case AP_INTEGER:
		jo = (*env)->NewObject(env, integer_class, integer_init, AP_GetLong(w, o));
		break;
	case AP_FLOAT:
		jo = (*env)->NewObject(env, double_class, double_init, AP_GetDouble(w, o));
		break;
	case AP_ATOM:
		jo = (*env)->NewStringUTF(env, AP_GetAtomStr(w, o));
		break;
	case AP_LIST:
		for (l = o, length = 0; !AP_IsNullList(w,l); l = AP_ListTail(w,l)) length++;
		t = (*env)->NewStringUTF(env, "");
		if (t) jo = (*env)->NewObjectArray(env, length, object_class, t);
		if (jo) {
			for (l = o, i = 0; !AP_IsNullList(w,l); l = AP_ListTail(w,l),i++) {
				t = PrologToJObject(w, AP_ListHead(w, l), env);
				if (!t) { jo = NULL; break; }
				(*env)->SetObjectArrayElement(env, jo, i, t);
				if ((*env)->ExceptionCheck(env)) { jo = NULL; break; } 
			}
		}
		break;
	}
	
	return jo;
}

static AP_Obj jobjectArrayToProlog(AP_World *w, JNIEnv *env, jobjectArray joa);

static AP_Obj JObjectToProlog(JNIEnv *env, jobject jo, AP_World *w)
{
	if (jo == NULL) {
		AP_SetError(w, AP_NewSymbolFromStr(w, "null_java_object"));
		return AP_UNBOUND_OBJ;
	} else if ((*env)->IsInstanceOf(env, jo, integer_class)) {
		jint i;
		i = (*env)->CallIntMethod(env, jo, integer_intValue);
		return AP_NewNumberFromLong(w, i);
	} else if ((*env)->IsInstanceOf(env, jo, double_class)) {
		jdouble d;
		d = (*env)->CallDoubleMethod(env, jo, double_doubleValue);
		return AP_NewFloatFromDouble(w, d);
	} else if ((*env)->IsInstanceOf(env, jo, string_class)) {
	    jboolean jfalse = JNI_FALSE;
		return AP_NewUIAFromStr(w, (*env)->GetStringUTFChars(env, jo, &jfalse));
	} else if ((*env)->IsInstanceOf(env, jo, object_class)) {
		return jobjectArrayToProlog(w, env, jo);
	} else {
		// add more detail to this error - name of object class, etc
		AP_SetError(w, AP_NewSymbolFromStr(w, "unknown_java_object"));
		return AP_UNBOUND_OBJ;
	}
}

static AP_Obj jobjectArrayToProlog(AP_World *w, JNIEnv *env, jobjectArray joa)
{
	jsize i,l = (*env)->GetArrayLength(env, joa); 
	AP_Obj list = AP_NullList(w);

	for (i = l-1; i >= 0; i--) {
		AP_Obj o = JObjectToProlog(env, (*env)->GetObjectArrayElement(env, joa, i), w);
		if (AP_ObjType(w, o) == AP_VARIABLE) return o;
		list = AP_NewInitList(w, o, list);
	}
	
	return list;
}

static jboolean PrologListToJArgs(AP_World *w, AP_Obj l, JNIEnv *env, jvalue *args)
{
	AP_Obj i;
	
	for (; !AP_IsNullList(w, l); l = AP_ListTail(w, l), args++) {
		i = AP_ListHead(w, l);
		
		switch (AP_ObjType(w, i)) {
		case AP_INTEGER:
			args->i = AP_GetLong(w, i);
			break;
		case AP_FLOAT:
			args->d = AP_GetDouble(w, i);
			break;
		case AP_ATOM:
		case AP_LIST:
			args->l = PrologToJObject(w, i, env);
			if (!args->l) return JNI_FALSE;
			break;
		default:
			return JNI_FALSE;
			break;
		}
	}
	
	return JNI_TRUE;
}
 
#pragma mark -
#pragma mark JAVA METHOD CALL ROUTINES

static AP_Obj JavaErrSymbol(AP_World *w, jint code)
{
	const char *message;
	
	switch (code) {
	case JNI_OK: 		message = "success"; 			break;
	case JNI_EDETACHED:	message = "thread_detached";	break;
	case JNI_EVERSION:	message = "version_error";		break;
	case JNI_ENOMEM:	message = "out_of_memory";		break;
	case JNI_EEXIST:	message = "vm_exists";			break;
	case JNI_EINVAL:	message = "invalid_arguments";	break;
	case JNI_ERR:
	default: 			message = "unknown_error";		break;
	}
	
	return AP_NewSymbolFromStr(w, message);
}

static AP_Result JavaResultToProlog(AP_World *w, const char *type, jint code)
{
	if (code == JNI_OK) return AP_SUCCESS;
	else {
		return AP_SetError(w,
			AP_NewInitStructure(w, AP_NewSymbolFromStr(w, type), 2,
								   JavaErrSymbol(w, code), AP_NewNumberFromLong(w, code)));	
	}
}

static AP_Result JavaToPrologException(JNIEnv *env, jthrowable exception, AP_World *w)
{
	jclass throwable;
	jmethodID toString;
	jstring message;
    jboolean jfalse = JNI_FALSE;
	
	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
	
	throwable = (*env)->GetObjectClass(env, exception);
    if (throwable == 0) {
		return AP_SetError(w, AP_NewSymbolFromStr(w, "exception_get_class_error"));
    }
	toString = (*env)->GetMethodID(env, throwable, "toString", "()Ljava/lang/String;");
    if (toString == 0) {
		return AP_SetError(w, AP_NewSymbolFromStr(w, "exception_get_method_error"));
    }
    message = (*env)->CallObjectMethod(env, exception, toString);
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionDescribe(env);
		(*env)->ExceptionClear(env);
		
		return AP_SetError(w, AP_NewSymbolFromStr(w, "exception_toString_error"));
	}
	
	return AP_SetError(w,
		AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "java_exception"), 1,
			AP_NewUIAFromStr(w, (*env)->GetStringUTFChars(env, message, &jfalse))));
}

static AP_Result java_call(AP_World *w, AP_Obj clazz, AP_Obj method, AP_Obj sig, AP_Obj args, AP_Obj result)
{
    jclass cls;
    jmethodID mid;
    const char *sig_s, *return_sig;
    jvalue jargs[100];
    jthrowable exception;
    jint ji; jdouble jd; jstring js; jobjectArray joa;
    AP_Obj presult;
    jboolean r, jfalse = JNI_FALSE;

	if (env == NULL) return AP_SetError(w, AP_NewSymbolFromStr(w, "vm_not_create"));

	if (AP_ObjType(w, clazz) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), clazz);
	}

	if (AP_ObjType(w, method) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), method);
	}
	
	if (AP_ObjType(w, sig) != AP_ATOM) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), sig);
	}
	
	if (AP_ObjType(w, args) != AP_LIST && !AP_IsNullList(w, args)) {
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "list"), args);
	}

    cls = (*env)->FindClass(env, AP_GetAtomStr(w, clazz));
    if (cls == 0) {
		return AP_SetError(w, AP_NewSymbolFromStr(w, "find_class_error"));
    }

	sig_s = AP_GetAtomStr(w, sig);
    mid = (*env)->GetStaticMethodID(env, cls, AP_GetAtomStr(w, method), sig_s);
    if (mid == 0) {
		return AP_SetError(w, AP_NewSymbolFromStr(w, "get_method_error"));
    }


	r = PrologListToJArgs(w, args, env, jargs);
	if (r == JNI_FALSE) return JavaToPrologException(env, (*env)->ExceptionOccurred(env), w);
	
	return_sig = strchr(sig_s, ')');
	
	if (!return_sig) {
		return AP_SetError(w, AP_NewSymbolFromStr(w, "invalid_signature_error"));
	}
	
	// Advance to return signature.
	return_sig++;
		
    switch (*return_sig) {
    case 'V':
    	(*env)->CallStaticVoidMethodA(env, cls, mid, jargs);
    	exception = (*env)->ExceptionOccurred(env);
    	if (exception) return JavaToPrologException(env, exception, w);
    	else return AP_SUCCESS;
    	break;
    case 'I':
    	ji = (*env)->CallStaticIntMethodA(env, cls, mid, jargs);
    	exception = (*env)->ExceptionOccurred(env);
    	if (exception) return JavaToPrologException(env, exception, w);
 		presult = AP_NewNumberFromLong(w, ji);
    	break;
    case 'D':
    	jd = (*env)->CallStaticDoubleMethodA(env, cls, mid, jargs);
    	exception = (*env)->ExceptionOccurred(env);
    	if (exception) return JavaToPrologException(env, exception, w);
    	presult = AP_NewFloatFromDouble(w, jd);
    	break;
    default:
    	if (!strcmp(return_sig, "Ljava/lang/String;")) {
    		js = (*env)->CallStaticObjectMethodA(env, cls, mid, jargs);
    		exception = (*env)->ExceptionOccurred(env);
	    	if (exception) return JavaToPrologException(env, exception, w);
			presult = AP_NewUIAFromStr(w, (*env)->GetStringUTFChars(env, js, &jfalse));
		} else if (!strcmp(return_sig, "[Ljava/lang/Object;")) {
			joa = (*env)->CallStaticObjectMethodA(env, cls, mid, jargs);
	    	exception = (*env)->ExceptionOccurred(env);
	    	if (exception) return JavaToPrologException(env, exception, w);
			presult = jobjectArrayToProlog(w, env, joa);
			if (AP_ObjType(w,presult) == AP_VARIABLE) return AP_EXCEPTION;
		} else {
			return AP_SetError(w, AP_NewSymbolFromStr(w, "invalid_signature_error"));
		}
		break;
    }

	return AP_Unify(w, result, presult);
}

#pragma mark -
#pragma mark JAVA VIRTUAL MACHINE CREATION/DESTRUCTION

static AP_Result java_new(AP_World *w, AP_Obj path)
{
	JavaVMInitArgs vm_args;
	JavaVMOption options[1];
    jint result;
    char classpath[1024];

	if (AP_ObjType(w, path) != AP_ATOM) {
		AP_SetStandardError(w, AP_TYPE_ERROR,
				AP_NewSymbolFromStr(w, "atom"), path);
		return AP_EXCEPTION;
	}
	
	vm_args.version = JNI_VERSION_1_2;
	sprintf(classpath, "-Djava.class.path=%s", AP_GetAtomStr(w, path));
	options[0].optionString = classpath;
	vm_args.options = options;
	vm_args.nOptions = 1;
	vm_args.ignoreUnrecognized = JNI_TRUE;

    result = JNI_CreateJavaVM(&jvm, (void **)&env, &vm_args);

// also gather common types and methods
	object_class = (*env)->FindClass(env, "java/lang/Object");
	integer_class = (*env)->FindClass(env, "java/lang/Integer");
	double_class = (*env)->FindClass(env, "java/lang/Double");
	string_class = (*env)->FindClass(env, "java/lang/String");
	// error check

	object_class = (*env)->NewGlobalRef(env, object_class);
	integer_class = (*env)->NewGlobalRef(env, integer_class);
	double_class = (*env)->NewGlobalRef(env, double_class);
	string_class = (*env)->NewGlobalRef(env, string_class);
	// error check

	integer_init = (*env)->GetMethodID(env, integer_class, "<init>", "(I)V");
	double_init = (*env)->GetMethodID(env, double_class, "<init>", "(D)V");

	integer_intValue = (*env)->GetMethodID(env, integer_class, "intValue", "()I");;
	double_doubleValue = (*env)->GetMethodID(env, double_class, "doubleValue", "()D");

    return JavaResultToProlog(w, "create_java_vm_error", result);
}

static AP_Result java_free(AP_World *w)
{
	jint result;
	
	if (jvm == NULL) return AP_SetError(w, AP_NewSymbolFromStr(w, "vm_not_create"));
	
	result = (*jvm)->DestroyJavaVM(jvm);
	jvm = NULL;
	env = NULL;

	return JavaResultToProlog(w, "destroy_java_vm_error", result);
}

#pragma mark -
#pragma mark PROLOG INTERFACE GLUE

static int glue_java_new(void) {return AP_OldToNewCall(java_new, 1);}
static int glue_java_free(void) {return AP_OldToNewCall(java_free, 0);}
static int glue_java_call(void) {return AP_OldToNewCall(java_call, 5);}

PI_BEGIN
	PI_MODULE("java")

	PI_DEFINE("java_new",1,glue_java_new)
	PI_DEFINE("java_free",0,glue_java_free)

	PI_DEFINE("java_call",5,glue_java_call)
PI_END

void pi_init(void)
{
    PI_INIT;
}
