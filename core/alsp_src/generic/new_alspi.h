/* The New Interface */

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {AP_FAIL, AP_SUCCESS, AP_EXCEPTION} AP_Result;
typedef void AP_World;
typedef struct {long p; int t;} AP_Obj;
typedef enum {AP_VARIABLE, AP_INTEGER, AP_FLOAT, AP_ATOM, AP_LIST, AP_STRUCTURE} AP_Type;

typedef enum {AP_INSTANTIATION_ERROR, AP_TYPE_ERROR, AP_DOMAIN_ERROR, AP_EXISTENCE_ERROR,
	AP_PERMISSION_ERROR, AP_REPRESENTATION_ERROR, AP_EVALUATION_ERROR, AP_RESOURCE_ERROR,
	AP_SYNTAX_ERROR, AP_SYSTEM_ERROR} AP_StandardError;

#define AP_API(x) x

AP_API(AP_Result) AP_Unify(AP_World *w, AP_Obj a, AP_Obj b);
AP_API(AP_Obj) AP_NewUIAFromStr(AP_World *w, const char *s);
AP_API(AP_Obj) AP_NewNumberFromLong(AP_World *w, long n);
AP_API(AP_Obj) AP_NewFloatFromDouble(AP_World *w, double d);
AP_API(AP_Obj) AP_NullList(AP_World *w);
AP_API(AP_Obj) AP_NewList(AP_World *w);
AP_API(AP_Obj) AP_NewInitList(AP_World *w, AP_Obj head, AP_Obj tail);
AP_API(AP_Type) AP_ObjType(AP_World *w, AP_Obj obj);
AP_API(long) AP_GetLong(AP_World *w, AP_Obj obj);
AP_API(double) AP_GetDouble(AP_World *w, AP_Obj obj);
AP_API(const char *) AP_GetAtomStr(AP_World *w, AP_Obj obj);
AP_API(int) AP_IsNullList(AP_World *w, AP_Obj obj);
AP_API(AP_Obj) AP_ListHead(AP_World *w,  AP_Obj list);
AP_API(AP_Obj) AP_ListTail(AP_World *w,  AP_Obj list);
AP_API(int) AP_IsAtom(AP_World *w, AP_Obj obj);
AP_API(AP_Obj) AP_NewInitStructure(AP_World *w, AP_Obj functor, int arity, ...);
AP_API(AP_Obj) AP_NewStructure(AP_World *w, AP_Obj functor, int arity);
AP_API(AP_Result) AP_Call(AP_World *w, AP_Obj module, AP_Obj *term);
AP_API(AP_Obj) AP_NewSymbolFromStr(AP_World *w, const char *s);
AP_API(AP_Obj) AP_GetArgument(AP_World *w, AP_Obj structure, int index);
extern AP_Obj AP_UNBOUND_OBJ;
AP_API(AP_Result) AP_SetException(AP_World *w, AP_Obj ball);
AP_API(AP_Obj) AP_GetException(AP_World *w);
AP_API(AP_Result) AP_SetError(AP_World *w, AP_Obj error_term);
AP_API(AP_Result) AP_SetStandardError(AP_World *w, AP_StandardError error_type, ...);
AP_API(int) AP_OldToNewCall(AP_Result (*new_func)(), int arity);
AP_API(int) AP_GetStructureArity(AP_World *w, AP_Obj struc);
AP_API(AP_Obj) AP_GetStructureFunctor(AP_World *w, AP_Obj struc);

#ifdef __cplusplus
}
#endif
