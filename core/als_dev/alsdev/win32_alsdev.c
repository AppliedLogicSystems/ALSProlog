#include <alspi.h>
#include <new_alspi.h>
#include <crtl.h>

extern void panic(const char *);

static char *simple_write(AP_World *w, AP_Obj obj, char *s)
{
    switch (AP_ObjType(w, obj)) {
    case AP_VARIABLE:
    	s += sprintf(s, "V%lx", obj.p);
	break;
    case AP_LIST:
	{
		AP_Obj p = obj;

		s += sprintf(s, "[");
		
		while (AP_ObjType(w, p) == AP_LIST) {
			s = simple_write(w, AP_ListHead(w, p), s);
			p = AP_ListTail(w, p);
		}
		
		if (!AP_IsNullList(w, p)) {
			s += sprintf(s, "|");
			s = simple_write(w, p, s);
		}
		
		s += sprintf(s, "]");
	}	
	break;
    case AP_STRUCTURE:
    {
    	int arity, i;
    	
    	s += sprintf(s, "%s(", AP_GetAtomStr(w, AP_GetStructureFunctor(w, obj)));
    	
    	arity = AP_GetStructureArity(w, obj);
    	
    	i = 1;
    	while (1) {
    		s = simple_write(w, AP_GetArgument(w, obj, i), s);
    		i++;
    		if (i > arity) break;
    		s += sprintf(s, ",");
    	}
    	
    	s += sprintf(s, ")");
    }
	break;
    case AP_ATOM:
		s += sprintf(s, "%s", AP_GetAtomStr(w, obj));
	break;
    case AP_INTEGER:
    	s += sprintf(s, "%ld", AP_GetLong(w, obj));
	break;
    case AP_FLOAT:
    	s += sprintf(s, "%f", AP_GetDouble(w, obj));
	break;
    }
    
    return s;
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	int   exit_status;
	PI_system_setup setup;
	AP_Obj term;
	AP_World *w = NULL;

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = getenv("DEV_ALSDIR");
    setup.saved_state = NULL;
    setup.load_executable_state = 1;
    setup.argc = __argc;
    setup.argv = __argv;
    setup.hInstance = hInstance;
    setup.hPrevInstance = hPrevInstance;
    setup.lpCmdLine = lpCmdLine;
    setup.nCmdShow = nCmdShow;

    //PI_set_console_functions(standard_console_read, standard_console_write,
    //				standard_console_error);

	if ((exit_status = PI_startup(&setup)) != 0) {
		PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
		exit(EXIT_ERROR);
    }

	pi_init();

	/* Only load blt_dvsh when there is no saved state.
	   Since blt_dvsh is part of the state, reconsulting
	   does not work correctly. */
{
	extern char executable_path[1000];
	extern long ss_image_offset(const char *image_name);

	if (!ss_image_offset(executable_path)) {
#if 0
		term = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "consult"),
					1, AP_NewSymbolFromStr(w, "ldr_dvsh"));
			
	    AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
#endif
		MessageBox(GetFocus(), "This is just a stub!", "ALS Prolog", 0);

	}
}

    term = AP_NewSymbolFromStr(w, "$start");
    {
    	AP_Result r;
    	r = AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
    	if (r == AP_EXCEPTION) {
    		char s[1000];
    		simple_write(w, AP_GetException(w), s);
    		MessageBox(GetFocus(), s, "ALS Prolog", 0);
    	} 
	}
	
    PI_shutdown();
}
