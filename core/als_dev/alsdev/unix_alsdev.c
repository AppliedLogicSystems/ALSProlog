#include "alspi.h"

int main(int argc, char *argv[])
{
    return PI_main(argc, argv, pi_init);
}

#if 0
#include "new_alspi.h"
#include <stdlib.h>
#include <stdio.h>

#include <tcl.h>
#include <tk.h>

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

	extern char executable_path[1024];
	extern long ss_image_offset(const char *image_name);

int main(int argc, char ** argv)
{
	int   exit_status;
	PI_system_setup setup;
	AP_Obj term;
	AP_World *w = NULL;

	//	PI_main(argc, argv, NULL);
	//	return 0;

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = getenv("DEV_ALSDIR");
    setup.saved_state = NULL;
    setup.load_executable_state = 0;
    setup.argc = argc;
    setup.argv = argv;

	if ((exit_status = PI_startup(&setup)) != 0) {
		PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
		exit(EXIT_ERROR);
    }

	pi_init();

	/* Only load blt_dvsh when there is no saved state.
	   Since blt_dvsh is part of the state, reconsulting
	   does not work correctly. */
{

	if (!ss_image_offset(executable_path)) {
#if 0
		term = AP_NewInitStructure(w, AP_NewSymbolFromStr(w, "consult"),
					1, AP_NewSymbolFromStr(w, "ldr_dvsh"));
			
	    AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
#endif
		printf("This is just a stub!\n");

	}
}

//term = AP_NewSymbolFromStr(w, "$start");
     term = AP_NewSymbolFromStr(w, "start_alsdev");
    {
    	AP_Result r;
    	r = AP_Call(w, AP_NewSymbolFromStr(w, "builtins"), &term);
    	if (r == AP_EXCEPTION) {
    		char s[1000];
    		simple_write(w, AP_GetException(w), s);
    		printf("ALS Prolog: %s\n", s);
    	} 
	}
	
    PI_shutdown();

    return 0;
    
}

#endif
