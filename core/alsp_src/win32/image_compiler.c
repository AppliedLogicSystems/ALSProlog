#include "alspi.h"

static void prolog_quote(char *s, const char *cs)
{
	while (*cs) {
		switch (*cs) {
		case '\\':
			*s = '\\'; s++;
			*s = '\\'; s++;
			break;
		default:
			*s = *cs; s++;
			break;
		}
		cs++;
	}
	
	*s = 0;
}

int main(int argc, char *argv[])
{
	PI_system_setup setup =
		{0, 0, 0, NULL, NULL, 0, 0, NULL, NULL, NULL, NULL, 0};
	char quoted_name[512];
	char command[512];
	char *compiler_argv[] = {"alspro", "-b", "-g", NULL};

	if (argc == 2) {
		printf("%s\n", argv[1]);
		prolog_quote(quoted_name, argv[1]);
		printf("%s\n", quoted_name);
		sprintf(command, "attach_image('%s',[select_lib(builtins,[debugger])]).", quoted_name);
		compiler_argv[3] = command;
		
		setup.alsdir = "..\\alsp_src";
		setup.argc = 4;
		setup.argv = compiler_argv;
		
	    PI_startup(&setup);
	    
	    PI_toplevel();
    } else {
    	printf("Please drop a base on this exe to add builtin image to it\n");
    }
}
