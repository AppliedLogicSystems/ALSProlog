#include "alspi.h"

static char *args[]= {
	"alspro_b",
	"standard_make.pro"
};

int main()
{
    return PI_main(2, args, NULL);
}

#if 0
int main()
{
    PI_system_setup setup;

	setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = "::alsp_src:";
    setup.saved_state = NULL;
    setup.load_executable_state = 0;
    setup.argc = 2;
    setup.argv = NULL;

    PI_set_console_functions(standard_console_read, standard_console_write,
    				standard_console_error);

 PI_startup(&setup);
 PI_status_toplevel(&success);
 PI_shutdown()
}
#endif