#include "alspi.h"

static char *args[]= {
	"alspro_b",
	"-b",
	"-obp",
	"-no_dot_alspro",
	"::als_dev:alsdev:ldr_alsdev.pro",
	"-g",
	"bldit('::', attach_image('ALS Prolog PPC',[]))"
};


int main()
{
    return PI_main(7, args, NULL);
}
