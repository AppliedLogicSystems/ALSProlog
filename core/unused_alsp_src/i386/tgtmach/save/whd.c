
#include <stdio.h>

main(argc,argv)

int argc;
char *argv[];

{
    static char *poorboydirs[] ={
		"DOSPHAR",
		"DOSERGO",
		"MSWins"
	};
	int ctr;

    if (argc < 2) {
        fprintf(stderr,"Usage: whd string\n");
        return(1);
    };

	for (ctr = 0; ctr < 3; ctr++) {
		if (strcmp(argv[1], poorboydirs[ctr]) == 0)
			return(0);
		}

	return(1);
}


