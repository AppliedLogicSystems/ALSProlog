/*------------------------------------------------------
		nolinkdir.c

		Tests whether or not the argument passed to it
		is one of the directories for a MACH which does
		not support links; so that everything has to 
		be copied.
			To add another such directory, simply add
		the MACH name in the initialization of poorboydirs.
 *-----------------------------------------------------*/

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
        fprintf(stderr,"Usage: nolinkdir name\n");
        return(1);
    };

	for (ctr = 0; ctr < (sizeof(poorboydirs)/sizeof(char *)); ctr++) {
		if (strcmp(argv[1], poorboydirs[ctr]) == 0)
			return(0);
		}

	return(1);
}


