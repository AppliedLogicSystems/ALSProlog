/*
	expire.c

	Determines the presult value to put into the paction.c program.

	Use is   expire days

	where days is the number of days that the demonstration copy should
	last.

	Written by Keith Hughes in the silliest manner possible.

*/

#include <stdio.h>
#include <sys/file.h>

main(argc,argv)

int argc;
char *argv[];

{
	unsigned long days = 30; /* Default number of days for evaluation */

	if (argc < 2) {
		fprintf(stderr,"Usage: expire days\n");
		exit(1);
	}


	if (sscanf(argv[1],"%d",&days) == 0) {
		fprintf(stderr,
			"expire: '%s' is incorrect day format\n",
			argv[1]);
		exit(1);
	}

	days = days * 86400 + (unsigned long)time(0);

	printf("presult = %d\n",days);

}


	
