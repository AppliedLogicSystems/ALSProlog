/*
	makeimage.c

	Make a distribution image of alspro that will shut off after so
	many days. 0 days means that it is good forever.

	Use is   makeimage prolog-image serial# [days]

	where days is the number of days that the demonstration copy should
	last.

	Written by Keith Hughes in the silliest manner possible.

	This program assumes that there is a global variable in the program
	called presult which is used for the comparison. This name was chosen
	as something suitably obscure to make it difficult to find the
	checking routines.
*/

#include <stdio.h>
#include <sys/file.h>

main(argc,argv)

int argc;
char *argv[];

{
	char execute[64];
	unsigned long days = 30; /* Default number of days for evaluation */
	unsigned long serial = 0;	/* Serial number for copy */
	int mkimage;

	if (argc < 3) {
		fprintf(stderr,"Usage: makeimage image-name serial# [days]\n");
		exit(1);
	}

	if (sscanf(argv[2],"%d",&serial) == 0) {
		fprintf(stderr,
			"makeimage: '%s' is incorrect serial# format\n",
			argv[2]);
		exit(1);
	}

	if (argc == 4) {	/* There is a second argument */
		if (sscanf(argv[3],"%d",&days) == 0) {
			fprintf(stderr,
				"makeimage: '%s' is incorrect day format\n",
				argv[3]);
			exit(1);
		}
	}

	/* Can give it 0 days */
	if (days)
		days = days * 86400 + (unsigned long)time(0);

	/* Uses open() so that can set the protection bits though it makes
	   output obnoxious */

	if ((mkimage = open("mkimage",O_WRONLY|O_CREAT,0777)) == -1) {
		perror("makeimage: Cannot create mkimage");
		exit(1);
	}

	sprintf(execute,"echo 'presult?W 0x%lx' | adb -w %s -\n",days,argv[1]);
	write(mkimage,execute,strlen(execute));
	sprintf(execute,"echo 'serialNum?W 0x%lx' | adb -w %s -\n",
		serial,argv[1]);
	write(mkimage,execute,strlen(execute));
	sprintf(execute,"rm mkimage\n");
	write(mkimage,execute,strlen(execute));

	close(mkimage);

	if (execl("/bin/csh","csh","mkimage",0) == -1) {
		perror("makeimage: Horrible error! exec failed!\n");
		exit(1);
	}
}


	
