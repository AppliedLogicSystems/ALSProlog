/*
 * prebldtok.c
 *	Copyright (c) 1993 Applied Logic Systems, Inc
 *
 * Author: Kevin A. Buettner
 * Creation: 6/2/93
 * Revision History:
 *
 * Description:
 *	With the advent of ANSI standard C, we can no longer do certain things
 * the way that we used to.  One of the things which we used to be able to
 * do is as follows:
 *	#define str(s) "s"
 * str(foo) would in the past generate the string "foo".  No longer; it
 * now generates "s" (in Ansi C).  The proper way to do the above in Ansi
 * C is:
 *	#define str(s) #s
 *
 * But this doesn't work for older C compilers.  So, this little program
 * attempts to create a string using the old method and will print to
 * standard output -DOldStrs if the old method works. Otherwise, it will
 * output nothing.  This is little program is meant to be used in the
 * command line to bldtok in the Makefile.
 */

#define str(s) "s"

main()
{
    if (strcmp(str(foo),"foo") == 0)
	printf("-DOldStrs");
	exit(0);
}
