/*
 *	icode.c
 *	Copyright (c) 1988-91 Applied Logic Systems, Inc.
 *
 *|	Create the icode.h macros file from icodedef.h
 *
 *	Written by Keith Hughes
 */

#include <stdio.h>

#include "defs.h"

#define ICODE(macro,str,x,y)	str,

char *icodes[] = {

#include "icodedef.h"

	(char *)0
};

main()

{
	int i;

	printf("/*\n *\ticode.h\n *\tCopyright (c) 1988-91 Applied Logic Systems, Inc.\n");
	printf(" *| Macro definitions for WAM ICODES;\n");
	printf(" *| This file generated from icodedef.h by Generic/Generate/icode.c\n */\n\n");

	for (i = 0;icodes[i]; i++)
		printf("#define %s\t%d\n",icodes[i],i);
	exit(0);
}
