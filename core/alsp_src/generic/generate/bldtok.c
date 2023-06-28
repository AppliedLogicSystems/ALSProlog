/*
 * bldtok.c		-- program to build tokens.h out of tokini.h,
 *			   parser.h, and hash.c
 *	Copyright (c) 1987-91,  Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 8/1/87
 * Revision History:
 *	Revised: 11/29/90	Kev	-- adapted to new expandable scheme
 */


#include <stdio.h>
#include "../parser.h"

#define TK(idx,t) {#idx,t,0,0}
#define OP(idx,t,a,p) {#idx,t,a,p}

struct tokini {
    char *id;
    char *name;
    int unop;
    int binop;
} t[] = {
#include "../tokini.h"
};


char *cvtstr(in) 	/* looks for \'s in a string and fixs them up */
    char *in;
{
    static char buf[100];
    char *out = buf;

    while (*in) {
	if (*in == '\\')
	    *out++ = '\\';
	*out++ = *in++;
    }
    *out = '\0';
    return buf;
}


main()
{
    int i,idx;
    FILE *fp;
    FILE *fp2;

    if ((fp=fopen("tokens.h","w"))==NULL) {
	fprintf(stderr,"Error opening tokens.h for write access\n");
	exit(1);
    }

    if ((fp2=fopen("tokini2.h","w"))==NULL) {
	fprintf(stderr,"Error opening tokini2.h for write access\n");
	exit(1);
    }
    
    for (i=0; i < sizeof(t) / sizeof(struct tokini); i++) {
	fprintf(fp,"#define %-16s\t%d\t/* %s */\n", t[i].id, i+1, t[i].name);
	fprintf(fp2,"{(unsigned char *)\"\\%o%s\"+1,%d,%d}",strlen(t[i].name),
			cvtstr(t[i].name), t[i].unop, t[i].binop);
	if (i == (sizeof(t)/sizeof(struct tokini))-1)
	    fprintf(fp2,"\n");
	else
	    fprintf(fp2,",\n");
    }

    fclose(fp);
    fclose(fp2);
    exit(0);
}
