/*=============================================================
 | 		magic.c	
 |
 |		-- Create the obp MAGIC string
 |
 | Author:  K.Bowen
 | Creation: 04/12/95
 *============================================================*/

#include <stdio.h>
#include "defs.h"

int fix_MAGIC(void);

		/* Note: Unslashed length must be kept = 120: see mod_header in loadfile.c */
static char MAGIC[] =
"ALS-Prolog Loadable Object Module\\r\\nFormat 1.21(XXXXXXXXXX,YYYYYYYYYY)\\r\\n\\032\\004\\019\\026";

main()
{
	FILE *fp;

	/* If not for the problem of keeping the fixed length, we could just do this:   
	printf("#define MAGIC \"ALS-Prolog Loadable Object Module\\r\\nFormat 1.21(%s,%s)\\r\\n\\032\\004\\019\\026\"\n",ProcStr,MinorOSStr); */

	fix_MAGIC();

    fp = fopen("magic.h", "w");
    fprintf(fp, "#define MAGIC \"%s\"\n",MAGIC);
    fclose(fp);

    exit(0);
}

int
fix_MAGIC(void)
{
    char *m = MAGIC;
    char *p = ProcStr;
    char *o = MinorOSStr;

    while (*m && *m != 'X')	/* find first X */
	m++;
    if (!*m)
	return;			/* return if at end of string */
    while (*m == 'X' && *p)	/* replace X's */
	*m++ = *p++;
    while (*m && *m != 'Y')	/* find first Y */
	m++;
    if (!*m)
	return;			/* return if at end of string */
    while (*m == 'Y' && *o)	/* replaces Y's */
	*m++ = *o++;
}
