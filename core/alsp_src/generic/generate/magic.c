/*=============================================================
 | 		sioinit.c	
 |
 |		-- Create the obp MAGIC string
 |
 | Author:  K.Bowen
 | Creation: 04/12/95
 *============================================================*/

#include "defs.h"

		/* Note: Unslashed length must be kept = 120: see mod_header in loadfile.c */
static char MAGIC[] =
"ALS-Prolog Loadable Object Module\\r\\nFormat 1.21(XXXXXXXXXX,YYYYYYYYYY)\\r\\n\\032\\004\\019\\026";

main()
{
	/* If not for the problem of keeping the fixed length, we could just do this:   
	printf("#define MAGIC \"ALS-Prolog Loadable Object Module\\r\\nFormat 1.21(%s,%s)\\r\\n\\032\\004\\019\\026\"\n",ProcStr,MinorOSStr); */

	fix_MAGIC();

    printf("#define MAGIC \"%s\"\n",MAGIC);

    exit(0);
}

int
fix_MAGIC()
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
