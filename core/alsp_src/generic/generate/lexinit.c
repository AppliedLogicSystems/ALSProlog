/*
 * Generate/lexinit.c	-- Initialize lexical table.
 *
 * Author:  Keith Hughes
 * Creation: 12/20/88
 * Revision History:
 * 	12/20/88,	kmh	Written to eliminate some startup code.
 *  06/25/92,   ron Added a separate table for the Mac, which treats
 *                   some characters differently.
 */

#include "../defs.h"
#include "../lexan.h"

/*
 * Lexical Analysis Global Variables
 */


char lx_chtb[256];	/* character type table	*/

main()

{
	register int i;

	for (i=0;i<=' ';i++)
		lx_chtb[i] = LX_WSP;

	for (i=' '+1;i<256;i++)
		lx_chtb[i] = LX_SPEC;

	for (i='a';i<='z';i++)
		lx_chtb[i] = LX_LCAL;

	for (i='A';i<='Z';i++)
		lx_chtb[i] = LX_UCAL;

	for (i='0'; i<='9'; i++)
		lx_chtb[i] = LX_NUM;

	lx_chtb['\n'] = LX_NL;
	lx_chtb['{'] = LX_SNGL;
	lx_chtb['}'] = LX_SNGL;
	lx_chtb['('] = LX_SNGL;
	lx_chtb[')'] = LX_SNGL;
	lx_chtb['['] = LX_SNGL;
	lx_chtb[']'] = LX_SNGL;
	lx_chtb[','] = LX_SNGL;
	lx_chtb[';'] = LX_SNGL;
	lx_chtb['!'] = LX_SNGL;
	lx_chtb['|'] = LX_SNGL;
	lx_chtb['\''] = LX_SQT;
	lx_chtb['"'] = LX_DQT;
	lx_chtb['%'] = LX_CMNT;
	/* _ is used to start variables just like uppercase letters are	*/
	lx_chtb['_'] = LX_UCAL;
	lx_chtb['$'] = LX_LCAL;
	lx_chtb['~'] = LX_CHRQ;
	lx_chtb[0] = LX_EOF;
	lx_chtb[255] = LX_EOF;
	lx_chtb[4] = LX_EOF;

	printf("#ifndef MacOS\n");
	printf("char lx_chtb[256] = {\n");
	for (i=0; i < 255; i++)
		printf("\t%d,\n",(int)lx_chtb[i]);

	printf("\t%d\n};\n",(int)lx_chtb[255]);
	printf("\n#else\n");
	
	/* On the Mac, the newline character is 13 */
	lx_chtb[13] = LX_NL;

	printf("char lx_chtb[256] = {\n");
	for (i=0; i < 255; i++)
	    printf("\t%d,\n", (int)lx_chtb[i]);
	printf("\t%d\n};\n#endif /* MacOS */\n", (int)lx_chtb[255]);
	exit(0);
}
