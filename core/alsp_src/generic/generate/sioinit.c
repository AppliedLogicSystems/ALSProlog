/*
 * Generate/sioinit.c	-- Initialize sio lexical table.
 *
 * Author:  Kevin Buettner
 * Creation: 10/28/91
 */

#include "../bsio.h"

/*
 * Lexical Analysis Global Variables
 */


unsigned short sio_chtb[256];	/* character type table	*/

main()

{
    register int i;

    for (i=0;i<=' ';i++)
	sio_chtb[i] = SIOC_WHITESPACE;

    for (i=127;i<256;i++)
	sio_chtb[i] = SIOC_WHITESPACE;

    for (i='a';i<='z';i++)
	sio_chtb[i] = SIOC_LOWERCASE;

    for (i='A';i<='Z';i++)
	sio_chtb[i] = SIOC_UPPERCASE;

    for (i='0'; i<='9'; i++) {
	if (i < '8')
	    sio_chtb[i] = SIOC_DECIMAL | SIOC_OCTAL;
	else
	    sio_chtb[i] = SIOC_DECIMAL;
    }

    for (i='a'; i<='f'; i++)
	sio_chtb[i] |= SIOC_HEXLOWER;
    
    for (i='A'; i<='F'; i++)
	sio_chtb[i] |= SIOC_HEXUPPER;

    sio_chtb['{'] = SIOC_SINGLE;
    sio_chtb['}'] = SIOC_SINGLE;
    sio_chtb['('] = SIOC_SINGLE;
    sio_chtb[')'] = SIOC_SINGLE;
    sio_chtb['['] = SIOC_SINGLE;
    sio_chtb[']'] = SIOC_SINGLE;
    sio_chtb[','] = SIOC_SINGLE;
    sio_chtb[';'] = SIOC_SINGLE;
    sio_chtb['!'] = SIOC_SINGLE;
    sio_chtb['|'] = SIOC_SINGLE;
    sio_chtb['"'] = SIOC_SPECIAL;
    sio_chtb['\''] = SIOC_SPECIAL;
    /* _ is used to start variables just like uppercase letters are	*/
    sio_chtb['_'] = SIOC_UPPERCASE;
    sio_chtb['e'] |= SIOC_E;
    sio_chtb['E'] |= SIOC_E;

    sio_chtb['#'] = SIOC_GRAPHIC;
    sio_chtb['$'] = SIOC_GRAPHIC;
    sio_chtb['&'] = SIOC_GRAPHIC;
    sio_chtb['*'] = SIOC_GRAPHIC;
    sio_chtb['+'] = SIOC_GRAPHIC | SIOC_PLUSMINUS;
    sio_chtb['-'] = SIOC_GRAPHIC | SIOC_PLUSMINUS;
    sio_chtb['.'] = SIOC_GRAPHIC;
    sio_chtb['/'] = SIOC_GRAPHIC;
    sio_chtb[':'] = SIOC_GRAPHIC;
    sio_chtb['<'] = SIOC_GRAPHIC;
    sio_chtb['='] = SIOC_GRAPHIC;
    sio_chtb['>'] = SIOC_GRAPHIC;
    sio_chtb['?'] = SIOC_GRAPHIC;
    sio_chtb['@'] = SIOC_GRAPHIC;
    sio_chtb['^'] = SIOC_GRAPHIC;
    sio_chtb['~'] = SIOC_GRAPHIC;
    sio_chtb['`'] = SIOC_GRAPHIC;
    sio_chtb['\\'] = SIOC_GRAPHIC;

    sio_chtb[7] |= SIOC_ESCCHAR;	/* \a */
    sio_chtb['\b'] |= SIOC_ESCCHAR;
    sio_chtb['\f'] |= SIOC_ESCCHAR;
    sio_chtb['\n'] |= SIOC_ESCCHAR;
    sio_chtb['\r'] |= SIOC_ESCCHAR;
    sio_chtb['\t'] |= SIOC_ESCCHAR;
    sio_chtb['\v'] |= SIOC_ESCCHAR;
    sio_chtb['\''] |= SIOC_ESCCHAR;
    sio_chtb['\\'] |= SIOC_ESCCHAR;
#ifdef SIO_BACKQUOTE_FOR_CHAR_CONSTS
    sio_chtb['`'] = SIOC_SPECIAL;
#endif /* SIO_BACKQUOTE_FOR_CHAR_CONSTS */

    printf("unsigned short sio_chtb[256] = {\n");
    for (i=0; i < 255; i++)
	if (i < ' ' || i>=127)
	    printf("\t0x%x,\n",(int)sio_chtb[i]);
	else
	    printf("\t0x%x,\t/* %c */\n",(int)sio_chtb[i],i);

    printf("\t0x%x\n};\n",(int)sio_chtb[255]);

    exit(0);
}
