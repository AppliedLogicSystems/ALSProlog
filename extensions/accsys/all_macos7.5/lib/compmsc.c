/*
 * compmsc.c 	--  Functions exist in Microsoft C 5.1 Library, 
 *					but they don't exist in HighC 1.7 Library
 *
 * Copyright (c) Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 9/6/1991
 */

#include <stdio.h>
#include "d4def.h"

#if !defined(HAVE_ECVT)
static char buf_for_ecvt[128];

char *ecvt(value,count,dec,sign)
	double 	value;
	int 	count;
	int 	*dec;
	int 	*sign;
{
	char 	*p;
	char 	*retval;
	char 	format[8];

	if (count < 1) {
		buf_for_ecvt[0] = 0;
		return((char *)&buf_for_ecvt[0]);
	}
	else if (count > 100)
		count = 100;

	count--;
	sprintf(format,"%%.%de", count);
	sprintf(buf_for_ecvt,format,value);

	if (buf_for_ecvt[0] == '-') {
		*sign = 1;
		buf_for_ecvt[2] = buf_for_ecvt[1]; 	//-- move first digit 
		retval = &buf_for_ecvt[2];
		p = &buf_for_ecvt[(3+count)];
	}
	else {
		*sign = 0;
		buf_for_ecvt[1] = buf_for_ecvt[0]; //-- move first digit 
		retval = &buf_for_ecvt[1];
		p = &buf_for_ecvt[(2+count)];
	}

	*p++ = 0;
	if (*p++ == '-')
		*dec = -(((*p-'0')*10) + (*(p+1)-'0'));
	else
		*dec = (((*p-'0')*10) + (*(p+1)-'0'));

	*dec += 1;

	return(retval);
}

#endif /* HAVE_ECVT */


/* I should write _memavl function  -- Ilyas 
   Assume that there is always 1MB memory available for malloc */
_memavl()
{
	return(0x100000);
}

#include <memory.h>
#include <string.h>
#include <ctype.h>
/* #include <floatingpoint.h> */
#include <float.h>

#if !defined(HAVE_MEMMOVE)
char *
memmove(dest,src,count)
	char *dest;
	char *src;
	unsigned long count;
{
	register char *p;
	register char *q;
	register char *lastone;

	if ((dest <= src) && (dest+count > src)) {
		for (p=dest, q=src, lastone=src+count; q < lastone; ) 
			*p++ = *q++; 
	}
	else if ((dest > src) && (src+count > dest)) {
		for (p=dest+(count-1), q=src+(count-1), lastone=src-1; q > lastone; ) 
			*p-- = *q--; 
	}
	else {
		memcpy(dest,src,count);
	}

	return(src);
}

#endif /* HAVE_MEMMOVE */

#if !defined(HAVE_STRUPR)
char *
strupr(str)
	char *str;
{
	register char ch;
	register char *p;
	char *newstr;

	if ((newstr=strdup(str)) == NULL)
		return(NULL);
	for (p=newstr; (ch=*p) != 0; p++) {
		if (islower(ch))
			*p = toupper(ch);
	}
	return(newstr);
}
#endif /* HAVE_STRUPR */


#if !defined(HAVE_ITOA)
char *
itoa(val,str,radix)
	int  val;
	char *str;
	int  radix; 	/* not used */
{
	int decpt;
	int sign;

	if (val < 0 ) { 
/*		econvert((double)val,12,&decpt,&sign,str+1);  */
		ecvt((double)val,12,&decpt,&sign,str+1);
		*str = '-';
		*(str+(decpt+1)) = 0;	
	}
	else {
/*		econvert((double)val,12,&decpt,&sign,str);   */
		ecvt((double)val,12,&decpt,&sign,str);
		*(str+decpt) = 0;	
	}
	return(str);
}
#endif /* HAVE_ITOA */

/*
char *
ltoa(val,str,radix)
	long  val;
	char *str;
	int  radix; 	-- not used 
{
	return(itoa((int)val,str,radix));
}
*/
