/*
 * bdos.c       -- Builtins which make MS-DOS system calls
 *
 * Copyright (c) Applied Logic Systemes, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 7/23/91
 *
 * Note :
 * 		This file originally was created as "dospreds.c" for MetaCourier
 * 		interface. ( Date: 11/8/89). That file is converted to this file.
 * 		(It contained the predicates defined here as foreign predicates).
 *
 * 		Normally, we should write new predicates for certain MS-DOS system
 * 		calls which cannot be done by just using the predicate "$$int86".
 */


#include "config.h"

/*
 * Include this file for only DOS version of the system
 */

#ifdef DOS


#include "types.h"
#include "mtypes.h"
#include "built.h"
#include "winter.h"
#include "parser.h"
#include "dos386.h"


/*
 * Format DTA (Disk Transfer Address) Buffer
 */
#define LOC_FATTR 	0x15
#define LOC_FTIME	0x16
#define LOC_FDATE	0x18
#define LOC_FSIZE	0x1A
#define LOC_FNAME	0x1E

#define DTA_SIZE 	64 

/*
 * DTA (Disk Transfer Address) Buffer
 */
static char  dta_buf[DTA_SIZE];

/*
 * Whether DTA buffer is intialized by an earlier interrupt 4E,
 * i.e. second "int386x" function call in "pbi_dos_first_match"
 * is executed earlier.
 */
static int dta_intialized = 0;

/*
 * Maximum size of a path name
 */
#define MAX_PATHNAME_SIZE 	256



/*
 *	$$dos_first_match(FAttrs,Path,FAttr,FTime,FDate,FSize,FName)
 *
 *  This routine returns the first file in the specified directory
 * 	which matches the file name and has a subset of the specified
 * 	file attributes. 
 *	This routine issues two interrupts. The first interrupt stores
 * 	the result in the program's DTA (Disk Transfer Address) buffer.
 *	The second interrupt gets a pointer to that buffer.
 */
pbi_dos_first_match()
{
	PWord v1, v2, v3, v4, v5, v6, v7;
	int t1, t2, t3, t4, t5, t6, t7;

	PWord fattr, ftime, fdate, fsize, fname;
	int   fnametype;
	char  pathname[MAX_PATHNAME_SIZE];
	char  *dta;
	union REGS386 in;
	struct SREGS386 seg;

	w_get_An(&v1,&t1,1);
	w_get_An(&v2,&t2,2);
	w_get_An(&v3,&t3,3);
	w_get_An(&v4,&t4,4);
	w_get_An(&v5,&t5,5);
	w_get_An(&v6,&t6,6);
	w_get_An(&v7,&t7,7);

	if (t1 == WTP_INTEGER && ((t2 == WTP_SYMBOL) || (t2 == WTP_UIA))) {
		if (t2 == WTP_SYMBOL) 
			strcpy(pathname,TOKNAME(v2));
		else 
			w_get_uianame(pathname,v2,MAX_PATHNAME_SIZE);

		/*
		 * Read original segment registers
		 */
		segread386(&seg);

   		/*
    	 * AH  = 1Ah   ( Set Disk Transfer Address )
		 * DS:EDX = pointer to DTA Buffer
		 */
   		in.h.ah = 0x1A;
		in.l.edx = (unsigned long) dta_buf;
		int386x(0x21,&in,&in,&seg);

   		/*
    	 * AH  = 4Eh   ( Search for First Match)
		 * CX  = file attributes
		 * DS:EDX = pointer to zero-terminated file path
		 */
		in.h.ah  = 0x4E;
		in.x.cx  = (unsigned short) v1;
		in.l.edx = (unsigned long) pathname;
		int386x(0x21,&in,&in,&seg);
		dta_intialized = 1;  		/* function 4E is executed */

   		/*
    	 * If carry flag is set, the DOS system call is failed.
		 */
   		if (in.l.cflag) 
	  	   FAIL

		/*
		 * Okay, it is sucessful. 
		 * Get the file information. 
		 */
		dta = (char *) dta_buf;
		fattr = (PWord) *(unsigned char *)(dta+LOC_FATTR);
		ftime = (PWord) *(unsigned short *)(dta+LOC_FTIME);
		fdate = (PWord) *(unsigned short *)(dta+LOC_FDATE);
		fsize = (PWord) *(unsigned long *)(dta+LOC_FSIZE);
		w_mk_uia(&fname,&fnametype,(char *)(dta+LOC_FNAME));

		if (w_unify(v3,t3,fattr,WTP_INTEGER) && 
			w_unify(v4,t4,ftime,WTP_INTEGER) &&
		    w_unify(v5,t5,fdate,WTP_INTEGER) && 
			w_unify(v6,t6,fsize,WTP_INTEGER) &&
			w_unify(v7,t7,fname,fnametype)) 
			SUCCEED
		else
			FAIL
	}
	else
		FAIL
}




/*
 *	$$dos_next_match(FAttr,FTime,FDate,FSize,FName)
 *
 *  Before a call to this function executed, the DTA buffer should contain
 *  results from a call to "pbi_dos_first_match" or to this function.
 *	This routine issues two interrupts. The first interrupt stores
 * 	the result in the program's DTA (Disk Transfer Address) buffer.
 *	The second interrupt gets a pointer to that buffer.
 */
pbi_dos_next_match()
{
	PWord v1, v2, v3, v4, v5;
	int t1, t2, t3, t4, t5;

	PWord fattr, ftime, fdate, fsize, fname;
	int   fnametype;
	char  *dta;
	union REGS386 in;
	struct SREGS386 seg;

	w_get_An(&v1,&t1,1);
	w_get_An(&v2,&t2,2);
	w_get_An(&v3,&t3,3);
	w_get_An(&v4,&t4,4);
	w_get_An(&v5,&t5,5);

	/* 
	 * If the second "int386x" in "pbi_dos_first_match" is not executed
	 * earlier, this function fails. (If we continue to execution,
	 * second "int386" call in this function screw ups since
	 * there is junk in DTA buffer.)
	 */
	if (dta_intialized == 0)
		FAIL;

	/*
	 * Read original segment registers
	 */
	segread386(&seg);

   	/*
     * AH  = 1Ah   ( Set Disk Transfer Address )
	 * DS:EDX = pointer to DTA Buffer
	 */
   	in.h.ah = 0x1A;
	in.l.edx = (unsigned long) dta_buf;
	int386x(0x21,&in,&in,&seg);

  	/*
     * AH  = 4Fh   ( Search for Next Match)
	 */
   	in.h.ah = 0x4F;
	int386(0x21,&in,&in);

   	/*
     * If carry flag is set, the DOS system call is failed.
	 */
   	if (in.l.cflag)
	   FAIL

	/*
	 * Okay, it is sucessful. 
	 * Get the file information. 
	 */
	dta = (char *) dta_buf;
	fattr = (PWord) *(unsigned char *)(dta+LOC_FATTR);
	ftime = (PWord) *(unsigned short *)(dta+LOC_FTIME);
	fdate = (PWord) *(unsigned short *)(dta+LOC_FDATE);
	fsize = (PWord) *(unsigned long *)(dta+LOC_FSIZE);
	w_mk_uia(&fname,&fnametype,(char *)(dta+LOC_FNAME));

	if (w_unify(v1,t1,fattr,WTP_INTEGER) && 
		w_unify(v2,t2,ftime,WTP_INTEGER) &&
	    w_unify(v3,t3,fdate,WTP_INTEGER) && 
		w_unify(v4,t4,fsize,WTP_INTEGER) &&
		w_unify(v5,t5,fname,fnametype)) 
		SUCCEED
	else
		FAIL
}



#endif 	/* DOS */
