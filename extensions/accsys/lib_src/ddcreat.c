/************************************************************
*                                                           *
* Copyright 1989 - 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                      *
*                                                           *
*************************************************************
*                                                           *
* Published by                                              *
*        Copia International, Inc.                          *
*        Wheaton, Illinois                                  *
*        U. S. A.                                           *
*                                                           *
*************************************************************/
 
/************
* dDcreat.c *
*************/

#include <ctype.h>
#ifdef MSC
#include <sys\types.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#include <sys\stat.h>
#endif

/***
 *** Example calling sequence:
 ***
	int  rc;

	#ifndef _WINDOWS	-- for Non-Windows --
		
		char *fields[] = { "Item",    "C30",
				   "Price",   "N10.2",
				   "Status",  "L",
				   "Date",    "D",
				   "Memo",    "M"
				 };
		rc = dDcreat("SALES", 5, fields);

	#else			-- for Windows --
		LPSTR fields[] = { (LPSTR) "Item",   (LPSTR) "C30",
				   (LPSTR) "Price",  (LPSTR) "N10.2",
				   (LPSTR) "Status", (LPSTR) "L",
				   (LPSTR) "Date",   (LPSTR) "D",
				   (LPSTR) "Memo",   (LPSTR) "M"
				 };
		rc = dDcreat((LPSTR) "SALES", 5, (pLPSTR) fields);
	     	
	#endif
 ***/

#ifdef __HIGHC__
int _DECLARE allblank(
#ifndef unix 
CHAR_PTR cptr)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif 	/* __HIGHC__ */

int DECLARE dDcreat(
#ifndef unix 
			/* d_report arrangement: N/A */
	/* inputs */
#ifndef ACDLL
#else /* ACDLL */
int DECLARE DdDcreat(
			/* d_report arrangement: N/A */
	/* inputs */
	DBGDAT_PTR dbgptr,   /* ptr to global data block */
#endif
	CHAR_PTR dbfname, /* DBF filename */
	int     nofields, /* no. of fields */

#ifndef _WINDOWS
	CHAR_PTR fields[] /* ptr to fields info */
#else
	pLPSTR fields     /* ptr to fields info */
#endif
	/* output : none */
		   )
#else  /* unix */ 
 	dbfname, nofields, fields ) 
	CHAR_PTR dbfname; 
	int nofields; 
	CHAR_PTR fields[]; 
#endif /* unix */ 


{
char exname[ACTMPLEN];

dUexpnm(dbfname, "DBF", exname);

return(_dcre8(
#ifdef ACDLL
		dbgptr,
#endif
		NEW, exname, nofields, (INT_PTR) fields));
} /* end of dDcreat() */

int DECLARE dDcopy(
#ifndef unix 
#ifndef ACDLL
 /* d_report arrangement: 140 */
	/* inputs */
#else
int DECLARE DdDcopy( /* d_report arrangement: 140 */
	/* inputs */
	DBGDAT_PTR dbgptr,   /* ptr to global data block */
#endif
	CHAR_PTR src,    /* original file name */
	CHAR_PTR dest    /* new file name */
		  )
#else  /* unix */ 
	
 	src, dest ) 
	CHAR_PTR src; 
	CHAR_PTR dest; 
#endif /* unix */ 

	/* output: none */
{
DBF origtbl;
int i;
char exname[ACTMPLEN];

if (!src || !dest || !ACstrcmp(src, dest))
{
    d_report = 141;
    return(dILLEGAL);
}

dUexpnm(src, "DBF",  exname);
origtbl = _dDopen(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_SINGLE, 1);
if (!origtbl) return(dretcode);

dUexpnm(dest, "DBF", exname);
i = _dcre8(
#ifdef ACDLL
	dbgptr,
#endif
	COPY, exname, ((PTBLFILE) origtbl)->fldsnum, (INT_PTR) origtbl);

(void) _dDclose(origtbl);

return(i);
} /* end of dDcopy() */


static int _DECLARE _dcre8(
#ifndef unix 

#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	int	 opcode, /* NEW, COPY */
	CHAR_PTR filename, /* file name */
	int      nofields, /* no. of fields */
	INT_PTR  castptr   /* opcode == NEW:  (pCHAR_PTR) ptr to fields info
			      opcode == COPY: (PTBLFILE) tblptr  */
			)
#else  /* unix */ 
	
 	opcode, filename, nofields, castptr ) 
	int opcode; 
	CHAR_PTR filename; 
	int nofields; 
	INT_PTR castptr; 
#endif /* unix */ 

{
int fn;     /* file number */
pCHAR_PTR fields;
pCHAR_PTR chkptr; /* ptr to field info */
PFLDNM nmptr;	/* ptr to field name info of an existing file */
PFLDTL tlptr;   /* ptr to field type & length info of an existing file */
#ifndef _WINDOWS
#define fieldname _dminbuf
#else
#define fieldname _dWINbuf
#endif
CHAR_PTR cptr, ptr;
int fldtype; /* field type */
int  i, iii;  /* general purpose integer variables */
int length; /* record */
int year, month, day;
int decimals;
int nmlen;
int hasmemo;
CHAR_PTR memory;

#ifndef __HIGHC__
int _DECLARE allblank(
#ifndef unix 
CHAR_PTR cptr)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif 	/* __HIGHC__ */

#define MEMSIZE 8200 /* this ensures 255 fields entry, DBF maximum */

if (nofields <= 0  || nofields > MAXFLDS)
{
    d_report = 142;
    return(dOUTRANGE);
}

if (opcode == NEW) fields = (pCHAR_PTR) castptr;
else
{
    nmptr = ((PTBLFILE) castptr)->fldnames;
    tlptr = ((PTBLFILE) castptr)->fltyplen;
}

memory = (CHAR_PTR) _ACalloc((unsigned long) MEMSIZE);
if (!memory)
{
    d_report = 143;
    return(dMEMERR); /* memory allocation failure */
}

(void) ACmemset(memory, 0, MEMSIZE); /* clear buffer */

dUtoday(&month, &day, &year);
*(memory + MONTH)  = (char) month;
*(memory + DAY)  = (char) day;
*(memory + YEAR) = (char) ((year - 1900) & 0x00ff);

hasmemo = 0;

/*** Enter Field Names ***/
length = 0;
for (i=1, cptr = memory + FIELDS;
     i <= nofields;
     i++, cptr += NXTFLD)
{
    /* check the name length */
    ACstrcpy(fieldname, (opcode == NEW) ? ACstrupr(*fields) :
    					  (CHAR_PTR) nmptr->fieldnm);
    if ( (nmlen = ACstrlen(fieldname)) > MXCHRSnm)
    				/* MXFCHRSnm characters maximum */
    {
	d_report = 144;
	_ACfree(memory);
	return(dOUTRANGE);
    }
    /* check against blank name */
    if (allblank(fieldname))
    {
	d_report = 145;
	_ACfree(memory);
        return(dILLEGAL);
    }
                    
    /* check against duplicate */
    if (opcode == NEW)
    {
	for (iii=1, chkptr = (pCHAR_PTR) castptr; iii < i; iii++)
	{
	    if (!ACstrcmp(*chkptr, fieldname))
            {
	        d_report = 146;
	        _ACfree(memory);
                return(dILLEGAL);
            }
            chkptr++;
            chkptr++;
        } /* end of inside for-loop */
    }
    /* good name; enter it! */
    (void) ACmemcpy(cptr, fieldname, nmlen);

    /*** Now work on the field type, length, decimal places ***/
    if (opcode == NEW)
    {
	fields++;

	fldtype = toupper(**fields);
	decimals = 0;

	switch(fldtype)
	{
	case 'C':
	    iii = ACatoi(*fields + 1);
	    if (!iii || iii > MXCFLDLEN)
	    {
		d_report = 147;
		_ACfree(memory);
		return(dOUTRANGE);
	    }
	    break;
	case 'N':
	case 'F':
	    ptr = *fields + 1;
	    iii = ACatoi(ptr);
	    while (*ptr && *ptr != '.') ptr++;
	    if (*ptr) ptr++;
	    decimals = ACatoi(ptr);
	    fn = decimals ? decimals : -1;
	    if (!iii || iii > MXNKLEN || iii < (fn + 2))
	    {
		d_report = 148;
		_ACfree(memory);
		return(dOUTRANGE);
	    }
	    break;
        case 'D':
	    iii = 8;
	    break;
	case 'L':
	    iii = 1;
	    break;
        case 'M':
	    iii = 10;
	    hasmemo++;
	    break;
	default: /* illegal field type */
	    d_report = 149;
	    _ACfree(memory);
	    return(dILLEGAL);
	}
	fields++; /* get ready to get next field */
    }
    else
    {
	fldtype  = tlptr->type;
	if (fldtype == 'M') hasmemo++;
	iii      = tlptr->length;
	decimals = tlptr->decimal;

	/* get ready to get next field */
	nmptr++;
	tlptr++;
    }

    *(cptr + FLDTYP)  = (char) fldtype;
    *(cptr + FLDLEN)  = (char) iii;
    *(cptr + FLDDEC) = (char) decimals;

    length += iii;
    if (length > MAXRECLEN)
    {
	d_report = 150;
	_ACfree(memory);
	return(dOUTRANGE);
    }

} /* end of outside for-loop */

*memory = hasmemo ? DBFwDBT4 : DBFwoDBT;
*cptr++ = FIELDEND;
*cptr   = ENDMARK;

iii = cptr - memory;
cptr = memory + TBLADRS;
*cptr++ = (char) (iii & 0x0ff);
*cptr++ = (char) ((iii >> 8) & 0x0ff);
length++; /* for status byte */
*cptr++ = (char) (length & 0x0ff);
*cptr++ = (char) ((length >> 8) & 0x0ff);

fn = _dcre8new(filename, FORCE);
if (fn < 0)
{
	d_report = 151;
	_ACfree(memory);
	return(dIOERR);    /* file creation by open() failed */
}

/* write the buffer */
iii++;
if (_ACwrite(fn, memory, (unsigned) iii) != (unsigned) iii)
{
    close(fn);
    (void) _ACremove(filename);
    _ACfree(memory);
    d_report = 152;
    return(dIOERR);
}

close(fn);
_ACfree(memory);
return(SUCCESS);

} /* end of _dcre8() */

static int _DECLARE allblank(
#ifndef unix 
CHAR_PTR cptr)
#else  /* unix */ 
	cptr ) 
	CHAR_PTR cptr; 
#endif /* unix */ 

{
int i;

if (!cptr) return(0);

for (i=1; *cptr && i<=30; i++, cptr++)
    if (*cptr != ' ' && *cptr != 0x09) return(0);
return(1); /* all blanks */

} /* end of allblank() */
