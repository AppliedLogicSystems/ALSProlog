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


/*---------------------------------------------------------
 |   Include ALS Prolog config.h:
 |   -- need appropriate -I switch in makefile
 |	    command lines; generally, the ../builds/ARCH_OS
 |		directory for the particular architecture and os.
 *--------------------------------------------------------*/

#include "config.h"

#ifdef UNIX
#define unix 1
#endif /* UNIX */

/*********
 * db4.h *
 *********/

#ifndef AccDB4

#if defined(__TURBOC__)						/* Turbo C */
#define DECLARE cdecl
#elif defined(__ZTC__)						/* Zortech C/C++ */
#define DECLARE _cdecl
#elif defined(_WINDOWS) && defined(ACDLL)	/* Windows DLL */
#define DECLARE FAR PASCAL		    		/* (Microsoft C6.0) */
#elif (_MSC_VER >= 600)
#define DECLARE _cdecl						/* Microsoft C6.0 or later */
#else
#define DECLARE								/* other C compilers */
#endif

/* AccSys special types */
        /* The following new type definitions allow the compilers to
	   		generate error/warning messages when pointers to database,
	   		index, and memo files are erroneously used.
         */
#ifndef _WINDOWS
    typedef char             * CHAR_PTR;
    typedef int              * INT_PTR;
    typedef unsigned int     * UINT_PTR;
    typedef long             * LONG_PTR;
    typedef double           * DOUBLE_PTR;
    typedef void             * VOID_PTR;
#else
#ifndef PASCAL
#ifdef NULL
#undef NULL
#endif
#include <windows.h>
#endif
    typedef LPSTR        far * pLPSTR;
    typedef char         far * CHAR_PTR;
    typedef int          far * INT_PTR;
    typedef unsigned int far * UINT_PTR;
    typedef long         far * LONG_PTR;
    typedef double       far * DOUBLE_PTR;
    typedef void         far * VOID_PTR;
#endif

/** file definitions **/
#ifndef _WINDOWS
    /*** for non-WINDOWS ***/
    typedef struct
    {
	int handle; /* file handle */
    } * DBF; /* DBF (database) file pointer */

    typedef struct
    {
	int handle; /* file handle */
    } * DBT; /* DBT (memo) file pointer */

    typedef struct
    {
	int handle; /* file handle */
    } * MDX; /* MDX (multiple index) file pointer */

    typedef struct
    {
	int indexid;
    } * IDX; /* IDX (index in MDX) pointer */

    typedef struct
    {
	int handle; /* file handle */
    } * NDX; /* NDX (index) file pointer */
#else
    /*** for WINDOWS ***/
    typedef struct
    {
	int handle; /* file handle */
    } far * DBF; /* DBF (database) file pointer */

    typedef struct
    {
	int handle; /* file handle */
    } far * DBT; /* DBT (memo) file pointer */

    typedef struct
    {
	int handle; /* file handle */
    } far * MDX; /* MDX (multiple index) file pointer */

    typedef struct
    {
	int indexid;
    } far * IDX; /* IDX (index in MDX) pointer */

    typedef struct
    {
	int handle; /* file handle */
    } far * NDX; /* NDX (index) file pointer */
#endif

/* tag definition for MDX */
typedef struct
{
    int  tagid;	     /* tag id */
    char tagnm[11];  /* tag name */
    int  keytype;    /* key type */
} TAG;  /* tag definition */

#ifndef _WINDOWS
    typedef TAG * TAG_PTR;
#else
    typedef TAG far * TAG_PTR;
#endif

/***** File Open Mode Definitions *****/
/*
 * The following can be logically ORed.
 *	example)  d_READONLY | d_SHARED  -- read-only & shared mode
 */
#define d_SINGLE     0	/* Single User mode */
#define d_READONLY   1	/* Read-Only mode */
#define d_SHARED     2	/* General shared mode:
				All stations can read & write. */
#define d_SHARE_READ 4  /* Read-only shared mode:
                                This station can read;
				this station can write if not d_READONLY;
                                other stations can only read (d_READONLY).
			   Others should open by (d_SHARED | d_READONLY).
			 */

/* DBF file mode; used by dDchmod(); returned by dDmode() */
#ifndef d_READONLY
#define d_READONLY  0x01  /* read-only mode */
#endif
#define hasMDX      0x02  /* has an associated MDX file */
#define hasDBT4     0x04  /* has an associated DBT file of dBASE IV format */
#define hasDBT3     0x08  /* has an associated DBT file
				of dBASE III or dBASE III PLUS format */
#define isENCRYPTED 0x10  /* DBF file encrypted */


/************************************************
* return values from AccSys-for-dBASE functions *
*************************************************/
#define SUCCESS      0   /** successful function return code **/
#define dERROR      -1   /** general error code **/
			  /* This error code has been reserved for
                             very fundamental error or system error
                             detection */
#define dBADNAME    -2  /** bad file or field name **/

#define dBADFILE    -3  /** accessed file is not organized in the
                             correct file format **/

#define dOUTRANGE   -4  /** out of range error **/
		        /*  e.g., record number greater than
                            the number of allocated records */

#define dNOTFOUND   -5  /** requested object not found **/

#define dBOF        -6  /** beginning of index file condition **/
		        /*  pointer reached the top of index file */

#define dEOF        -7  /** end of index file condition **/
		        /*  pointer reached the bottom of
                            the index file */

#define dKEYVIOL    -8  /** key violation **/

#define dMISMATCH   -9  /** mismatch between index and data file **/
#define dNOOPEN    -10  /** file not open **/
#define dILLEGAL   -11  /** illegal request **/

#define dIOERR     -12  /** I/O error **/
#define dMEMERR    -13  /** memory allocation failure **/
#define dENCRYPT   -14  /** file encrypted **/
#define dRDONLY    -15  /** file in read-only mode **/

/****************************************************
 * function/variable prototypes of AccSys-for-dBASE *
 ****************************************************/
#ifndef ACDLL
extern char dversion[]; /* version information */
extern int d_report;    /* failure report number */
extern int dretcode;    /* error code from
                           dDopen, dXopen, dTopen, dNopen */
extern int d_blksiz;	/* block size for MDX and DBT: 1 through 32 */

extern unsigned short d_request; /* special request flag */
extern long d_recno;	/* record number of record just appended */

#endif /* <--- #ifndef ACDLL */

/*
 * The 'd_request' word is special request flag to AccSys functions.
 * Each bit, when it is set to 1, directs AccSys functions to perform
 * special requests.
 * (Bit 0 is the right most bit, can be set by hex number 0x01.)
 * 
 *   bit
 * position	
 * --------	
 *    0		Read or write a record without the first status byte.
 *		Applicable functions:
 *			dDgetrec, dDapprec, dDinsrec and dDupdrec.
 *		
 * 1 - 15	reserved for future use
 *
 */

/*
 * The following #define macro can be used to set a bit position defined
 * in the 'd_request' word.
 */
#define NO_STATUS 0x01 /* to read or write a record without the status byte */


/* flag constants to be used in the 'style' arguments of some dU functions */
#define MDXstyle     0
#define NDXstyle     1

/* global data structure for ACDLL */
#ifdef ACDLL
typedef struct gdat
{
	char dversion[16]; /* version control variable: was 1.02A */
	int  d_report;     /* AccSys/dBASE error reporter */
	int  dretcode;     /* error code from
	                        dDopen, dNopen, dXopen, dXactidx,
				dTopen, dTopen3 */
	unsigned short d_request;
	int d_blksiz;
	long d_recno;
	char _dtmpdir[85]; /* ACTMPLEN == 85: directory for temporary files */
	char _dminbuf[120];
#ifdef _WINDOWS
	char _dWINbuf[256];
#endif
} DBGDAT;
#define SZDBGDAT sizeof(DBGDAT);
typedef DBGDAT far * DBGDAT_PTR;

extern DBGDAT dbgdata;

#define dversion dbgdata.dversion
#define d_report dbgdata.d_report
#define dretcode dbgdata.dretcode
#define d_request dbgdata.d_request
#define d_blksiz  dbgdata.d_blksiz
#define d_recno	  dbgdata.d_recno
#define _dminbuf  dbgdata._dminbuf[120];
#define _dWINbuf  dbgdata._dWINbuf[256];
#endif /* <--- #ifdef ACDLL */


/* AccSys for dBASE C functions prototypes */
#ifdef __cplusplus
extern "C" {
#endif



#ifdef unix

int DECLARE dDapart(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname
#endif
	);

int DECLARE dDcat(
#ifdef ProtypesNeeded
	CHAR_PTR origdbf, CHAR_PTR adddbf
#endif
	);

int DECLARE dDcreat(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, int nofields, CHAR_PTR fields[]
#endif
	);

int DECLARE dDcmpstr(
#ifdef ProtypesNeeded
	CHAR_PTR dbf1, CHAR_PTR dbf2
#endif
	);

int DECLARE dDdate(
#ifdef ProtypesNeeded
	DBF dbfptr, INT_PTR month, INT_PTR day, INT_PTR year
#endif
	);

DBF DECLARE dDopen(
#ifdef ProtypesNeeded
	CHAR_PTR name, int mode, int buffs
#endif
	);

int DECLARE dDchmod(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, int mode
#endif
	);

int DECLARE dDmode(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname
#endif
	);

int DECLARE dDclose(
#ifdef ProtypesNeeded
	DBF dbfptr
#endif
	);

long DECLARE dDbuffs(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, int buffs
#endif
	);

int DECLARE dDflush(
#ifdef ProtypesNeeded
	DBF dbfptr
#endif
	);

int DECLARE dDinsrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno, CHAR_PTR record
#endif
	);

int DECLARE dDapprec(
#ifdef ProtypesNeeded
	DBF dbfptr, CHAR_PTR record
#endif
	);

int DECLARE dDreccnt(
#ifdef ProtypesNeeded
	DBF dbfptr, LONG_PTR reccnt
#endif
	);

#define dDsize dDreccnt /* compatibility with old function name */

int DECLARE dDupdrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno, CHAR_PTR record
#endif
	);

int DECLARE dDdelrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno
#endif
	);

int DECLARE dDrmrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno
#endif
	);

int DECLARE dDrclrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno
#endif
	);

int DECLARE dDrecsta(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno
#endif
	);

int DECLARE dDgetrec(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno, CHAR_PTR record
#endif
	);

int DECLARE dDfldno(
#ifdef ProtypesNeeded
	DBF dbfptr, int fieldpos, INT_PTR offset,
		   CHAR_PTR fieldnm, CHAR_PTR type, INT_PTR width, INT_PTR decimal
#endif
	);

int DECLARE dDfldnm(
#ifdef ProtypesNeeded
	DBF dbfptr, CHAR_PTR fieldnm, INT_PTR offset,
	INT_PTR fieldpos, CHAR_PTR type, INT_PTR width, INT_PTR decimal
#endif
	);

int DECLARE dDreclen(
#ifdef ProtypesNeeded
	DBF dbfptr
#endif
	);

int DECLARE dDfields(
#ifdef ProtypesNeeded
	DBF dbfptr
#endif
	);

int DECLARE dDcopy(
#ifdef ProtypesNeeded
	CHAR_PTR src, CHAR_PTR dest
#endif
	);

int DECLARE dDsort(
#ifdef ProtypesNeeded
	CHAR_PTR src, CHAR_PTR dest, int sortfields,
	CHAR_PTR specifier[]
#endif
	);

int DECLARE dNcreat(
#ifdef ProtypesNeeded
	CHAR_PTR ndxname, CHAR_PTR keyexpr, CHAR_PTR typelen
#endif
	);

int DECLARE dNindex(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, CHAR_PTR ndxname, CHAR_PTR kexpr,
	CHAR_PTR typelen, int unique,
	int (*keygen)(CHAR_PTR record, CHAR_PTR key)
#endif
	);

int DECLARE dNcopy(
#ifdef ProtypesNeeded
	CHAR_PTR src, CHAR_PTR dest
#endif
	);

NDX DECLARE dNopen(
#ifdef ProtypesNeeded
	CHAR_PTR ndxname, int mode, int buffs
#endif
	);

void DECLARE dN3datek(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dNclose(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

long DECLARE dNbuffs(
#ifdef ProtypesNeeded
	CHAR_PTR ndxname, int buffs
#endif
	);

int DECLARE dNkeylen(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dNkeytyp(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dNexpr(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR kexpr, INT_PTR unique
#endif
	);

int DECLARE dNexplen(
#ifdef ProtypesNeeded
	NDX ndxptr, INT_PTR length
#endif
	);

int DECLARE dNgetrno(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dNputkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, long recno
#endif
	);

int DECLARE dNrmkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, long recno
#endif
	);

int DECLARE dNupdkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno
#endif
	);

int DECLARE dNnxtkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dNprvkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dNcurkey(
#ifdef ProtypesNeeded
	NDX ndxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dNrewind(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dNforwrd(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dNflush(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

int DECLARE dTchkmm(
#ifdef ProtypesNeeded
	CHAR_PTR memofield
#endif
	);

int DECLARE dTcreat(
#ifdef ProtypesNeeded
	CHAR_PTR dbtname
#endif
	);

DBT DECLARE dTopen(
#ifdef ProtypesNeeded
	CHAR_PTR dbtname, int mode
#endif
	);

int DECLARE dTclose(
#ifdef ProtypesNeeded
	DBT dbtptr
#endif
	);

long DECLARE dTmemuse(
#ifdef ProtypesNeeded
	CHAR_PTR dbtname
#endif
	);

int DECLARE dTnewmm(
#ifdef ProtypesNeeded
	DBT dbtptr, long length, CHAR_PTR memobuff, CHAR_PTR memofield
#endif
	);

int DECLARE dTaddmm(
#ifdef ProtypesNeeded
	DBT dbtptr, long length, CHAR_PTR memobuff,
	CHAR_PTR memofield, INT_PTR setnew
#endif
	);

int DECLARE dTupdmm(
#ifdef ProtypesNeeded
	DBT dbtptr, long length, CHAR_PTR memobuff,
	CHAR_PTR memofield, INT_PTR setnew
#endif
	);

int DECLARE dTgetmm(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield, long offset, long length,
	CHAR_PTR memobuff
#endif
	);

int DECLARE dTmemosz(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield, LONG_PTR size
#endif
	);

int DECLARE dTcreat3(
#ifdef ProtypesNeeded
	CHAR_PTR dbtname
#endif
	);

DBT DECLARE dTopen3(
#ifdef ProtypesNeeded
	CHAR_PTR dbtname, int mode
#endif
	);

int DECLARE dTclose3(
#ifdef ProtypesNeeded
	DBT dbtptr
#endif
	);

int DECLARE dTputmm3(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memobuff, CHAR_PTR memofield
#endif
	);

int DECLARE dTgetmm3(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield, CHAR_PTR memobuff
#endif
	);

int DECLARE dTmemsz3(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield, LONG_PTR size
#endif
	);

int  DECLARE dU3itodf(
#ifdef ProtypesNeeded
	int month, int day, int year, CHAR_PTR dfield
#endif
	);

int  DECLARE dU3itodk(
#ifdef ProtypesNeeded
	int month, int day, int year, CHAR_PTR datekey
#endif
	);

void DECLARE dUatocf(
#ifdef ProtypesNeeded
	CHAR_PTR string, int width, CHAR_PTR cfield
#endif
	);

int  DECLARE dUatonf(
#ifdef ProtypesNeeded
	CHAR_PTR string, int width, int decimal, CHAR_PTR nfield
#endif
	);

void DECLARE dUdfto3i(
#ifdef ProtypesNeeded
	CHAR_PTR dfield, INT_PTR month, INT_PTR day, INT_PTR year
#endif
	);

int  DECLARE dUdftodk(
#ifdef ProtypesNeeded
	CHAR_PTR dfield, CHAR_PTR datekey
#endif
	);

void DECLARE dUdkto3i(
#ifdef ProtypesNeeded
	CHAR_PTR datekey, INT_PTR month, INT_PTR day, INT_PTR year
#endif
	);

int  DECLARE dUdtonf(
#ifdef ProtypesNeeded
	double number, int width, int decimal, CHAR_PTR nfield
#endif
	);

void DECLARE dUdtonk(
#ifdef ProtypesNeeded
	int style, double number, CHAR_PTR key
#endif
	);

void DECLARE dUexpnm(
#ifdef ProtypesNeeded
	CHAR_PTR orig, CHAR_PTR add, CHAR_PTR result
#endif
	);

void DECLARE dUinitmf(
#ifdef ProtypesNeeded
	CHAR_PTR memofield
#endif
	);

int  DECLARE dUleap(
#ifdef ProtypesNeeded
	int year
#endif
	);

double DECLARE dUnftod(
#ifdef ProtypesNeeded
	CHAR_PTR nfield, int width
#endif
	);

int DECLARE dUnftonk(
#ifdef ProtypesNeeded
	int style, CHAR_PTR nfield, int width, CHAR_PTR key
#endif
	);

int DECLARE dUnktoa(
#ifdef ProtypesNeeded
	int style, CHAR_PTR key, int width, int decimal,
		    CHAR_PTR string
#endif
	);

double DECLARE dUnktod(
#ifdef ProtypesNeeded
	int style, CHAR_PTR key
#endif
	);

void DECLARE dUtoday(
#ifdef ProtypesNeeded
	INT_PTR month, INT_PTR day, INT_PTR year
#endif
	);

int DECLARE dUtmpdir(
#ifdef ProtypesNeeded
	CHAR_PTR tempdir
#endif
	);

IDX DECLARE dXactidx(
#ifdef ProtypesNeeded
	MDX mdxptr, CHAR_PTR tagname
#endif
	);

int DECLARE dXaddtag(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname,
	CHAR_PTR iexpr, CHAR_PTR typelen, CHAR_PTR FORcond
#endif
	);

long DECLARE dXbuffs(
#ifdef ProtypesNeeded
	CHAR_PTR mdxname, int buffs
#endif
	);

int DECLARE dXclose(
#ifdef ProtypesNeeded
	MDX mdxptr
#endif
	);

int DECLARE dXcopy(
#ifdef ProtypesNeeded
	CHAR_PTR src, CHAR_PTR dest
#endif
	);

int DECLARE dXcurkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dXdeaidx(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int DECLARE dXexplen(
#ifdef ProtypesNeeded
	MDX mdxptr, CHAR_PTR tagname, INT_PTR length
#endif
	);

int DECLARE dXexpr(
#ifdef ProtypesNeeded
	MDX mdxptr, CHAR_PTR tagname, CHAR_PTR iexpr,
	CHAR_PTR FORcond, INT_PTR  unique, CHAR_PTR order
#endif
	);

int DECLARE dXezindx(
#ifdef ProtypesNeeded
	CHAR_PTR name, int fieldno, int orderuniq
#endif
	);

int DECLARE dXflush(
#ifdef ProtypesNeeded
	MDX mdxptr
#endif
	);

int DECLARE dXforwrd(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int DECLARE dXgetrno(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

long DECLARE dXidxbuf(
#ifdef ProtypesNeeded
	MDX mdxptr, CHAR_PTR tagname
#endif
	);

int DECLARE dXindex(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname, CHAR_PTR iexpr,
	CHAR_PTR typelen, int unique,
	int (*keygen)(CHAR_PTR record, CHAR_PTR key),
	CHAR_PTR FORcond,
	int (*condcheck)(CHAR_PTR record)
#endif
	);

int DECLARE dXkeytyp(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int DECLARE dXkeylen(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int DECLARE dXlistag(
#ifdef ProtypesNeeded
	MDX mdxptr, TAG_PTR tagptr
#endif
	);

int DECLARE dXnxtkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

MDX DECLARE dXopen(
#ifdef ProtypesNeeded
	CHAR_PTR mdxname, int mode, int buffs
#endif
	);

int DECLARE dXprvkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, LONG_PTR recno
#endif
	);

int DECLARE dXputkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, long recno
#endif
	);

int DECLARE dXrewind(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int DECLARE dXrename(
#ifdef ProtypesNeeded
	CHAR_PTR oldname, CHAR_PTR newname
#endif
	);

int DECLARE dXrmkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR key, long recno
#endif
	);

int DECLARE dXrmtag(
#ifdef ProtypesNeeded
	CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname
#endif
	);

int DECLARE dXsortag(
#ifdef ProtypesNeeded
	MDX mdxptr, TAG_PTR tagptr
#endif
	);

int DECLARE dXtags(
#ifdef ProtypesNeeded
	MDX mdxptr
#endif
	);

int DECLARE dXupdkey(
#ifdef ProtypesNeeded
	IDX idxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno
#endif
	);


#else  /* unix */

#ifndef ACDLL

int DECLARE dDapart(CHAR_PTR dbfname);

int DECLARE dDcat(CHAR_PTR origdbf, CHAR_PTR adddbf);

#ifndef _WINDOWS
int DECLARE dDcreat(CHAR_PTR dbfname, int nofields, CHAR_PTR fields[]);
#else
int DECLARE dDcreat(LPSTR dbfname, int nofields, pLPSTR fields);
#endif
int DECLARE dDcmpstr(CHAR_PTR dbf1, CHAR_PTR dbf2);
int DECLARE dDdate(DBF dbfptr, INT_PTR month, INT_PTR day, INT_PTR year);
DBF DECLARE dDopen(CHAR_PTR name, int mode, int buffs);
int DECLARE dDchmod(CHAR_PTR dbfname, int mode);
int DECLARE dDmode(CHAR_PTR dbfname);
int DECLARE dDclose(DBF dbfptr);
long DECLARE dDbuffs(CHAR_PTR dbfname, int buffs);
int DECLARE dDflush(DBF dbfptr);
int DECLARE dDinsrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDapprec(DBF dbfptr, CHAR_PTR record);
int DECLARE dDreccnt(DBF dbfptr, LONG_PTR reccnt);
#define dDsize dDreccnt /* compatibility with old function name */
int DECLARE dDupdrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDdelrec(DBF dbfptr, long recno);
int DECLARE dDrmrec(DBF dbfptr, long recno);
int DECLARE dDrclrec(DBF dbfptr, long recno);
int DECLARE dDrecsta(DBF dbfptr, long recno);
int DECLARE dDgetrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDfldno(DBF dbfptr, int fieldpos, INT_PTR offset,
		   CHAR_PTR fieldnm, CHAR_PTR type, INT_PTR width, INT_PTR decimal);
int DECLARE dDfldnm(DBF dbfptr, CHAR_PTR fieldnm, INT_PTR offset,
		   INT_PTR fieldpos, CHAR_PTR type, INT_PTR width, INT_PTR decimal);
int DECLARE dDreclen(DBF dbfptr);
int DECLARE dDfields(DBF dbfptr);
int DECLARE dDcopy(CHAR_PTR src, CHAR_PTR dest);
#ifndef _WINDOWS
int DECLARE dDsort(CHAR_PTR src, CHAR_PTR dest, int sortfields,
		  CHAR_PTR specifier[]);
#else
int DECLARE dDsort(LPSTR src, LPSTR dest, int sortfields,
		  pLPSTR specifier);
#endif

int DECLARE dNcreat(CHAR_PTR ndxname, CHAR_PTR keyexpr, CHAR_PTR typelen);
int DECLARE dNindex(CHAR_PTR dbfname, CHAR_PTR ndxname, CHAR_PTR kexpr,
		   CHAR_PTR typelen, int unique,
		   int (*keygen)(CHAR_PTR record, CHAR_PTR key));
int DECLARE dNcopy(CHAR_PTR src, CHAR_PTR dest);
NDX DECLARE dNopen(CHAR_PTR ndxname, int mode, int buffs);
void DECLARE dN3datek(NDX ndxptr);
int DECLARE dNclose(NDX ndxptr);
long DECLARE dNbuffs(CHAR_PTR ndxname, int buffs);
int DECLARE dNkeylen(NDX ndxptr);
int DECLARE dNkeytyp(NDX ndxptr);
int DECLARE dNexpr(NDX ndxptr, CHAR_PTR kexpr, INT_PTR unique);
int DECLARE dNexplen(NDX ndxptr, INT_PTR length);
int DECLARE dNgetrno(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNputkey(NDX ndxptr, CHAR_PTR key, long recno);
int DECLARE dNrmkey(NDX ndxptr, CHAR_PTR key, long recno);
int DECLARE dNupdkey(NDX ndxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno);
int DECLARE dNnxtkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNprvkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNcurkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNrewind(NDX ndxptr);
int DECLARE dNforwrd(NDX ndxptr);
int DECLARE dNflush(NDX ndxptr);
int DECLARE dTchkmm(CHAR_PTR memofield);
int DECLARE dTcreat(CHAR_PTR dbtname);
DBT DECLARE dTopen(CHAR_PTR dbtname, int mode);
int DECLARE dTclose(DBT dbtptr);
long DECLARE dTmemuse(CHAR_PTR dbtname);
int DECLARE dTnewmm(DBT dbtptr, long length, CHAR_PTR memobuff, CHAR_PTR memofield);
int DECLARE dTaddmm(DBT dbtptr, long length, CHAR_PTR memobuff,
			CHAR_PTR memofield, INT_PTR setnew);
int DECLARE dTupdmm(DBT dbtptr, long length, CHAR_PTR memobuff,
			CHAR_PTR memofield, INT_PTR setnew);
int DECLARE dTgetmm(DBT dbtptr, CHAR_PTR memofield, long offset, long length,
		   CHAR_PTR memobuff);
int DECLARE dTmemosz(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int DECLARE dTcreat3(CHAR_PTR dbtname);
DBT DECLARE dTopen3(CHAR_PTR dbtname, int mode);
int DECLARE dTclose3(DBT dbtptr);
int DECLARE dTputmm3(DBT dbtptr, CHAR_PTR memobuff, CHAR_PTR memofield);
int DECLARE dTgetmm3(DBT dbtptr, CHAR_PTR memofield, CHAR_PTR memobuff);
int DECLARE dTmemsz3(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int  DECLARE dU3itodf(int month, int day, int year, CHAR_PTR dfield);
int  DECLARE dU3itodk(int month, int day, int year, CHAR_PTR datekey);
void DECLARE dUatocf(CHAR_PTR string, int width, CHAR_PTR cfield);
int  DECLARE dUatonf(CHAR_PTR string, int width, int decimal, CHAR_PTR nfield);
void DECLARE dUdfto3i(CHAR_PTR dfield, INT_PTR month, INT_PTR day, INT_PTR year);
int  DECLARE dUdftodk(CHAR_PTR dfield, CHAR_PTR datekey);
void DECLARE dUdkto3i(CHAR_PTR datekey, INT_PTR month, INT_PTR day, INT_PTR year);
int  DECLARE dUdtonf(double number, int width, int decimal, CHAR_PTR nfield);
void DECLARE dUdtonk(int style, double number, CHAR_PTR key);
void DECLARE dUexpnm(CHAR_PTR orig, CHAR_PTR add, CHAR_PTR result);
void DECLARE dUinitmf(CHAR_PTR memofield);
int  DECLARE dUleap(int year);
double DECLARE dUnftod(CHAR_PTR nfield, int width);
int DECLARE dUnftonk(int style, CHAR_PTR nfield, int width, CHAR_PTR key);
int DECLARE dUnktoa(int style, CHAR_PTR key, int width, int decimal,
		    CHAR_PTR string);
double DECLARE dUnktod(int style, CHAR_PTR key);
void DECLARE dUtoday(INT_PTR month, INT_PTR day, INT_PTR year);
int DECLARE dUtmpdir(CHAR_PTR tempdir);
IDX DECLARE dXactidx(MDX mdxptr, CHAR_PTR tagname);
int DECLARE dXaddtag(CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname,
		    CHAR_PTR iexpr, CHAR_PTR typelen, CHAR_PTR FORcond);
long DECLARE dXbuffs(CHAR_PTR mdxname, int buffs);
int DECLARE dXclose(MDX mdxptr);
int DECLARE dXcopy(CHAR_PTR src, CHAR_PTR dest);
int DECLARE dXcurkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dXdeaidx(IDX idxptr);
int DECLARE dXexplen(MDX mdxptr, CHAR_PTR tagname, INT_PTR length);
int DECLARE dXexpr(MDX mdxptr, CHAR_PTR tagname, CHAR_PTR iexpr,
		   CHAR_PTR FORcond, INT_PTR  unique, CHAR_PTR order);
int DECLARE dXezindx(CHAR_PTR name, int fieldno, int orderuniq);
int DECLARE dXflush(MDX mdxptr);
int DECLARE dXforwrd(IDX idxptr);
int DECLARE dXgetrno(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
long DECLARE dXidxbuf(MDX mdxptr, CHAR_PTR tagname);
int DECLARE dXindex(CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname, CHAR_PTR iexpr,
		   CHAR_PTR typelen, int unique,
		   int (*keygen)(CHAR_PTR record, CHAR_PTR key),
		   CHAR_PTR FORcond,
		   int  (*condcheck)(CHAR_PTR record));
int DECLARE dXkeytyp(IDX idxptr);
int DECLARE dXkeylen(IDX idxptr);
int DECLARE dXlistag(MDX mdxptr, TAG_PTR tagptr);
int DECLARE dXnxtkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
MDX DECLARE dXopen(CHAR_PTR mdxname, int mode, int buffs);
int DECLARE dXprvkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dXputkey(IDX idxptr, CHAR_PTR key, long recno);
int DECLARE dXrewind(IDX idxptr);
int DECLARE dXrename(CHAR_PTR oldname, CHAR_PTR newname);
int DECLARE dXrmkey(IDX idxptr, CHAR_PTR key, long recno);
int DECLARE dXrmtag(CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname);
int DECLARE dXsortag(MDX mdxptr, TAG_PTR tagptr);
int DECLARE dXtags(MDX mdxptr);
int DECLARE dXupdkey(IDX idxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno);

#else  /* ACDLL macros */

int DECLARE DdDapart(DBGDAT_PTR dbgptr, CHAR_PTR dbfname);
#define dDapart(a) DdDapart((DBGDAT_PTR) &dbgdata, a)
int DECLARE DdDcat(DBGDAT_PTR dbgptr, CHAR_PTR origdbf, CHAR_PTR adddbf);
#define dDcat(a, b) DdDcat((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE DdDcreat(DBGDAT_PTR dbgptr, LPSTR dbfname, int nofields, pLPSTR fields);
#define dDcreat(a, b, c) DdDcreat((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE DdDcopy(DBGDAT_PTR dbgptr, CHAR_PTR src, CHAR_PTR dest);
#define dDcopy(a, b) DdDcopy((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE DdDcmpstr(DBGDAT_PTR dbgptr, CHAR_PTR dbf1, CHAR_PTR dbf2);
#define dDcmpstr(a, b, c) DdDcmpstr((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE dDdate(DBF dbfptr, INT_PTR month, INT_PTR day, INT_PTR year);
DBF DECLARE DdDopen(DBGDAT_PTR dbgptr, CHAR_PTR name, int mode, int buffs);
#define dDopen(a, b, c) DdDopen((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE DdDchmod(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, int mode);
#define dDchmod(a, b) DdDchmod((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE DdDmode(DBGDAT_PTR dbgptr, CHAR_PTR dbfname);
#define dDmode(a) DdDmode((DBGDAT_PTR) &dbgdata, a)
int DECLARE dDclose(DBF dbfptr);
long DECLARE DdDbuffs(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, int buffs);
#define dDbuffs(a, b) DdDbuffs((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dDflush(DBF dbfptr);
int DECLARE dDinsrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDapprec(DBF dbfptr, CHAR_PTR record);
int DECLARE dDreccnt(DBF dbfptr, LONG_PTR reccnt);
#define dDsize dDreccnt /* compatibility with old function name */
int DECLARE dDupdrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDdelrec(DBF dbfptr, long recno);
int DECLARE dDrmrec(DBF dbfptr, long recno);
int DECLARE dDrclrec(DBF dbfptr, long recno);
int DECLARE dDrecsta(DBF dbfptr, long recno);
int DECLARE dDgetrec(DBF dbfptr, long recno, CHAR_PTR record);
int DECLARE dDfldno(DBF dbfptr, int fieldpos, INT_PTR offset,
		   CHAR_PTR fieldnm, CHAR_PTR type, INT_PTR width, INT_PTR decimal);
int DECLARE dDfldnm(DBF dbfptr, CHAR_PTR fieldnm, INT_PTR offset,
		   INT_PTR fieldpos, CHAR_PTR type, INT_PTR width, INT_PTR decimal);
int DECLARE dDreclen(DBF dbfptr);
int DECLARE dDfields(DBF dbfptr);
int DECLARE DdDsort(DBGDAT_PTR dbgptr, LPSTR src, LPSTR dest, int sortfields,
		  pLPSTR specifier);
#define dDsort(a,b,c,d) DdDsort((DBGDAT_PTR) &dbgdata,a,b,c,d)
int DECLARE DdNcreat(DBGDAT_PTR dbgptr, CHAR_PTR ndxname, CHAR_PTR keyexpr, CHAR_PTR typelen);
#define dNcreat(a, b, c) DdNcreat((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE DdNindex(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, CHAR_PTR ndxname,
		CHAR_PTR kexpr, CHAR_PTR typelen, int unique,
		   int (*keygen)(CHAR_PTR record, CHAR_PTR key));
#define dNindex(a,b,c,d,e,f) DdNindex((DBGDAT_PTR) &dbgdata,a,b,c,d,e,f)
int DECLARE DdNcopy(DBGDAT_PTR dbgptr, CHAR_PTR src, CHAR_PTR dest);
#define dNcopy(a,b) DdNcopy((DBGDAT_PTR) &dbgdata, a,b)
NDX DECLARE DdNopen(DBGDAT_PTR dbgptr, CHAR_PTR ndxname, int mode, int buffs);
#define dNopen(a, b, c) DdNopen((DBGDAT_PTR) &dbgdata, a, b, c)
void DECLARE dN3datek(NDX ndxptr);
int DECLARE dNclose(NDX ndxptr);
long DECLARE DdNbuffs(DBGDAT_PTR dbgptr, CHAR_PTR ndxname, int buffs);
#define dNbuffs(a, b) DdNbuffs((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dNkeylen(NDX ndxptr);
int DECLARE dNkeytyp(NDX ndxptr);
int DECLARE dNexpr(NDX ndxptr, CHAR_PTR kexpr, INT_PTR unique);
int DECLARE dNexplen(NDX ndxptr, INT_PTR length);
int DECLARE dNgetrno(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNputkey(NDX ndxptr, CHAR_PTR key, long recno);
int DECLARE dNrmkey(NDX ndxptr, CHAR_PTR key, long recno);
int DECLARE dNupdkey(NDX ndxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno);
int DECLARE dNnxtkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNprvkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNcurkey(NDX ndxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dNrewind(NDX ndxptr);
int DECLARE dNforwrd(NDX ndxptr);
int DECLARE dNflush(NDX ndxptr);
int DECLARE dTchkmm(CHAR_PTR memofield);
int DECLARE DdTcreat(DBGDAT_PTR dbgptr, CHAR_PTR dbtname);
#define dTcreat(a) DdTcreat((DBGDAT_PTR) &dbgdata, a)
DBT DECLARE DdTopen(DBGDAT_PTR dbgptr, CHAR_PTR dbtname, int mode);
#define dTopen(a, b) DdTopen((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dTclose(DBT dbtptr);
long DECLARE DdTmemuse(DBGDAT_PTR dbgptr, CHAR_PTR dbtname);
#define dTmemuse(a) DdTmemuse((DBGDAT_PTR) &dbgdata, a)
int DECLARE dTnewmm(DBT dbtptr, long length, CHAR_PTR memobuff, CHAR_PTR memofield);
int DECLARE dTaddmm(DBT dbtptr, long length, CHAR_PTR memobuff,
			CHAR_PTR memofield, INT_PTR setnew);
int DECLARE dTupdmm(DBT dbtptr, long length, CHAR_PTR memobuff,
			CHAR_PTR memofield, INT_PTR setnew);
int DECLARE dTgetmm(DBT dbtptr, CHAR_PTR memofield, long offset, long length,
		   CHAR_PTR memobuff);
int DECLARE dTmemosz(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int DECLARE DdTcreat3(DBGDAT_PTR dbgptr, CHAR_PTR dbtname);
#define dTcreat3(a) DdTcreat3((DBGDAT_PTR) &dbgdata, a)
DBT DECLARE DdTopen3(DBGDAT_PTR dbgptr, CHAR_PTR dbtname, int mode);
#define dTopen3(a, b) DdTopen3((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dTclose3(DBT dbtptr);
int DECLARE dTputmm3(DBT dbtptr, CHAR_PTR memobuff, CHAR_PTR memofield);
int DECLARE dTgetmm3(DBT dbtptr, CHAR_PTR memofield, CHAR_PTR memobuff);
int DECLARE dTmemsz3(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int  DECLARE DdU3itodf(DBGDAT_PTR dbgptr, int month, int day, int year, CHAR_PTR dfield);
#define dU3itodf(a, b, c, d) DdU3itodf((DBGDAT_PTR) &dbgdata, a, b, c, d)
int  DECLARE DdU3itodk(DBGDAT_PTR dbgptr, int month, int day, int year, CHAR_PTR datekey);
#define dU3itodk(a, b, c, d) DdU3itodk((DBGDAT_PTR) &dbgdata, a, b, c, d)
void DECLARE dUatocf(CHAR_PTR string, int width, CHAR_PTR cfield);
int  DECLARE DdUatonf(DBGDAT_PTR dbgptr, CHAR_PTR string, int width, int decimal, CHAR_PTR nfield);
#define dUatonf(a, b, c, d) DdUatonf((DBGDAT_PTR) &dbgdata, a, b, c, d)
void DECLARE dUdfto3i(CHAR_PTR dfield, INT_PTR month, INT_PTR day, INT_PTR year);
int  DECLARE DdUdftodk(DBGDAT_PTR dbgptr, CHAR_PTR dfield, CHAR_PTR datekey);
#define dUdftodk(a, b) DdUdftodk((DBGDAT_PTR) &dbgdata, a, b)
void DECLARE dUdkto3i(CHAR_PTR datekey, INT_PTR month, INT_PTR day, INT_PTR year);
int  DECLARE DdUdtonf(DBGDAT_PTR dbgptr, double number, int width, int decimal, CHAR_PTR nfield);
#define dUdtonf(a, b, c, d) DdUdtonf((DBGDAT_PTR) &dbgdata, a, b, c, d)
void DECLARE dUdtonk(int style, double number, CHAR_PTR key);
void DECLARE dUexpnm(CHAR_PTR orig, CHAR_PTR add, CHAR_PTR result);
void DECLARE dUinitmf(CHAR_PTR memofield);
int  DECLARE dUleap(int year);
double DECLARE dUnftod(CHAR_PTR nfield, int width);
int DECLARE DdUnftonk(DBGDAT_PTR dbgptr, int style, CHAR_PTR nfield, int width, CHAR_PTR key);
#define dUnftonk(a, b, c, d) DdUnftonk((DBGDAT_PTR) &dbgdata, a, b, c, d)
int DECLARE DdUnktoa(DBGDAT_PTR dbgptr, int style, CHAR_PTR key, int width, int decimal,
		    CHAR_PTR string);
#define dUnktoa(a, b, c, d, e) DdUnktoa((DBGDAT_PTR) &dbgdata, a, b, c, d, e)
double DECLARE dUnktod(int style, CHAR_PTR key);
void DECLARE dUtoday(INT_PTR month, INT_PTR day, INT_PTR year);
int DECLARE DdUtmpdir(DBGDAT_PTR dbgptr, CHAR_PTR tempdir);
#define dUtmpdir(a) DdUtmpdir((DBGDAT_PTR) &dbgdata, a)
IDX DECLARE dXactidx(MDX mdxptr, CHAR_PTR tagname);
int DECLARE DdXaddtag(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname,
		    CHAR_PTR iexpr, CHAR_PTR typelen, CHAR_PTR FORcond);
#define dXaddtag(a,b,c,d,e,f) DdXaddtag((DBGDAT_PTR) &dbgdata, a,b,c,d,e,f)
long DECLARE DdXbuffs(DBGDAT_PTR dbgptr, CHAR_PTR mdxname, int buffs);
#define dXbuffs(a, b) DdXbuffs((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dXclose(MDX mdxptr);
int DECLARE DdXcopy(DBGDAT_PTR dbgptr, CHAR_PTR src, CHAR_PTR dest);
#define dXcopy(a, b) DdXcopy((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dXcurkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dXdeaidx(IDX idxptr);
int DECLARE dXexplen(MDX mdxptr, CHAR_PTR tagname, INT_PTR length);
int DECLARE dXexpr(MDX mdxptr, CHAR_PTR tagname, CHAR_PTR iexpr,
		   CHAR_PTR FORcond, INT_PTR  unique, CHAR_PTR order);
int DECLARE DdXezindx(DBGDAT_PTR dbgptr, CHAR_PTR name, int fieldno,
		int orderuniq);
#define dXezindx(a, b, c) DdXezindx((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE dXflush(MDX mdxptr);
int DECLARE dXforwrd(IDX idxptr);
int DECLARE dXgetrno(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
long DECLARE dXidxbuf(MDX mdxptr, CHAR_PTR tagname);
int DECLARE DdXindex(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, CHAR_PTR mdxname,
		CHAR_PTR tagname, CHAR_PTR iexpr, CHAR_PTR typelen,
		int unique, int (*keygen)(CHAR_PTR record, CHAR_PTR key),
		CHAR_PTR FORcond,
		int  (*condcheck)(CHAR_PTR record));
#define dXindex(a,b,c,d,e,f,g,h,i) DdXindex((DBGDAT_PTR) &dbgdata,a,b,c,d,e,f,g,h,i)
int DECLARE dXkeytyp(IDX idxptr);
int DECLARE dXkeylen(IDX idxptr);
int DECLARE dXlistag(MDX mdxptr, TAG_PTR tagptr);
int DECLARE dXnxtkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
MDX DECLARE DdXopen(DBGDAT_PTR dbgptr, CHAR_PTR mdxname, int mode, int buffs);
#define dXopen(a, b, c) DdXopen((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE dXprvkey(IDX idxptr, CHAR_PTR key, LONG_PTR recno);
int DECLARE dXputkey(IDX idxptr, CHAR_PTR key, long recno);
int DECLARE dXrewind(IDX idxptr);
int DECLARE DdXrename(DBGDAT_PTR dbgptr, CHAR_PTR oldname, CHAR_PTR newname);
#define dXrename(a, b) DdXrename((DBGDAT_PTR) &dbgdata, a, b)
int DECLARE dXrmkey(IDX idxptr, CHAR_PTR key, long recno);
int DECLARE DdXrmtag(DBGDAT_PTR dbgptr, CHAR_PTR dbfname, CHAR_PTR mdxname, CHAR_PTR tagname);
#define dXrmtag(a, b, c) DdXrmtag((DBGDAT_PTR) &dbgdata, a, b, c)
int DECLARE dXsortag(MDX mdxptr, TAG_PTR tagptr);
int DECLARE dXtags(MDX mdxptr);
int DECLARE dXupdkey(IDX idxptr, CHAR_PTR oldkey, CHAR_PTR newkey, long recno);
#endif /* end of ACDLL macros */

#endif /* unix */




#ifdef __cplusplus
}
#endif

#define AccDB4 /* AccDB4 definition flag is on */

#endif
