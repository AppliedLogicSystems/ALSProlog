/**************
 * dZinclud.h *
 **************/
/****************************************************************************
*
* Copyright (c) 1989 - 1991, Billy Shakespeare & Company, Inc.
* All rights reserved.
*
*****************************************************************************
*
* Published by Copia International, Inc.
*
****************************************************************************/


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

#ifndef _WINDOWS
typedef char * UPTR;
typedef unsigned char * UCHAR_PTR;
#else
    #ifndef PASCAL
	#ifdef NULL
	    #undef NULL
	#endif
	#include <windows.h>
    #endif
typedef char far * UPTR;
typedef unsigned char far * UCHAR_PTR;
#endif

/* operation code */
#define OP0  0 /* DBF      => DBF 			 */
#define OP1  1 /* DBF      => MDX			 */
#define OP2  2 /* DBF	   => NDX			 */

#define K64 65536
#define TOUTMSIZ 12288
#define XNNXLEN 8 /* extra bytes in Xnn record */

#define dSINGLE SH_DENYRW

#define NULINDEX 0xff
#define OK    0
#define EMPTY 1

typedef struct leaf
{
    unsigned char nonleaf;   /* index to parent node in nonleaf table */

    CHAR_PTR	  leafmem;  /* memory ptr for this leaf */
    unsigned int  msize;     /* memory size used for this leaf */
    unsigned int  items;     /* number of items within this leaf memory */

    CHAR_PTR 	  valptr;   /* ptr to next item */

    /* information needed for external sorting */
    long 	  blkloc;    /* block location */
    long	  blksize;   /* size of this block in bytes */
} LEAF;
#ifndef _WINDOWS
typedef LEAF     *LEAFPTR;
#else
typedef LEAF far *LEAFPTR;
#endif

typedef struct nlf
{
#ifndef _WINDOWS
    struct nlf      *parent; /* ptr to parent node in nonleaf table */
#else
    struct nlf  far *parent; /* ptr to parent node in nonleaf table */
#endif
    LEAFPTR 	  loser;  /* ptr to loser leaf node */

    unsigned char left;    /* index to left branch */
    unsigned char right;   /* index to right branch */
    LEAFPTR	  winner; /* ptr to winner leaf node */
} NONLEAF;
#ifndef _WINDOWS
typedef NONLEAF     *NONLEAFPTR;
#else
typedef NONLEAF far *NONLEAFPTR;
#endif


#define K 8
extern LEAF leafs[K];
extern NONLEAF nonleafs[];
#define ROOT    (nonleafs[6])
#define WINVAL  ((ROOT.winner)->valptr)
#define WINLEAF (ROOT.winner)

typedef struct lvl
{
    long elmleft;
    CHAR_PTR buffer;
    unsigned int nodesz;
    CHAR_PTR bufptr;
    int  count;
    long curnodenum;
} LEVEL;
#ifndef _WINDOWS
typedef LEVEL     *PLEVEL;
#else
typedef LEVEL far *PLEVEL;
#endif

typedef struct gdata
{
int  _action;   /* copy of opcode */
char _origtemp[80];
char _sminbuf[256];
UPTR _infilep;  /* either for table, primary idx or sec. idx */
UPTR _outfilep; /* either for table, primary idx or sec. idx */
int  _indbfd; /* for non-.DB (OP2 or OP3) */
/* primary key info */
int _keys;     /* # of keys or key number */
int _copykeys; /* copy of 'keys' */
int _complen;  /* length to compare */
int _compofst;  /* offset to first key of comparison */
int _AsDecend;  /* order of comparison: Ascending or Descending */
int _keytype;  /* key type: 'C', 'M', 'N', or 'T' as defined above */
int _uniq;     /* indexing : 'U'nique or 'D'uplicate keys allowed */
int _uniquniq;
char _uniquebuf[101];
CHAR_PTR _nextsptr; /* next position in sort-qualifier */
int _itemlen;    /* item length */
long _tblcount;    /* # of items to sort / index */
int _temp1fd, _temp2fd;
char _temp1nm[ACTMPLEN];
char _temp2nm[ACTMPLEN];
char _tempnm[ACTMPLEN];
char _tagnm[20];
IDX  _idxpointer;

/* from _dzuitl.c */
CHAR_PTR  _sortbuf;
pCHAR_PTR _sortptr;

/* from _dzindex.c */
LEVEL _vector[32];
PLEVEL _vanchor;
long _leafblks; /* # of leaf blocks */
int  _maxitems; /* # of maximum keys in a leaf node */
int  _skeylen;  /* length of system key */
long _hdrnode;
long _initnode; /* initial node */
long _curnode;  /* current node */
long _lastnode;
long _usednodes; /* # of nodes used */
long _initeof;   /* initial eof node number */
AVAILIST _freelist;
int _blksz;

/* from _dzmerge.c */
LEAF _leafs[K];

/* for debugging */
int _debugi, _ddd;
CHAR_PTR _debugp;
} SORTGDAT;
#define SZSORTGDAT sizeof(SORTGDAT)
#ifndef _WINDOWS
typedef SORTGDAT     *PSORTGDAT;
#else
typedef SORTGDAT far *PSORTGDAT;
#endif

#define action   sgp->_action
#define origtemp sgp->_origtemp
#define sminbuf  sgp->_sminbuf
#define infilep  sgp->_infilep
#define outfilep sgp->_outfilep
#define indbfd   sgp->_indbfd
#define keys     sgp->_keys
#define copykeys sgp->_copykeys
#define complen  sgp->_complen
#define compofst sgp->_compofst
#define AsDecend sgp->_AsDecend
#define keytype  sgp->_keytype
#define uniq     sgp->_uniq
#define uniquniq sgp->_uniquniq
#define uniquebuf sgp->_uniquebuf
#define nextsptr sgp->_nextsptr
#define itemlen  sgp->_itemlen
#define tblcount sgp->_tblcount
#define temp1fd  sgp->_temp1fd
#define temp2fd  sgp->_temp2fd
#define temp1nm  sgp->_temp1nm
#define temp2nm  sgp->_temp2nm
#define tempnm   sgp->_tempnm
#define tagnm    sgp->_tagnm
#define idx_ptr  sgp->_idxpointer

/* from _dzuitl.c */
#define sortbuf  sgp->_sortbuf
#define sortptr  sgp->_sortptr

/* from _dzindex.c */
#define vector   sgp->_vector
#define vanchor  sgp->_vanchor
#define leafblks sgp->_leafblks
#define maxitems sgp->_maxitems
#define skeylen  sgp->_skeylen
#define hdrnode  sgp->_hdrnode
#define initnode sgp->_initnode
#define curnode  sgp->_curnode
#define lastnode sgp->_lastnode
#define usednodes sgp->_usednodes
#define initeof  sgp->_initeof
#define freelist sgp->_freelist
#define blksz	 sgp->_blksz

/* from _dzmerge.c */
#define leafs    sgp->_leafs

/* for debugging */
#define debugi   sgp->_debugi
#define ddd      sgp->_ddd
#define debugp   sgp->_debugp

#ifndef unix

int _DECLARE _dZmain(
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR origfile, CHAR_PTR nextfile, int opcode, CHAR_PTR squalify);
int _DECLARE badexit(PSORTGDAT sgp, int badnumber);
int _DECLARE isort(
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PSORTGDAT sgp, DBF tblfd,
	long reccnt, int reclen,
	unsigned int blocks, int external, LONG_PTR unitsize);
int _DECLARE dmerge(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp, UPTR outptr, long totitems,
		unsigned int blocks, long unitsize, int external,
		unsigned int initmsiz);
int _DECLARE getismem(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp, long filelen, long items, INT_PTR external,
		UINT_PTR blocks);
int _DECLARE getxsmem(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp);
long _DECLARE getqfnum(CHAR_PTR sptr, pCHAR_PTR newsptr);
void _DECLARE freeallm(PSORTGDAT sgp, int cnt);
int _DECLARE initvector(
#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			PSORTGDAT sgp, int handle);
int _DECLARE bldindex(
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp, int handle, CHAR_PTR kptr);
int _DECLARE setheader(
#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			PSORTGDAT sgp, int handle);

extern int (_DECLARE *asyscmp)
		   (PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE A0Ccmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE D0Ccmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE A0Icmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE D0Icmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE A12cmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE D1cmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UA12cmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UD1cmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE A12Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE D1Tcmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UA12Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UD1Tcmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE A1Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE D1Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UA1Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE UD1Ncmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);

#define CHARid    0
#define NUMERICid 1
#define FLOATid   2
#define DATEid    3
#define DESCENDid 4
#define TOTALid   8

extern int (_DECLARE *op1cmp[TOTALid])
		   (PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
extern int (_DECLARE *Uop1cmp[TOTALid])
		   (PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
extern int (_DECLARE *op2cmp[TOTALid])
		   (PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
extern int (_DECLARE *Uop2cmp[TOTALid])
		   (PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);

#endif 	/* unix */



#ifdef unix

int _DECLARE _dZmain();
int _DECLARE badexit();
int _DECLARE isort();
int _DECLARE dmerge();
int _DECLARE getismem();
int _DECLARE getxsmem();
long _DECLARE getqfnum();
void _DECLARE freeallm();
int _DECLARE initvector();
int _DECLARE bldindex();
int _DECLARE setheader();

extern int (_DECLARE *asyscmp)();

int _DECLARE A0Ccmp();
int _DECLARE D0Ccmp();
int _DECLARE A0Icmp();
int _DECLARE D0Icmp();
int _DECLARE A12cmp();
int _DECLARE D1cmp();
int _DECLARE UA12cmp();
int _DECLARE UD1cmp();
int _DECLARE A12Ncmp();
int _DECLARE D1Tcmp();
int _DECLARE UA12Ncmp();
int _DECLARE UD1Tcmp();
int _DECLARE A1Ncmp();
int _DECLARE D1Ncmp();
int _DECLARE UA1Ncmp();
int _DECLARE UD1Ncmp();

#define CHARid    0
#define NUMERICid 1
#define FLOATid   2
#define DATEid    3
#define DESCENDid 4
#define TOTALid   8

extern int (_DECLARE *op1cmp[TOTALid]);
extern int (_DECLARE *Uop1cmp[TOTALid]);
extern int (_DECLARE *op2cmp[TOTALid]);
extern int (_DECLARE *Uop2cmp[TOTALid]);

#endif 	/* unix */
