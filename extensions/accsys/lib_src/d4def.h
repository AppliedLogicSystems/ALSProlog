/***********
 * d4def.h *
 ***********/

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
#define HAVE_ECVT 1
#define HAVE_STRDUP 1
#endif /* UNIX */

#ifndef AccDB4
#include "db4.h"
#endif

#ifndef d4DEF

#if defined(__TURBOC__)				/* Turbo C */
#define _DECLARE cdecl
#elif defined(__ZTC__)				/* Zortech C/C++ */
#define _DECLARE _cdecl
#elif defined(_WINDOWS) && defined(ACDLL)	/* Windows DLL */
#define _DECLARE PASCAL				/* (Microsoft C6.0 ) */
#elif (_MSC_VER >= 600)				/* Microsoft C6.0 or later */
#define _DECLARE _cdecl
#else
#define _DECLARE					/* other C compilers */
#endif

#ifndef _WINDOWS
typedef CHAR_PTR     * pCHAR_PTR;
typedef TAG_PTR      * pTAG_PTR;
#else
typedef CHAR_PTR     far * pCHAR_PTR;
typedef TAG_PTR      far * pTAG_PTR;
#endif

/* Buffer Priority Decision Making Alogorithm:
        AccSys keeps track of the priority of each buffer in use by the
        Least Recently Used (LRU) Algorithm.
            Ref.) J. R. Spirn, "Program Behavior: Models and Measurements",
                  Elsevier North-Holland, Inc., 1977.
*/

typedef struct potato
{
#ifndef _WINDOWS
    struct potato *next; /* ptr to next POTATO */
    struct potato *prev; /* ptr to previous POTATO */
#else
    struct potato far *next; /* ptr to next POTATO */
    struct potato far *prev; /* ptr to previous POTATO */
#endif
    CHAR_PTR bufptr;     /* this member will be cast to various
			    types of buffers */
} POTATO;
#ifndef _WINDOWS
typedef POTATO      *PPOTATO;
typedef PPOTATO     *pPPOTATO;
#else
typedef POTATO  far *PPOTATO;
typedef PPOTATO far *pPPOTATO;
#endif

typedef struct
{
        char type;             /* field type */
        unsigned char length;  /* field length */
        unsigned char decimal; /* decimal places */
} FLDTL;
#define SZFLDTL sizeof(FLDTL)
#ifndef _WINDOWS
typedef FLDTL   *PFLDTL;
#else
typedef FLDTL   far *PFLDTL;
#endif

typedef struct
{
        char fieldnm[11]; /* field name terminating with NULL(0)
                             max. length => 10 char's + 1(NULL) char */
} FLDNM;
#define SZFLDNM sizeof(FLDNM)
#ifndef _WINDOWS
typedef FLDNM   *PFLDNM;
#else
typedef FLDNM   far *PFLDNM;
#endif

/* common data structure */
typedef struct comdat
{
        int      handle;   /* file handle */
        int      flag;     /* flag to indicate OPEN/CLOSED */
        int      special;  /* special flag */
        int      ftype;    /* file type */
        int      fmode;    /* mode of open file */
        long     size;     /* item size */
        long     eofnode;  /* eof node */
        int      itemlen;  /* item length in bytes */
        int      nodesz;   /* node size */
        int      maxitems; /* maximum possible # of items in a node */
        int      changed;  /* 1: some header-write  0: no write */
        long     curnode;  /* node that contains current record */
} COMDAT;
#ifndef _WINDOWS
typedef COMDAT     *PCOMDAT;
#else
typedef COMDAT far *PCOMDAT;
#endif

/* DBF node buffer structure definition */
typedef struct dbfnode
{
        long		nodeid;  /* node number */
        unsigned char	bufflag; /* buffer flag: UNUSED, ACREAD or ACWRITE */
        long		fstrec;  /* record number of 1st record
                                    in this node */
        int		has;     /* # of records */
        CHAR_PTR	nodebuf; /* node buffer */
}
TBLNOD;
#ifndef _WINDOWS
typedef TBLNOD      *PTBLNOD;
typedef PTBLNOD     *pPTBLNOD;
#else
typedef TBLNOD  far *PTBLNOD;
typedef PTBLNOD far *pPTBLNOD;
#endif

#define ACCESSSZ 0x020
#define MAXRLK 51
/* DBF file definition */
typedef struct tblfile
{
        COMDAT   tblcdata;   /* table common data */
#ifdef ACDLL
	DBGDAT_PTR pdbg;     /* pointer to global data */
#endif
        unsigned char month, day, year; /* date of last updeate */
        int fldsnum;         /* no. of fields */
        PFLDNM fldnames;     /* array of field names */
        PFLDTL fltyplen;     /* array of field type & length */
        unsigned int stadrs; /* record start address */

        unsigned tblacc[ACCESSSZ]; /* access hash table */

        PTBLNOD nodes;       /* table instance of potatoes:
                                chain of partial nodes */

                /*** I/O buffer information ***/
        int    iostrat;   /* I/O strategy: d_BUF, d_BUFWT, d_NOBUF */
        PPOTATO potatoes; /* chain of potatoes */
        PPOTATO mru;      /* ptr to most recently used potato */
        PPOTATO lru;      /* ptr to least recently used potato */
        unsigned potatnum;  /* number of potatoes used */
        long     lstpnode;  /* last node physically present */

                /*** Relationship with DBT, MDX and NDX ***/
#ifndef _WINDOWS
        struct dbtfile *adbt; /* associated DBT */
        struct mdxfile *amdx; /* associated MDX */
        struct ndxfile *ndxhead, *ndxtail; /* head and tail to NDX chain */
#else
        struct dbtfile far *adbt; /* associated DBT */
        struct mdxfile far *amdx; /* associated MDX */
        struct ndxfile far *ndxhead, far *ndxtail;
					   /* head and tail to NDX chain */
#endif
} TBLFILE;
#define SZTBLFILE sizeof(TBLFILE)
#ifndef _WINDOWS
typedef TBLFILE  *PTBLFILE;
#else
typedef TBLFILE  far *PTBLFILE;
#endif
#define tblptr ((PTBLFILE) dbfptr)

/* MDX or NDX node definition */
typedef struct xdxnod
{
        int  indexid;  /* index id number */
        long nodeid;   /* node number */
        unsigned char  bufflag;  /* buffer flag: UNUSED, ACREAD or ACWRITE */
        int            has;      /* # of indexes */
        CHAR_PTR       nodebuf; /* node buffer */
}
XDXNOD;
#ifndef _WINDOWS
typedef XDXNOD * PXDXNOD;
typedef PXDXNOD * pPXDXNOD;
#else
typedef XDXNOD far * PXDXNOD;
typedef PXDXNOD far * pPXDXNOD;
#endif

typedef struct xdxvec
{
    long     nodeid;
    unsigned potatoid;
    unsigned short offset;
} XDXVEC;
#ifndef _WINDOWS
typedef XDXVEC * PXDXVEC;
#else
typedef XDXVEC far * PXDXVEC;
#endif

/******************************/
/* index definition structure */
/******************************/
typedef struct index
{
    COMDAT   xdxcdata;  /* common data for index */
    int      indexid;   /* index indentification */
    int      position;  /* position control flag for index */
    long     root;      /* root node */
    int      keytype;   /* key type */
    int      exprlen;   /* key expression length */
    CHAR_PTR express;  /* key expression (including terminating 0) */
#ifndef unix
    int (_DECLARE *keycmp)
		(const VOID_PTR left, const VOID_PTR right, size_t len);
#else 	/* unix */
    int (_DECLARE *keycmp)();
#endif 	/* unix */
    int      complen;   /* # of bytes used for comparison */

    /*** used for indexed access ***/
    XDXVEC    xdxacc[ACCESSSZ]; /* access vector */
    PXDXVEC   leaf, work;	/* pointers to access vector */
    long      currecno;		/* current record number */
    CHAR_PTR  curkey;		/* current key buffer */

    /* IDX only data */
    long      bnumglst;     /* blk # / garbage collection list */
    long      tail, head;   /* tail & head of active node list */
    short     hasix;	    /* 1: has indices,  0: empty index */
} INDEX;
#ifndef _WINDOWS
typedef INDEX * PINDEX;
#else
typedef INDEX far * PINDEX;
#endif

/***********************
 * I/O cache structure *
 ***********************/
typedef struct adiobuff
{
    PXDXNOD  xnodes;    /* index instance of potatoes:
                                chain of partial nodes */
        /*** I/O buffer information ***/
    PPOTATO potatoes; /* chain of potatoes */
    PPOTATO mru;      /* ptr to most recently used potato */
    PPOTATO lru;      /* ptr to least recently used potato */
    unsigned potatnum;  /* number of potatoes used */
    long   lstpnode;    /* last node phyiscally present */
} ADIOBUFF; /* AccSys-for-dBASE I/O buffer structure */
#ifndef _WINDOWS
typedef ADIOBUFF * PADIOBUFF;
#else
typedef ADIOBUFF far * PADIOBUFF;
#endif

/****************************/
/* NDX definition structure */
/****************************/
typedef struct ndxfile
{
    INDEX     ndx;           /* index structure */
    PTBLFILE  atbl;         /* pointer to associated table */
    ADIOBUFF  ndxbuff;
#ifdef ACDLL
    DBGDAT_PTR pdbg;     /* pointer to global data */
#endif

    /*** info for other index files ***/
#ifndef _WINDOWS
    struct ndxfile *next; /* next 2ndary index files in chain */
    struct ndxfile *prev; /* previous 2ndary index files in chain */
#else
    struct ndxfile far *next; /* next 2ndary index files in chain */
    struct ndxfile far *prev; /* previous 2ndary index files in chain */
#endif
} NDXFILE;
#ifndef _WINDOWS
typedef NDXFILE * PNDXFILE;
#else
typedef NDXFILE far * PNDXFILE;
#endif
#define nptr ((PNDXFILE) ndxptr)

/****************************/
/* MDX definition structure */
/****************************/
typedef struct avlist
{
    int      updated; /* MDX global header change indicator (1/0: yes/no) */
    long     mdxeof;  /* MDX eof node */
    long     ghead;   /* head of garbage collection list */
    long     gnums;   /* number of garbage collected nodes (not accurate) */
    int      blksiz;  /* no. of blocks: 2 - 32 */
} AVAILIST;
#ifndef _WINDOWS
typedef AVAILIST * PAVAILIST;
#else
typedef AVAILIST far * PAVAILIST;
#endif

typedef struct mdxfile
{
    int      handle;    /* file handle */
    int      flag;      /* flag to indicate OPEN/CLOSED */
    int      fmode;     /* file open mode */
    PTBLFILE atbl;     /* pointer to associated table */
    int      indexes;   /* number of indexes */
    AVAILIST freelist;  /* list of free nodes */
    ADIOBUFF  mdxbuff;  /* I/O buffer */
#ifdef ACDLL
    DBGDAT_PTR pdbg;     /* pointer to global data */
#endif
#ifndef _WINDOWS
    struct iidxmdx *headiidx; /* head of active IIDXMDXes */
    struct iidxmdx *tailiidx; /* tail of active IIDXMDXes */
#else
    struct iidxmdx far *headiidx; /* head of active IIDXMDXes */
    struct iidxmdx far *tailiidx; /* tail of active IIDXMDXes */
#endif
} MDXFILE;
#ifndef _WINDOWS
typedef MDXFILE *PMDXFILE;
#else
typedef MDXFILE far *PMDXFILE;
#endif
#define mptr ((PMDXFILE) mdxptr)

/***********************************************/
/* Individual Index Structure for an MDX index */
/***********************************************/
typedef struct iidxmdx
{
    INDEX     idx;          /* index structure */
    PMDXFILE  amdx;        /* pointer to associated MDX */
    long      hdrnode;      /* header node */
    CHAR_PTR  tagname;     /* tag name */
    /*** info for other index files ***/
#ifndef _WINDOWS
    struct iidxmdx *nextidx; /* next index in MDX */
    struct iidxmdx *previdx; /* previous index in MDX */
#else
    struct iidxmdx far *nextidx; /* next index in MDX */
    struct iidxmdx far *previdx; /* previous index in MDX */
#endif
} IIDXMDX;
#ifndef _WINDOWS
typedef IIDXMDX * PIIDXMDX;
#else
typedef IIDXMDX far * PIIDXMDX;
#endif
#define iptr ((PIIDXMDX) idxptr)

/* structure for memo files */
typedef struct dbtfile
{
        int      handle;   /* file handle */
#ifdef ACDLL
	DBGDAT_PTR pdbg;     /* pointer to global data */
#endif
        int      flag;     /* flag to indicate OPEN/CLOSED */
        int      dbtmode;  /* mode of open file */
        long     dbteof;   /* eof location */
        long     freehead; /* head of free-list */
        int      blksiz;   /* block size in bytes */
        CHAR_PTR dbtbuf;  /* DBT I/O buff */
        int      version;  /* dBASE IV or dBASE III (PLUS) */
} DBTFILE;
#ifndef _WINDOWS
typedef DBTFILE * PDBTFILE;
#else
typedef DBTFILE far * PDBTFILE;
#endif
#define pdbt ((PDBTFILE) dbtptr)

#define ACCMASK  0x01f
#define ACREAD   0x01
#define ACTHEAD  0x0fc
#define ACTLSIZ  8
#define ACTMPLEN 85
#define ACTTAGS  0x220
#define ACTTAIL  0x0f8
#define ACWRITE  0x02
#define BACKSLASH 0x5c
#define BLKSIZ   0x14
#define BNOGLIST 4
#define BOTTOM     0xfffe
#define CHARACTER 0
#define CLOSED 0
#define COPY    1
#define DAY     0x3
#define DAY2     0x2e
#define DBFTYPE 0x0
#define DBFwDBT3  0x83
#define DBFwDBT4  0x8B
#define DBFwoDBT  0x03
#define DBRONLY  0x60
#define DBTMINSZ 0x18
#define DBTNODSZ 0x200
#define dDATECONST 1721425.0
#define DELETE  1
#define DESCEND   0x08
#define DNODSHIFT  10
#define DNODSIZ  0x400
#define d_BUF    1
#define d_BUFWT  4
#define d_NOBUF  2
#define ENCRYPT 0xF
#define ENDMARK   0x1A
#define EOFNODE   0x4
#define EOFNODE2  0x14
#define FIELDEND  0x0D
#define FIELDS  0x20
#define FLDDEC  0x11
#define FLDLEN  0x10
#define FLDTYP  0xb
#define FORCE   0x80
#define FORcndtn 0x02fa
#define FORMARK  0x0f5
#define FORsig   0x01
#define FORWARD 1
#define hasDBT   0x80
#define HASDBT3 3
#define HASDBT4 2
#define HASIDX  1
#define HASMDX  1
#define HIGH     0x11
#define IDXMARK  0x0f6
#define INPROGRESS 0
#define isDBF    0x03
#define isDBT4   0x08
#define isIDX     0x10
#define JULIAN 146097
#define KCOMPLEN  0x0C
#define KEXPRESS  0x18
#define KEYLEN    0x12
#define LOW      0x10
#define MAVLNODE 0x28
#define MAXFLDNM   10
#define MAXFLDS    255
#define MAXKEYEX  220
#define MAXKEYLEN 100
#define MAXKEYS   0x0E
#define MAXRECLEN 4000
#define MAXTAGS    47
#define MDXENODE 0x20
#define MDXFLAG 0x1C
#define MDXGHEAD 0x24
#define MDXHDRSZ 0x800
#define MDXNAME  0x04
#define MDXTYPE 2
#define MEMOWTSZ 8192
#define MONTH   0x2
#define MONTH2   0x2d
#define MXCFLDLEN  254
#define MXCHRSnm    10
#define MXNKLEN     20
#define NDKEYLEN 8
#define NDXKTYPE  0x10
#define NDXNODSZ 0x200
#define NDXTYPE 0
#define NEW     0
#define NIL 0xffff
#define NKEYLEN  12
#define NUMDATE   1
#define NXTFLD  0x20
#define OPEN   9786
#define ORDERBYTE 0x08
#define PREVIOUS 0
#define PUTKEY     0xfffc
#define RDBFwDBT3 0xC3
#define RDBFwDBT4 0xCB
#define RDBFwoDBT 0x63
#define RECALL  2
#define RECLEN  0xA
#define RECORD  0x4
#define REMOVE   1
#define REWIND  0
#define ROOTNODE  0x0
#define SNODSHIFT   9
#define SNODSIZ  0x200
#define SZADIO sizeof(ADIOBUFF)
#define SZCOMDAT sizeof(COMDAT)
#define SZDBTFILE sizeof(DBTFILE)
#define SZIIDXMDX sizeof(IIDXMDX)
#define SZMDXFILE sizeof(MDXFILE)
#define SZNDXFILE sizeof(NDXFILE)
#define SZPOTATO sizeof(POTATO)
#define SZTBLNOD sizeof(TBLNOD)
#define SZXDXNOD sizeof(XDXNOD)
#define SZXDXVEC sizeof(XDXVEC)
#define TAG10    0x0f
#define TAG2     0x13
#define TAGNAME  0x04
#define TAGNUMS  0x1c
#define TAGSTART 0x200
#define TAGTYPE  0x14
#define TBLADRS 0x8
#define TDATASZ  0x20
#define TOCHAN   0x12
#define TOP        0xffff
#define UNINIT     0xfffd
#define UNIQUE    0x40
#define UNUSED  0x00
#define UPDATE  0
#define XDXHDRSZ 0x200
#define XDXKTYPE  0x09
#define YEAR    0x1
#define YEAR2    0x2c

/* external declarations for AccSys internal variables and sub-functions */

#ifdef unix 

int _DECLARE _Arename(
#ifdef ProtypesNeeded
	CHAR_PTR oldname, CHAR_PTR newname
#endif
	);

long _DECLARE _dDbuffs(
#ifdef ProtypesNeeded
	int fh, int buffs, UINT_PTR basicsiz, INT_PTR nofields
#endif
	);

int _DECLARE _Dnodesiz(
#ifdef ProtypesNeeded
	int reclen
#endif
	);

int _DECLARE _dDclose(
#ifdef ProtypesNeeded
	DBF dbfptr
#endif
	);

DBF _DECLARE _dDopen(
#ifdef ProtypesNeeded
	CHAR_PTR filename, int mode, int buffs
#endif
	);

int _DECLARE _dopn(
#ifdef ProtypesNeeded
	CHAR_PTR filename, int mode
#endif
	);

int _DECLARE _dcls(
#ifdef ProtypesNeeded
	int fh
#endif
	);

int _DECLARE _dtblsiz(
#ifdef ProtypesNeeded
	DBF dbfptr, LONG_PTR size
#endif
	);

int _DECLARE _dfindbf(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno, int iomode,
    LONG_PTR nodenum, INT_PTR offset,
    pPTBLNOD tnode
#endif
	);

void _DECLARE _dfindnd(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno,
    LONG_PTR nodenum, INT_PTR offset
#endif
	);

int _DECLARE _dgetbf(
#ifdef ProtypesNeeded
	DBF dbfptr, long recno, int iomode, long nodenum,
    int offset, pPTBLNOD tnode
#endif
	);

int _DECLARE _dbfndwt(
#ifdef ProtypesNeeded
	DBF dbfptr, PTBLNOD nodptr, PCOMDAT comptr
#endif
	);

PPOTATO _DECLARE _dgetptt(
#ifdef ProtypesNeeded
	pPPOTATO mru, pPPOTATO lru
#endif
	);

void _DECLARE _dcycptt(
#ifdef ProtypesNeeded
	PPOTATO cycle, pPPOTATO mru, pPPOTATO lru
#endif
	);

int _DECLARE _dapprec(
#ifdef ProtypesNeeded
	DBF dbfptr, PTBLNOD ndptr, CHAR_PTR record, int offset
#endif
	);

int _DECLARE _dcre8(
#ifdef ProtypesNeeded
	int opcode, CHAR_PTR filename, int nofields, INT_PTR castptr
#endif
	);

int _DECLARE _dcre8new(
#ifdef ProtypesNeeded
	CHAR_PTR filename, int force
#endif
	);

int _DECLARE _dNclose(
#ifdef ProtypesNeeded
	NDX ndxptr
#endif
	);

long _DECLARE _dNbuffs(
#ifdef ProtypesNeeded
	int fh, int buffs, UINT_PTR basicsiz
#endif
	);

NDX _DECLARE _dNopen(
#ifdef ProtypesNeeded
	CHAR_PTR filename, int mode, int buffs
#endif
	);

int _DECLARE _dxput(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
  	CHAR_PTR key, long recno
#endif
	);

int _DECLARE _ddescnd(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, CHAR_PTR key, int iomode,
    pPXDXNOD pnode, LONG_PTR exactrec
#endif
	);

int _DECLARE _dputkey(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
    PXDXNOD ndptr, CHAR_PTR key,
    long recno, unsigned short offset,
    pCHAR_PTR key1, LONG_PTR node1,
    pCHAR_PTR key2, LONG_PTR node2
#endif
	);

int _DECLARE _dgrbnod(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, PXDXVEC work, long nodenum,
    pPXDXNOD retnode
#endif
	);

int _DECLARE _dascnd(
#ifdef ProtypesNeeded
	int opcode, PINDEX pindex, PADIOBUFF pio,
    PAVAILIST pfreelst,
    CHAR_PTR key1, long node1, CHAR_PTR key2, long node2
#endif
	);

int _DECLARE _drewfwd(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, int which
#endif
	);

unsigned short _DECLARE _2bytes(
#ifdef ProtypesNeeded
	CHAR_PTR ptr
#endif
	);

void _DECLARE _bytes2(
#ifdef ProtypesNeeded
	unsigned short value, CHAR_PTR ptr
#endif
	);

long _DECLARE _4bytes(
#ifdef ProtypesNeeded
	CHAR_PTR ptr
#endif
	);

void _DECLARE _bytes4(
#ifdef ProtypesNeeded
	long value, CHAR_PTR ptr
#endif
	);

int  _DECLARE _zero2(
#ifdef ProtypesNeeded
	CHAR_PTR ptr
#endif
	);

int  _DECLARE _zero4(
#ifdef ProtypesNeeded
	CHAR_PTR ptr
#endif
	);

int _DECLARE _dkeyrm(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
    CHAR_PTR key, long recno
#endif
	);

int _DECLARE _drmkey(
#ifdef ProtypesNeeded
	CHAR_PTR key, long recno, PINDEX pindex, PADIOBUFF pio,
    PXDXNOD nodeptr, pCHAR_PTR updkey, INT_PTR rmvnode
#endif
	);

int _DECLARE _dnxtkey(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, CHAR_PTR nextkey,
	LONG_PTR recno
#endif
	);

int _DECLARE _dprvkey(
#ifdef ProtypesNeeded
	int opcode, PINDEX pindex, PADIOBUFF pio,
	CHAR_PTR prevkey, pPXDXNOD pnodeptr, LONG_PTR recno
#endif
	);

int _DECLARE _dhdnkey(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, long node, LONG_PTR lastnode,
    pCHAR_PTR hiddenkey
#endif
	);

extern int _daystbl[];

double _DECLARE _ACjulian(
#ifdef ProtypesNeeded
	int year
#endif
	);

int _DECLARE _dmmseek(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield
#endif
	);

int _DECLARE _dTmmsz(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memofield, LONG_PTR size
#endif
	);

int _DECLARE _dtptmem(
#ifdef ProtypesNeeded
	DBT dbtptr, CHAR_PTR memobuff, long length, long writelen,
    CHAR_PTR memofield, LONG_PTR located
#endif
	);

int _DECLARE _dtmmnhd(
#ifdef ProtypesNeeded
	DBT dbtptr, long newhead, long nheadhas
#endif
	);

int _DECLARE _dtchdbf(
#ifdef ProtypesNeeded
	CHAR_PTR name, int product
#endif
	);

void _DECLARE _dbasenm(
#ifdef ProtypesNeeded
	CHAR_PTR hasdot, CHAR_PTR basename
#endif
	);

int _DECLARE _dseekwt(
#ifdef ProtypesNeeded
	int fn, long position, CHAR_PTR writebuf, unsigned writelen
#endif
	);

int _DECLARE _dseekrd(
#ifdef ProtypesNeeded
	int fn, long position, CHAR_PTR readbuf, unsigned readlen
#endif
	);

MDX _DECLARE _dXopen(
#ifdef ProtypesNeeded
	CHAR_PTR filename, int mode, int buffs
#endif
	);

int _DECLARE _dXclose(
#ifdef ProtypesNeeded
	MDX mdxptr
#endif
	);

long _DECLARE _dxgetnd(
#ifdef ProtypesNeeded
	int fn, PAVAILIST pavailist
#endif
	);

long _DECLARE _dXbuffs(
#ifdef ProtypesNeeded
	int fh, int buffs, UINT_PTR basicsiz
#endif
	);

int _DECLARE _dxstag(
#ifdef ProtypesNeeded
	MDX mdxptr, CHAR_PTR tagname, int simple,
    INT_PTR indexid, LONG_PTR hdrnode, UINT_PTR memsize,
    INT_PTR exprlen, CHAR_PTR expression, CHAR_PTR FORcond,
    INT_PTR  unique, CHAR_PTR order
#endif
	);

int _DECLARE _dXgrec(
#ifdef ProtypesNeeded
	PINDEX pindex, PADIOBUFF pio, CHAR_PTR key, LONG_PTR recno
#endif
	);

int _DECLARE _dXiflsh(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int _DECLARE _dxwtGhd(
#ifdef ProtypesNeeded
	MDX mdxptr
#endif
	);

int _DECLARE _dxwtIhd(
#ifdef ProtypesNeeded
	IDX idxptr
#endif
	);

int _DECLARE _dxwtnod(
#ifdef ProtypesNeeded
	int fn, PADIOBUFF pio, unsigned nodesize, INT_PTR changed
#endif
	);

void _DECLARE _dfredm(
#ifdef ProtypesNeeded
	PTBLNOD nodeptr, int count
#endif
	);

void _DECLARE _dfrexm(
#ifdef ProtypesNeeded
	PXDXNOD nodeptr, int count
#endif
	);

int  _DECLARE _datoi(
#ifdef ProtypesNeeded
	CHAR_PTR sptr, pCHAR_PTR newsptr
#endif
	);

void _DECLARE _Atonf(
#ifdef ProtypesNeeded
	CHAR_PTR digits,int width,int decimal,int decpt,int sign,
    CHAR_PTR nfield
#endif
	);

int _DECLARE _dchktl(
#ifdef ProtypesNeeded
	CHAR_PTR typelen, INT_PTR type, INT_PTR keylen,
   	INT_PTR order
#endif
	);

int _DECLARE _dindex(
#ifdef ProtypesNeeded
	DBF dbfptr, CHAR_PTR tofilenm, CHAR_PTR tagname, int order,
   	int type, int fldoffset, int keylen, int unique,
   	int (*keygen)(CHAR_PTR record, CHAR_PTR key),
	int  (*condcheck)(CHAR_PTR record)
#endif
	);

int _DECLARE _ddescmp(
#ifdef ProtypesNeeded
	const VOID_PTR left, const VOID_PTR right,
    size_t len
#endif
	);

int _DECLARE _dncmp(
#ifdef ProtypesNeeded
	const VOID_PTR left, const VOID_PTR right,
    size_t len
#endif
	);

int _DECLARE _ddesncm(
#ifdef ProtypesNeeded
	const VOID_PTR left, const VOID_PTR right,
    size_t len
#endif
	);

int _DECLARE _dfcmp(
#ifdef ProtypesNeeded
	CHAR_PTR left, CHAR_PTR right, size_t len
#endif
	);

int _DECLARE _ddesfcm(
#ifdef ProtypesNeeded
	const VOID_PTR left, const VOID_PTR right,
    size_t len
#endif
	);

long _DECLARE _dTmemus(
#ifdef ProtypesNeeded
	int fh, INT_PTR basicsiz
#endif
	);

CHAR_PTR DECLARE _dtmpnm(
#ifdef ProtypesNeeded
	CHAR_PTR tempname
#endif
	);


#else	/* unix */

#ifndef ACDLL
int _DECLARE _Arename(CHAR_PTR oldname, CHAR_PTR newname);
long _DECLARE _dDbuffs(int fh, int buffs, UINT_PTR basicsiz, INT_PTR nofields);
int _DECLARE _Dnodesiz(int reclen);
int _DECLARE _dDclose(DBF dbfptr);
DBF _DECLARE _dDopen(CHAR_PTR filename, int mode, int buffs);
int _DECLARE _dopn(CHAR_PTR filename, int mode);
int _DECLARE _dcls(int fh);
int _DECLARE _dtblsiz(DBF dbfptr, LONG_PTR size);
int _DECLARE _dfindbf(DBF dbfptr, long recno, int iomode,
                    LONG_PTR nodenum, INT_PTR offset,
                    pPTBLNOD tnode);
void _DECLARE _dfindnd(DBF dbfptr, long recno,
                    LONG_PTR nodenum, INT_PTR offset);
int _DECLARE _dgetbf(DBF dbfptr, long recno, int iomode, long nodenum,
                   int offset, pPTBLNOD tnode);
int _DECLARE _dbfndwt(DBF dbfptr, PTBLNOD nodptr, PCOMDAT comptr);
PPOTATO _DECLARE _dgetptt(pPPOTATO mru, pPPOTATO lru);
void _DECLARE _dcycptt(PPOTATO cycle, pPPOTATO mru, pPPOTATO lru);
int _DECLARE _dapprec(DBF dbfptr, PTBLNOD ndptr, CHAR_PTR record, int offset);
int _DECLARE _dcre8(int opcode, CHAR_PTR filename, int nofields, INT_PTR castptr);
int _DECLARE _dcre8new(CHAR_PTR filename, int force);
int _DECLARE _dNclose(NDX ndxptr);
long _DECLARE _dNbuffs(int fh, int buffs, UINT_PTR basicsiz);
NDX _DECLARE _dNopen(CHAR_PTR filename, int mode, int buffs);
int _DECLARE _dxput(PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
		  CHAR_PTR key, long recno);
int _DECLARE _ddescnd(PINDEX pindex, PADIOBUFF pio, CHAR_PTR key, int iomode,
                    pPXDXNOD pnode, LONG_PTR exactrec);
int _DECLARE _dputkey(PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
                    PXDXNOD ndptr, CHAR_PTR key,
                    long recno, unsigned short offset,
                    pCHAR_PTR key1, LONG_PTR node1,
		    pCHAR_PTR key2, LONG_PTR node2);
int _DECLARE _dgrbnod(PINDEX pindex, PADIOBUFF pio, PXDXVEC work, long nodenum,
                    pPXDXNOD retnode);
int _DECLARE _dascnd(int opcode, PINDEX pindex, PADIOBUFF pio,
                   PAVAILIST pfreelst,
                   CHAR_PTR key1, long node1, CHAR_PTR key2, long node2);
int _DECLARE _drewfwd(PINDEX pindex, PADIOBUFF pio, int which);
unsigned short _DECLARE _2bytes(CHAR_PTR ptr);
void _DECLARE _bytes2(unsigned short value, CHAR_PTR ptr);
long _DECLARE _4bytes(CHAR_PTR ptr);
void _DECLARE _bytes4(long value, CHAR_PTR ptr);
int  _DECLARE _zero2(CHAR_PTR ptr);
int  _DECLARE _zero4(CHAR_PTR ptr);
int _DECLARE _dkeyrm(PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
                   CHAR_PTR key, long recno);
int _DECLARE _drmkey(CHAR_PTR key, long recno, PINDEX pindex, PADIOBUFF pio,
                   PXDXNOD nodeptr, pCHAR_PTR updkey, INT_PTR rmvnode);
int _DECLARE _dnxtkey(PINDEX pindex, PADIOBUFF pio, CHAR_PTR nextkey,
			LONG_PTR recno);
int _DECLARE _dprvkey(int opcode, PINDEX pindex, PADIOBUFF pio,
		CHAR_PTR prevkey, pPXDXNOD pnodeptr, LONG_PTR recno);
int _DECLARE _dhdnkey(PINDEX pindex, PADIOBUFF pio, long node, LONG_PTR lastnode,
                    pCHAR_PTR hiddenkey);
extern int _daystbl[];
double _DECLARE _ACjulian(int year);
int _DECLARE _dmmseek(DBT dbtptr, CHAR_PTR memofield);
int _DECLARE _dTmmsz(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int _DECLARE _dtptmem(DBT dbtptr, CHAR_PTR memobuff, long length, long writelen,
                    CHAR_PTR memofield, LONG_PTR located);
int _DECLARE _dtmmnhd(DBT dbtptr, long newhead, long nheadhas);
int _DECLARE _dtchdbf(CHAR_PTR name, int product);
void _DECLARE _dbasenm(CHAR_PTR hasdot, CHAR_PTR basename);
int _DECLARE _dseekwt(int fn, long position, CHAR_PTR writebuf, unsigned writelen);
int _DECLARE _dseekrd(int fn, long position, CHAR_PTR readbuf, unsigned readlen);
MDX _DECLARE _dXopen(CHAR_PTR filename, int mode, int buffs);
int _DECLARE _dXclose(MDX mdxptr);
long _DECLARE _dxgetnd(int fn, PAVAILIST pavailist);
long _DECLARE _dXbuffs(int fh, int buffs, UINT_PTR basicsiz);
int _DECLARE _dxstag(MDX mdxptr, CHAR_PTR tagname, int simple,
                   INT_PTR indexid, LONG_PTR hdrnode, UINT_PTR memsize,
                   INT_PTR exprlen, CHAR_PTR expression, CHAR_PTR FORcond,
		   INT_PTR  unique, CHAR_PTR order);
int _DECLARE _dXgrec(PINDEX pindex, PADIOBUFF pio, CHAR_PTR key, LONG_PTR recno);
int _DECLARE _dXiflsh(IDX idxptr);
int _DECLARE _dxwtGhd(MDX mdxptr);
int _DECLARE _dxwtIhd(IDX idxptr);
int _DECLARE _dxwtnod(int fn, PADIOBUFF pio, unsigned nodesize, INT_PTR changed);
void _DECLARE _dfredm(PTBLNOD nodeptr, int count);
void _DECLARE _dfrexm(PXDXNOD nodeptr, int count);
int  _DECLARE _datoi(CHAR_PTR sptr, pCHAR_PTR newsptr);
void _DECLARE _Atonf(CHAR_PTR digits,int width,int decimal,int decpt,int sign,
		     CHAR_PTR nfield);
int _DECLARE _dchktl(CHAR_PTR typelen, INT_PTR type, INT_PTR keylen,
		   INT_PTR order);
int _DECLARE _dindex(DBF dbfptr, CHAR_PTR tofilenm, CHAR_PTR tagname, int order,
                   	int type, int fldoffset, int keylen, int unique,
                   	int (*keygen)(CHAR_PTR record, CHAR_PTR key),
		   			int  (*condcheck)(CHAR_PTR record));
#ifdef _WINDOWS
int _DECLARE ACmemCMP(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
#endif
int _DECLARE _ddescmp(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
int _DECLARE _dncmp(const VOID_PTR left, const VOID_PTR right,
		    size_t len);
int _DECLARE _ddesncm(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
int _DECLARE _dfcmp(CHAR_PTR left, CHAR_PTR right, size_t len);

int _DECLARE _ddesfcm(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
long _DECLARE _dTmemus(int fh, INT_PTR basicsiz);
CHAR_PTR DECLARE _dtmpnm(CHAR_PTR tempname);

#else /* ACDLL */

int _DECLARE _Arename(DBGDAT_PTR dbgptr, CHAR_PTR oldname, CHAR_PTR newname);
long _DECLARE _dDbuffs(DBGDAT_PTR dbgptr, int fh, int buffs,
			UINT_PTR basicsiz, INT_PTR nofields);
int _DECLARE _Dnodesiz(int reclen);
int _DECLARE _dDclose(DBF dbfptr);
DBF _DECLARE _dDopen(DBGDAT_PTR dbgptr, CHAR_PTR filename,
			int mode, int buffs);
int _DECLARE _dopn(DBGDAT_PTR dbgptr, CHAR_PTR filename, int mode);
int _DECLARE _dcls(int fh);
int _DECLARE _dtblsiz(DBF dbfptr, LONG_PTR size);
int _DECLARE _dfindbf(DBF dbfptr, long recno, int iomode,
                    LONG_PTR nodenum, INT_PTR offset,
                    pPTBLNOD tnode);
void _DECLARE _dfindnd(DBF dbfptr, long recno,
                    LONG_PTR nodenum, INT_PTR offset);
int _DECLARE _dgetbf(DBF dbfptr, long recno, int iomode, long nodenum,
                   int offset, pPTBLNOD tnode);
int _DECLARE _dbfndwt(DBF dbfptr, PTBLNOD nodptr, PCOMDAT comptr);
PPOTATO _DECLARE _dgetptt(pPPOTATO mru, pPPOTATO lru);
void _DECLARE _dcycptt(PPOTATO cycle, pPPOTATO mru, pPPOTATO lru);
int _DECLARE _dapprec(DBF dbfptr, PTBLNOD ndptr, CHAR_PTR record, int offset);
int _DECLARE _dcre8(DBGDAT_PTR dbgptr,
	int opcode, CHAR_PTR filename, int nofields, INT_PTR castptr);
int _DECLARE _dcre8new(CHAR_PTR filename, int force);
int _DECLARE _dNclose(NDX ndxptr);
long _DECLARE _dNbuffs(DBGDAT_PTR dbgptr,
			int fh, int buffs, UINT_PTR basicsiz);
NDX _DECLARE _dNopen(DBGDAT_PTR dbgptr,
			CHAR_PTR filename, int mode, int buffs);
int _DECLARE _dxput(DBGDAT_PTR dbgptr,
		  PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
		  CHAR_PTR key, long recno);
int _DECLARE _ddescnd(DBGDAT_PTR dbgptr, PINDEX pindex, PADIOBUFF pio, CHAR_PTR key,
			int iomode, pPXDXNOD pnode, LONG_PTR exactrec);
int _DECLARE _dputkey(DBGDAT_PTR dbgptr,
			PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
			PXDXNOD ndptr, CHAR_PTR key,
			long recno, unsigned short offset,
			pCHAR_PTR key1, LONG_PTR node1,
			pCHAR_PTR key2, LONG_PTR node2);
int _DECLARE _dgrbnod(DBGDAT_PTR dbgptr,
		    PINDEX pindex, PADIOBUFF pio, PXDXVEC work, long nodenum,
                    pPXDXNOD retnode);
int _DECLARE _dascnd(DBGDAT_PTR dbgptr,
			int opcode, PINDEX pindex, PADIOBUFF pio,
			PAVAILIST pfreelst,
			CHAR_PTR key1, long node1, CHAR_PTR key2, long node2);
int _DECLARE _drewfwd(DBGDAT_PTR dbgptr,
			PINDEX pindex, PADIOBUFF pio, int which);
unsigned short _DECLARE _2bytes(CHAR_PTR ptr);
void _DECLARE _bytes2(unsigned short value, CHAR_PTR ptr);
long _DECLARE _4bytes(CHAR_PTR ptr);
void _DECLARE _bytes4(long value, CHAR_PTR ptr);
int  _DECLARE _zero2(CHAR_PTR ptr);
int  _DECLARE _zero4(CHAR_PTR ptr);
int _DECLARE _dkeyrm(DBGDAT_PTR dbgptr,
		PINDEX pindex, PADIOBUFF pio, PAVAILIST pfreelst,
                   CHAR_PTR key, long recno);
int _DECLARE _drmkey(DBGDAT_PTR dbgptr,
		CHAR_PTR key, long recno, PINDEX pindex, PADIOBUFF pio,
		PXDXNOD nodeptr, pCHAR_PTR updkey, INT_PTR rmvnode);
int _DECLARE _dnxtkey(DBGDAT_PTR dbgptr,
		PINDEX pindex, PADIOBUFF pio, CHAR_PTR nextkey,
		LONG_PTR recno);
int _DECLARE _dprvkey(DBGDAT_PTR dbgptr,
		int opcode, PINDEX pindex, PADIOBUFF pio,
		CHAR_PTR prevkey, pPXDXNOD pnodeptr, LONG_PTR recno);
int _DECLARE _dhdnkey(DBGDAT_PTR dbgptr,
		PINDEX pindex, PADIOBUFF pio, long node, LONG_PTR lastnode,
		pCHAR_PTR hiddenkey);
extern int _daystbl[];
double _DECLARE _ACjulian(int year);
int _DECLARE _dmmseek(DBT dbtptr, CHAR_PTR memofield);
int _DECLARE _dTmmsz(DBT dbtptr, CHAR_PTR memofield, LONG_PTR size);
int _DECLARE _dtptmem(DBT dbtptr, CHAR_PTR memobuff, long length, long writelen,
                    CHAR_PTR memofield, LONG_PTR located);
int _DECLARE _dtmmnhd(DBT dbtptr, long newhead, long nheadhas);
int _DECLARE _dtchdbf(DBGDAT_PTR dbgptr, CHAR_PTR name, int product);
void _DECLARE _dbasenm(CHAR_PTR hasdot, CHAR_PTR basename);
int _DECLARE _dseekwt(int fn, long position, CHAR_PTR writebuf, unsigned writelen);
int _DECLARE _dseekrd(int fn, long position, CHAR_PTR readbuf, unsigned readlen);
MDX _DECLARE _dXopen(DBGDAT_PTR dbgptr,
			CHAR_PTR filename, int mode, int buffs);
int _DECLARE _dXclose(MDX mdxptr);
long _DECLARE _dxgetnd(DBGDAT_PTR dbgptr, int fn, PAVAILIST pavailist);
long _DECLARE _dXbuffs(DBGDAT_PTR dbgptr,
			int fh, int buffs, UINT_PTR basicsiz);
int _DECLARE _dxstag(MDX mdxptr, CHAR_PTR tagname, int simple,
                   INT_PTR indexid, LONG_PTR hdrnode, UINT_PTR memsize,
                   INT_PTR exprlen, CHAR_PTR expression, CHAR_PTR FORcond,
		   INT_PTR  unique, CHAR_PTR order);
int _DECLARE _dXgrec(DBGDAT_PTR dbgptr,
	PINDEX pindex, PADIOBUFF pio, CHAR_PTR key, LONG_PTR recno);
	int _DECLARE _dXiflsh(IDX idxptr);
int _DECLARE _dxwtGhd(MDX mdxptr);
int _DECLARE _dxwtIhd(IDX idxptr);
int _DECLARE _dxwtnod(DBGDAT_PTR dbgptr, int fn, PADIOBUFF pio,
			unsigned nodesize, INT_PTR changed);
void _DECLARE _dfredm(PTBLNOD nodeptr, int count);
void _DECLARE _dfrexm(PXDXNOD nodeptr, int count);
int  _DECLARE _datoi(CHAR_PTR sptr, pCHAR_PTR newsptr);
void _DECLARE _Atonf(CHAR_PTR digits,int width,int decimal,int decpt,int sign,
		     CHAR_PTR nfield);
int _DECLARE _dchktl(DBGDAT_PTR dbgptr, CHAR_PTR typelen,
			INT_PTR type, INT_PTR keylen, INT_PTR order);
int _DECLARE _dindex(DBF dbfptr, CHAR_PTR tofilenm, CHAR_PTR tagname, int order,
                   int type, int fldoffset, int keylen, int unique,
                   FARPROC keygen,
		   FARPROC condcheck);
#ifdef _WINDOWS
int _DECLARE ACmemCMP(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
#endif
int _DECLARE _ddescmp(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
int _DECLARE _dncmp(const VOID_PTR left, const VOID_PTR right,
		    size_t len);
int _DECLARE _ddesncm(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
int _DECLARE _dfcmp(CHAR_PTR left, CHAR_PTR right, size_t len);

int _DECLARE _ddesfcm(const VOID_PTR left, const VOID_PTR right,
		      size_t len);
long _DECLARE _dTmemus(DBGDAT_PTR dbgptr, int fh, INT_PTR basicsiz);
CHAR_PTR DECLARE _dtmpnm(DBGDAT_PTR dbgptr, CHAR_PTR tempname);

#endif /* <=== of #ifdef ACDLL ... #else .... */

#endif	/* unix */


#ifndef ACDLL
extern char _dtmpdir[]; /* directory for temporary files */
extern char _dminbuf[]; /* _dminbuf[120] */
#ifdef _WINDOWS
extern char _dWINbuf[]; /* _dWINbuf[120] */
#endif
#endif

#ifdef ACDLL
#undef dversion
#define dversion dbgptr->dversion
#undef d_report
#define d_report dbgptr->d_report
#undef dretcode
#define dretcode dbgptr->dretcode
#undef d_request
#define d_request dbgptr->d_request
#undef  d_blksiz
#define d_blksiz  dbgptr->d_blksiz
#undef d_recno
#define d_recno	  dbgptr->d_recno
#undef _dtmpdir
#define _dtmpdir  dbgptr->_dtmpdir /* [85] */
#undef _dminbuf
#define _dminbuf  dbgptr->_dminbuf /* [120] */
#undef  _dWINbuf
#define _dWINbuf  dbgptr->_dWINbuf /* [256] */
#endif

/* Compiler dependent macros */

#ifdef unix
#ifdef STANDARD
#undef STANDARD
#endif
#else 	/* unix */

#ifdef MacOS
#ifdef STANDARD
#undef STANDARD
#endif
int stricmp(CHAR_PTR str1, CHAR_PTR str2);
char *strdup(const char *s);
char *strupr(char *str);
int memicmp(void *left, void *right, size_t len);
int _memavl(void);
char *itoa(int  val, char *str, int  radix);
char *ecvt(double value, int count, int *dec, int *sign);
#else 	/* MacOS */

#ifdef LATTICE
#define O_BINARY O_RAW
#define d_DENYRW  O_SDRW
#define d_DENYNO  O_SDN
#define d_DENYWR  O_SDW
#define MSCLONE
#define _osmajor _DOS
#define sopen open
void _DECLARE itoa(int ivalue, CHAR_PTR buffer, int ignore);
void _DECLARE ltoa(long lvalue, CHAR_PTR buffer, int ignore);
#define doslock _doslock
#define dosunlk _dosunlk
#define nvlock  _nvlock
#define nvunlk  _nvunlk
#ifdef STANDARD
#undef STANDARD
#endif

#else 	/* LATTICE */
	/* MSC, TURBOC, or ZORTECH or __HIGHC__ */
#define STANDARD 1
#ifdef ZORTECH
int stricmp(CHAR_PTR str1, CHAR_PTR str2);
#endif

#endif	/* LATTICE */
#endif	/* MacOS */
#endif 	/* unix */


#ifdef unix

VOID_PTR  _DECLARE _ACalloc(
#ifdef ProtypesNeeded
	unsigned long memsize
#endif
	);

void _DECLARE _ACfree(
#ifdef ProtypesNeeded
	VOID_PTR memory
#endif
	);

#else 	/* unix */

#ifndef _WINDOWS
VOID_PTR  _DECLARE _ACalloc(unsigned long memsize);
void _DECLARE _ACfree(VOID_PTR memory);
#else
void far * _DECLARE _ACalloc(unsigned long memsize);
void _DECLARE _ACfree(VOID_PTR memory);
#endif

#endif	/* unix */


#ifndef unix
#ifndef __HIGHC__
#ifndef _WINDOWS
#define ACmemcpy memcpy
#define ACmemccpy memccpy
#define ACmemset memset
#define ACmemmove memmove
#define ACmemcmp memcmp
#define ACmemCMP memcmp
#define ACmemicmp memicmp
#define ACstrchr strchr
#define ACstrstr strstr
#define ACstrrchr strrchr
#define ACstrcpy strcpy
#define ACstrncpy strncpy
#define ACstrcmp strcmp
#define ACstricmp stricmp
#define ACstrlen strlen
#define ACstrcat strcat
#define ACstrupr strupr
#define ACatoi atoi
#define ACatof atof
#define ACitoa itoa
#define ACltoa ltoa
#define _ACread read
#define _ACwrite write
#define _ACremove remove
#else 	/* _WINDOWS */
#define ACmemcpy _fmemcpy
#define ACmemccpy _fmemccpy
#define ACmemset _fmemset
#define ACmemmove _fmemmove
#define ACmemcmp _fmemcmp
#define ACmemicmp _fmemicmp
#define ACstrchr _fstrchr
#define ACstrstr _fstrstr
#define ACstrrchr _fstrrchr
#define ACstrcpy lstrcpy
#define ACstrncpy _fstrncpy
#define ACstrcmp lstrcmp
#define ACstricmp _fstricmp
#define ACstrlen lstrlen
#define ACstrcat lstrcat
#define ACstrupr _fstrupr
	int _DECLARE ACatoi(CHAR_PTR stringptr);
	double _DECLARE ACatof(CHAR_PTR ascii);
	void _DECLARE ACitoa(int ivalue, CHAR_PTR ascii, int base);
	void _DECLARE ACltoa(long lvalue, CHAR_PTR ascii, int base);
#define _ACread _lread
#define _ACwrite _lwrite
	int _DECLARE _ACremove(CHAR_PTR filename);
#endif 	/* _WINDOWS */
#endif 	/* __HIGHC__ */
#endif 	/* unix */


#ifdef __HIGHC__ 
/* HighC Functions */
#define ACmemcpy memcpy
#define ACmemccpy _memccpy
#define ACmemset memset
#define ACmemmove memmove
#define ACmemcmp memcmp
#define ACmemCMP memcmp
#define ACmemicmp _memicmp
#define ACstrchr strchr
#define ACstrstr strstr
#define ACstrrchr strrchr
#define ACstrcpy strcpy
#define ACstrncpy strncpy
#define ACstrcmp strcmp
#define ACstricmp _stricmp
#define ACstrlen strlen
#define ACstrcat strcat
#define ACstrupr _strupr
#define ACatoi atoi
#define ACatof atof
#define ACitoa _itoa
#define ACltoa _ltoa
#define _ACread _read
#define _ACwrite _write
#define _ACremove remove
/* Extra Functions needed in HighC */
#define lseek _lseek 
#define close _close
#define itoa _itoa
#define ltoa _ltoa
#define strrev _strrev
#define filelength _filelength
#endif  /* __HIGHC__ */

#ifdef unix
#include <memory.h>
#include <string.h>
#define ACmemcpy memcpy
#define ACmemccpy memccpy
#define ACmemset memset
#define ACmemmove memmove
#define ACmemcmp memcmp
#define ACmemCMP memcmp
#define ACmemicmp memicmp
#define ACstrchr strchr
#define ACstrstr strstr
#define ACstrrchr strrchr
#define ACstrcpy strcpy
#define ACstrncpy strncpy
#define ACstrcmp strcmp
#define ACstricmp stricmp
#define ACstrlen strlen
#define ACstrcat strcat
#define ACstrupr strupr
#define ACatoi atoi
#define ACatof atof
#define ACitoa itoa
#define ACltoa ltoa
#define _ACread read
#define _ACwrite write
#define _ACremove remove
#endif 	/* unix */

#ifdef MacOS
#include <memory.h>
#include <string.h>
#include <sys/errno.h>
#include <unistd.h>
#define ACmemcpy memcpy
#define ACmemccpy memccpy
#define ACmemset memset
#define ACmemmove memmove
#define ACmemcmp memcmp
#define ACmemCMP memcmp
#define ACmemicmp memicmp
#define ACstrchr strchr
#define ACstrstr strstr
#define ACstrrchr strrchr
#define ACstrcpy strcpy
#define ACstrncpy strncpy
#define ACstrcmp strcmp
#define ACstricmp stricmp
#define ACstrlen strlen
#define ACstrcat strcat
#define ACstrupr strupr
#define ACatoi atoi
#define ACatof atof
#define ACitoa itoa
#define ACltoa ltoa
#define _ACread read
#define _ACwrite write
#define _ACremove remove
#endif 	/* MacOS */



#ifdef STANDARD
#include <share.h>
#define d_DENYRW  SH_DENYRW
#define d_DENYNO  SH_DENYNO
#define d_DENYWR  SH_DENYWR
#endif

#ifdef DEBUG
extern long acdebug;
#define MISC    0x01
#define LCKCNTL 0x02
#define PDWRITE 0x04
#define PDBUFF  0x08
#define ACDEBUG0(x) if (acdebug & x) printf("%s:%d\n", __FILE__, __LINE__);
#define ACDEBUG1(x, fmt) if (acdebug & x) printf(fmt, __FILE__, __LINE__);
#define ACDEBUG2(x, fmt, a) if (acdebug & x) printf(fmt,__FILE__,__LINE__,a);
#define ACDEBUG3(x,fmt,a,b) if (acdebug & x) printf(fmt,__FILE__,__LINE__,a,b);
#define ACDEBUG4(x,fmt,a,b,c) if (acdebug & x) printf(fmt,__FILE__,__LINE__,a,b,c);
#define ACDEBUG5(x,fmt,a,b,c,d) if (acdebug & x) printf(fmt,__FILE__,__LINE__,a,b,c,d);
#endif

#endif /* match -> #ifndef d4DEF */

#define d4DEF
