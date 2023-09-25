/*===================================================================*
 |			loadfile.c           
 |		Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |			-- Object module store and load functions
 |
 | Author: Kevin A. Buettner
 | Creation Date: 4/22/86
 | Revision History:
 | 02/24/88 - kev -- Sun Mods
 | 10/29/90 - ilyas -- To create .OBP files without creating
 |                                                       a temporary file.
 | 06/03/92 - ron -- Added support in get_file_date_time() and
 |                   isdir() for Macintosh file system.
 | 11/29/94 - C. Houpt	-- Moved MacOS specific file code for
 |			  get_file_modified_time() and isdir() to fsmac.c
 |			  I strongly suggest moving these functions to fsunix/fsdos.
 |			  -- Zero initilized the OBP headers, because otherwise random bytes
 |				 appear in the headers (this makes it difficult to compare files).
 |			  -- Various UCHAR casts.
 |			  -- Merge OBP and non-OBP load_file() for debugging,
 |				 the non-OBP load_file was out of date.
 *===================================================================*/
#include "defs.h"

#if	defined(DOS)	/* OS includes */
#include <stdefs.h>
#include <system.cf>

#elif	defined(AtariOS)
#include <errno.h>
#include <stat.h>

#elif	defined(VMS)
#include <errno.h>
#include <types.h>
#include <stat.h>
#include <file.h>

#elif   defined(MacOS)
#include <Resources.h>
#include <PLStringFuncs.h>
#if defined(HAVE_GUSI)
#include <GUSI.h>
#elif defined(MPW_TOOL)
#include <fcntl.h>
#else
#include <unix.h>
#endif
#elif defined(UNIX)
#include <errno.h>
#elif defined(MSWin32)
#include <windows.h>
#include "fswin32.h"
#endif			/* OS includes */


#include "wintcode.h"
#include "icodegen.h"
#include "version.h"


#ifdef OBP

/*
 * A .pro file must be at least EPSILON_TIME seconds older than
 * .obp before we decide to use the .obp file. This is done to
 * account for network delays and clock skews between machines.
 */

#define EPSILON_TIME   60

typedef struct {
    char  magic[120];
    long  endian;
    long  symtab_start;		/* Position in file of symbol table */
    long  symtab_size;		/* Number of symbol table entries */
    long  icode_start;		/* Start of icode records */
    long  icode_size;		/* Number of icode records */
} mod_header;

#define char_to_int(c) (((c)&0x80)?((c)|~0xFF):(c))


/*
 * Operand formats:
 *      Operands will be stored in the file from first to last.
 *      The table below describes the operand formats for each icode call.
 */

#define FT_UNUSED 	0	/* operand not used             */
#define FT_BYTE		1	/* operand takes up a (signed) byte */
#define FT_WORD		2	/* operand takes up a (signed) word */
#define FT_LONG		3	/* operand takes up a long      */
#define FT_XWORD	4	/* operand is an (unsigned)     */
				/* translated word, the translation */
				/* being done between the symbol */
				/* table section of the obp file */
				/* and the real symbol table    */
#define FT_STRING	5	/* operand is a string          */

#define U FT_UNUSED		/* abbreviations for the above  */
#define B FT_BYTE
#define W FT_WORD
#define L FT_LONG
#define X FT_XWORD
#define S FT_STRING


#define FR(o1,o2,o3,o4) ((o1) | (o2)<<4 | (o3)<<8 | (o4)<<12)

#define	LAST_IC IC_ENDALLOC

#define ICODE(macro,str,addr,obp)	obp,


/*
 * It is assumed in the format table below that a short is 16 bits.
 */

static unsigned short format_tab[] =
{
    FR(U, U, U, U),		/* IC_ENDALLOC          -26 */
    FR(U, U, U, U),		/* IC_BEGINALLOC        -25 */
    FR(X, W, X, U),		/* IC_CREMODCLOSURE     -24 */
    FR(U, U, U, U),		/* IC_PUT_ENDMOD_OBP    -23 */
    FR(X, W, U, U),		/* IC_ADDTO_AUTONAME    -22 */
    FR(X, U, U, U),		/* IC_ADDTO_AUTOUSE     -21 */
    FR(U, U, U, U),		/* undefined            -20 */
    FR(U, U, U, U),		/* undefined            -19 */
    FR(U, U, U, U),		/* IC_RESET             -18 */
    FR(U, U, U, U),		/* undefined            -17 */
    FR(U, U, U, U),		/* IC_ADDCLAUSE         -16 */
    FR(B, U, U, U),		/* IC_PUTMACRO          -15 */
    FR(B, U, U, U),		/* IC_ENDMACRO          -14 */
    FR(U, U, U, U),		/* IC_BEGINMACRO        -13 */
    FR(B, X, W, L),		/* IC_1STARG            -12 */
    FR(X, W, U, U),		/* IC_EXPORTPRED        -11 */
    FR(X, U, U, U),		/* IC_NEWMODULE         -10 */
    FR(U, U, U, U),		/* IC_ENDMODULE         -9  */
    FR(X, U, U, U),		/* IC_ADDUSE            -8  */
    FR(U, U, U, U),		/* IC_CHANGEMOD         -7  */
    FR(U, U, U, U),		/* IC_EXECCOMMAND       -6  */
    FR(U, U, U, U),		/* IC_EXECQUERY         -5  */
    FR(U, U, U, U),		/* IC_ASSERTA           -4  */
    FR(U, U, U, U),		/* IC_ASSERTZ           -3  */
    FR(X, W, U, U),		/* IC_ENDCLAUSE         -2  */
    FR(U, U, U, U),		/* IC_INIT              -1  */

#include "icodedef.h"

    FR(U, U, U, U)		/* last       */
};

/*-----------------------------------------------------------------------*
 | fix_magic is called at initialization time to replaces the X's with
 | the processor string and the Y's with the minor os string
 *-----------------------------------------------------------------------*/

#include "magic.h"

#ifdef HIDEME
static char MAGIC[] =
"ALS-Prolog Loadable Object Module\r\nFormat 1.21(XXXXXXXXXX,YYYYYYYYYY)\r\n\032\004\019\026";

void
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
#endif /* HIDEME */

#ifdef DOS
/*
 * Since putc is defined as a macro, this causes problem in DOS.
 * So we use fputc instead of putc.
 */
#undef putc
#define putc(a,b) fputc(a,b)
#endif /* DOS */


#ifdef AtariOS
/* In Atari, the first arg of putc has to coerced to char type */
#undef putc
#define putc(a,b) fputc((char)(a),b)
#endif /* AtariOS */


/*
 * Return codes of the function "f_load"
 */
#define FLOAD_FAIL 0
#define FLOAD_SUCCESS 1
#define FLOAD_ILLOBP 2


/*
 * .OBP file information
 */

static FILE *obp_fp;
static long obp_nrecs;

/*
 * Put the given icode into the .OBP file.
 */
void
f_icode(int opcode, long a1, long a2, long a3, long a4)
{
    long  args[4];
    register int format;
    register long arg;
    register int i;
    static long init_pos;
    static long init_obp_nrecs;


    args[0] = a1;
    args[1] = a2;
    args[2] = a3;
    args[3] = a4;

    switch (opcode) {
	case IC_INIT:
	    init_pos = ftell(obp_fp);
	    init_obp_nrecs = obp_nrecs;
	    break;
	case IC_ICRESET:
	    fseek(obp_fp, init_pos, 0);
	    obp_nrecs = init_obp_nrecs;
	    return;
	    break;
	default:
	    break;
    }

    obp_nrecs++;
    putc(opcode, obp_fp);
    format = format_tab[(opcode - (LAST_IC))];
    for (i = 0; i < 4; i++, format >>= 4) {
	arg = args[i];
	switch (format & 0xf) {
	    case FT_UNUSED:
		break;
	    case FT_BYTE:
		putc(arg, obp_fp);
		break;
	    case FT_WORD:
		putc(arg, obp_fp);
		putc((arg >> 8), obp_fp);
		break;
	    case FT_XWORD:
		arg = symmap(arg);
		putc(arg, obp_fp);
		putc(arg >> 8, obp_fp);
		break;
	    case FT_LONG:
		putc(arg, obp_fp);
		putc((arg >> 8), obp_fp);
		putc((arg >> 16), obp_fp);
		putc((arg >> 24), obp_fp);
		break;
	    case FT_STRING:{
		    register char *s = (char *) arg;

		    /* Typical unix lossage!
		     * This was: while (putc(*s++, obp_fp)) ;
		     * Think about what happens when putc returns EOF. Note that
		     * EOF is non-zero.
		     */
		    do {
		    	putc(*s, obp_fp);
		    } while (*s++);
		}
		break;
	    default:
		break;
	}
    }
}



/*
 * Open and initialize the .OBP file.
 * Returns 0 if it is unsuccessful.
 */
int
obp_open(char *fname)
{
    mod_header obp_header = {
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
    	0,
    	0,
    	0,
    	0,
    	0,    
    };

#ifdef CW_PLUGIN
char new_name[1000];
extern char *RedirectOBP(char *fname, char *new_name);

fname = RedirectOBP(fname, new_name);

#endif
    /*
     * Are we able to open .OBP file?
     */
    if ((obp_fp = fopen(fname, "w+b")) == NULL) {
	return (0);
    }

#if defined(MacOS) && defined(HAVE_GUSI)
    fsetfileinfo(fname, 'ALS4', 'OBPT');
#endif

    push_symmap();		/* push current symbol table map */
    /* and allocate a new one */

    obp_nrecs = 0;		/* initialize number of icode records */

    fseek(obp_fp, (long) 0, 0);	/* skip header area in .OBP file */
    fwrite((char *) (&obp_header), sizeof (obp_header), 1, obp_fp);

    return (1);
}

void
obp_close(void)
{
    int   i;
    unsigned short strsize;
    long *toks;
    long  ntoks;
    long  tok;
    mod_header obp_header = {
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    	"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
    	0,
    	0,
    	0,
    	0,
    	0,    
    };

    /*
     * Update the header information.
     */
    strcpy(obp_header.magic, MAGIC);
    obp_header.endian = ENDIAN_MAGIC;
    obp_header.icode_start = sizeof (mod_header);
    obp_header.icode_size = obp_nrecs;
    obp_header.symtab_start = ftell(obp_fp);

    /*
     * Write the symbol table information
     */
    toks = sym_order(&ntoks);
    obp_header.symtab_size = ntoks;
    for (i = 0; i < ntoks; i++) {
	tok = toks[i];
	strsize = TOKNAMELEN(tok) + 1;
	fwrite((char *) (&strsize), sizeof (strsize), 1, obp_fp);
	fwrite(TOKNAME(tok), (size_t)strsize, 1, obp_fp);
	fwrite(&TOKUNOP(tok), sizeof (TOKUNOP(tok)), 1, obp_fp);
	fwrite((char *) (&TOKBINOP(tok)), sizeof (TOKBINOP(tok)), 1, obp_fp);
    }

    /*
     * Rewrite the header information and close file
     */
    fseek(obp_fp, (long) 0, 0);
    fwrite((char *) (&obp_header), sizeof (obp_header), 1, obp_fp);
    fclose(obp_fp);

    /*
     * Release space used by current token map and restore old one
     */
    pop_symmap();
}


#define CBUFSIZ 1024

#if defined(sparc) || defined(__hp9000s800) || defined(UNIX_IRIX)
#define USHORT(p) (*(p) << 8 | *(p+1)) 
#else
#define USHORT(p) (*((unsigned short *)(p)))
#endif

static int
mem_load(unsigned char *startrp)
{
    mod_header *headerp;
    long  i, j;
    long *tokmap;
    int   opcode;
    long  args[4];
    register int format;
    register long data;
    unsigned char *rp;
    unsigned short strsize;
    unsigned short binop, unop;
    
    headerp = (mod_header *)startrp;
    
    if (strcmp(headerp->magic, MAGIC) != 0 || headerp->endian != ENDIAN_MAGIC)
    	return (FLOAD_ILLOBP);
    
    rp = startrp + headerp->symtab_start;

    tokmap = (long *) malloc(headerp->symtab_size * sizeof (long));

    for (i = 0; i < headerp->symtab_size; i++) {
    	strsize = USHORT(rp); rp += sizeof(strsize);
    	j = find_token((UCHAR *) rp); rp += strsize;
    	*(tokmap + i) = j;
    	unop = USHORT(rp); rp += sizeof(unop);
    	binop = USHORT(rp); rp += sizeof(binop);
	if (unop) {
	    if (TOKUNOP(j) && TOKUNOP(j) != unop) {
		fprintf(stderr,
		 "Warning: Unary operator conflict for '%s'\n",
			TOKNAME(j));
	    }
	    TOKUNOP(j) = unop;
	}
	if (binop) {
	    if (TOKBINOP(j) && TOKBINOP(j) != binop) {
		fprintf(stderr,
		"Warning: Binary operator conflict for '%s'\n",
			TOKNAME(j));
	    }
	    TOKBINOP(j) = binop;
	}
    }

    rp = startrp + headerp->icode_start;
    i = headerp->icode_size;

    while (i--) {
	opcode = *rp++;
	opcode = char_to_int(opcode);
	format = format_tab[(opcode - (LAST_IC))];

	for (j = 0; j < 4; j++, format >>= 4) {
	    switch (format & 0xf) {
		case FT_UNUSED:
		    data = 0;
		    break;
		case FT_BYTE:
		    data = *rp++;
		    data = char_to_int(data);
		    break;
		case FT_WORD:
		    data = *rp++ & 0xff;
		    data |= *rp++ << 8;
		    if (data & 0x8000)
			data |= ~(long) 0xffff;
		    break;
		case FT_LONG:
		    data = *rp++ & 0xff;
		    data |= (*rp++ & 0xff) << 8;
		    data |= ((long) (*rp++ & 0xff)) << 16;
		    data |= ((long) *rp++) << 24;
		    break;
		case FT_XWORD:
		    data = *rp++ & 0xff;
		    data |= (*rp++ & 0xff) << 8;
		    data = tokmap[data];
		    break;
		case FT_STRING:
		    data = (long) rp;
		    while (*rp++) ;
		    break;
		default:
		    data = 0;
		    break;
	    }

	    args[j] = data;
	}

	icode(opcode, args[0], args[1], args[2], args[3]);
    }

    free(tokmap);
    
#ifdef Indexing
    gen_indexing();
#endif

    w_relinkall();		/* relink all procedures */

    return (FLOAD_SUCCESS);
}

/*
 * f_load: Load a .OBP file
 */

int
f_load(const char *fname)
{
    unsigned char *p;
    mem_file_info i;
    int result;
    
    p = open_memory_file(fname, &i);
    
    if (p) {
    	result = mem_load(p);
	close_memory_file(&i);
    } else result = FLOAD_FAIL;
    
    return result;
}


#ifdef MacOS
static const char *strip_path(const char *name)
{
    const char *p;
    size_t l;
    
    l = strlen(name);
    
    if (l == 0) return name;
    
    p = name + l;
    
    while(1) {
    	p--;
    	if (*p == ':') {p++; break;}
    	if (p == name) break;
    }
    
    return p;
}


/*
 * obpres_load: Load a OBPT resource
 */

int obpres_load(const char *fname)
{
    char **obp_handle;
    Str255 pfname;
    int result;

    fname = strip_path(fname);
    c2pstrcpy(pfname, fname);
    PLstrcat(pfname, "\p.obp");

    obp_handle = GetNamedResource('OBPT', pfname);
    
    if (obp_handle) {
    	HLock(obp_handle);
    
    	result = mem_load(*obp_handle);
    	
	HUnlock(obp_handle);
	ReleaseResource(obp_handle);
    	
    } else result = FLOAD_FAIL;
    
    return result;
}
#endif /* MacOS */


static struct obp_stack_rec {
    int   nrecs;
    int   makeobp;
    FILE *fp;
} obp_stack[100];
static int obp_stack_top = 0;

void
obp_push(void)
{
    obp_stack[obp_stack_top].nrecs = obp_nrecs;
    obp_stack[obp_stack_top].makeobp = makeobp;
    obp_stack[obp_stack_top].fp = obp_fp;
    obp_stack_top++;
}

void
obp_pop(void)
{
    obp_stack_top--;
    obp_nrecs = obp_stack[obp_stack_top].nrecs;
    makeobp = obp_stack[obp_stack_top].makeobp;
    obp_fp = obp_stack[obp_stack_top].fp;
}

#define LOAD_RETURN(status) 					\
	{ 	makeobp=old_makeobp;				\
		w_reconstamp=old_reconsult_stamp;		\
		obp_fp = old_obp_fp;				\
		obp_nrecs = old_obp_nrecs;			\
		return(status);					\
	}

#else /* OBP */

#define LOAD_RETURN(status)	return(status);

#endif	/* OBP */

#if (defined(__DJGPP__) || defined(__GO32__))

static	long	get_file_modified_time	( const char * );
static	int	isdir			(const char * );

static long 
get_file_modified_time(const char *fname)
{
#if defined(DOS)

    long  date_and_time;

    if (!isdir(fname)) {
	date_and_time = get_file_date_and_time(fname);
	if (date_and_time != -1)
	    return (date_and_time);
	else
	    return (0);
    }
    else
	return (0);
#elif defined(MSWin32)

	HANDLE f;
	FILETIME wt;
	long result;
	
	f = CreateFile(fname, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if (f != INVALID_HANDLE_VALUE) {
		if (GetFileTime(f, NULL, NULL, &wt)) {
			result = wt.dwHighDateTime;
		} else result = 0;
		CloseHandle(f);
	} else result = 0;
	
	return result;
#else  /* DOS */

    struct stat buf;

    if (stat(fname, &buf) == -1 || buf.st_mode & S_IFDIR)
	return (long) 0;

    return buf.st_mtime;

#endif /* DOS */
}

/*
 * Returns 1 if fname is a directory, 0 otherwise.
 *
 */
static int
isdir(const char *fname)
{
#if defined(DOS)

#define DIR_BIT 0x0010

    int   fattr;		/* file attribute */

    fattr = get_file_attr(fname);
    if (fattr != -1)
	return (fattr & DIR_BIT);
    else
	return (0);
#elif defined(MSWin32)

	DWORD fattr;
	
	fattr = GetFileAttributes(fname);
	if (fattr != -1) return (fattr & FILE_ATTRIBUTE_DIRECTORY);
	else return 0;
	
#else  /* DOS */

    struct stat buf;

    if (stat(fname, &buf) == -1)
	return (0);

    return (buf.st_mode & S_IFDIR);

#endif /* DOS */
}

#endif /* 0000 */

#ifdef PURE_ANSI
#define R_OK 0
static int faccess(char *fname, int mode)
{
    FILE *f;
    f = fopen(fname, "r");
    if (f == NULL) return 1;
    else {
	fclose(f);
	return 0;
    }
}
#define get_file_modified_time(fname) (!faccess(fname, R_OK))
#endif /* PURE_ANSI */


#ifdef OLDCLOAD

int
load_file(char *fname, int options)
{
    long  pro_time;
    char *ext, *fnp;
    char  new_fname[256];
#ifdef OBP
    long  obp_time;
    int   status;
    long  old_reconsult_stamp = w_reconstamp;
    int   old_makeobp = makeobp;
    FILE *old_obp_fp = obp_fp;
    int   old_obp_nrecs = obp_nrecs;
#endif /* OBP */
    int   errorcount;

#ifdef OBP
    /*
     * Assume we are not creating .obp files until we
     * explicitly say otherwise.
     */
    makeobp = 0;
#endif /* OBP */

    /*
     * Are we reconsulting?
     */
    if (options & RECONSULT)
	w_reconstamp = w_timestamp;

    /*
     *  If the file "user", directly consult it
     */
    if (strcmp(fname, "user") == 0) {
	errorcount = consult(find_token((UCHAR *)fname));
	LOAD_RETURN(1)
    }

    /*
     * Make a copy of the file name
     */
    for (fnp = fname, ext = new_fname; (*ext++ = *fnp++);) ;
    *(ext - 1) = '.';		/* put '.' for extensions */

    /*
     * Run fnp backwards looking for an extension.
     */
#ifdef PURE_ANSI
    for (fnp--; fnp != fname && *fnp != '.'; fnp--) ;
#else
    for (fnp--; fnp != fname && *fnp != DIR_SEPARATOR && *fnp != '.'; fnp--) ;
#endif /* PURE_ANSI */

/*  printf("load_file:fname=%s fnp=%s\n",fname,fnp);  */
    /*
     *  Try to access the file as specified by making sure that
     *  it is not a directory. If it is accessable, load that file.
     */
#ifdef PURE_ANSI
    if (faccess(fname, R_OK) == 0) 
#else
    if (access(fname, R_OK) == 0 && !isdir(fname))
#endif /* PURE_ANSI */
	{
/*  printf("load_file:access ok to fname=%s\n",fname);  */
#ifdef OBP
	if (strcmp(fnp, ".obp") == 0) {

	    if (f_load(fname) != FLOAD_FAIL) {
		LOAD_RETURN(1)
	    }
	    else {
		LOAD_RETURN(0)
	    }
	}
	else 
#endif /* OBP */
	{
	    errorcount = consult(find_token((UCHAR *)fname));
	    LOAD_RETURN(1)
	}
    }

/* printf("load_file:access NOT ok to fname=%s\n",fname);  */
    /*
     *  Check whether the file has an extension or not.
     */
    if (*fnp != '.') {
	/*
	 *      File doesn't have an extension.
	 *      Get dates for .pro .obp versions (0 if none exists)
	 */
/* printf("load_file: no extension--base new_fname=%s\n",new_fname);  */
	strcpy(ext, "pro");
/* printf("load_file: added pro ext-new_fname=%s\n",new_fname);  */
	pro_time = get_file_modified_time(new_fname);
#if OBP
	strcpy(ext, "obp");

	obp_time = get_file_modified_time(new_fname);
#endif /* OBP */
    }
    else {
	/*
	 *      File has an extension, and we couldn't access
	 *      the file as specified.
	 */
	LOAD_RETURN(0)
    }
/* printf("load_file: pro_time=%d obp_time=%d\n",pro_time,obp_time);  */

    /*
     *  Try to load .obp file if it is newer than .pro file
     */
#ifdef OBP
    if (obp_time && (obp_time > (pro_time + EPSILON_TIME))) {
	strcpy(ext, "obp");
	status = f_load(new_fname);
	if (status == FLOAD_FAIL) {
	    LOAD_RETURN(0)
	}
	else if (status == FLOAD_SUCCESS) {
	    LOAD_RETURN(1)
	}
	else {			/* FLOAD_ILLOBP */
	    obp_time = 0;
	    fprintf(stderr,
	       "\nWarning: File '%s' in old or unrecognized .OBP format.\n",
		    fname);
	    if (pro_time)
		fprintf(stderr, "Attempting to recompile.\n");
	}
    }
#endif /* OBP */

    /*
     *  Try to load .pro file if it is newer than .obp file
     */
/* printf("load_file: About to try .pro file--check obp creation\n"); */
    if (pro_time) {
	/*
	 * Are we able to create .obp file?
	 */
#ifdef OBP
	// Suppress opening OBP and makobp=1, if suppress option enabled
	if (! (options & SUPPRESS_OBP)) {
	    strcpy(ext, "obp");

	    if (obp_open(new_fname) == 0) {
		fprintf(stderr, "Warning: Unable to create %s \n", new_fname);
	    }
	    else {
		makeobp = 1;
	    }
	}
#endif /* OBP */

	/*
	 * Load .pro file
	 */
/* printf("load_file: Really about to try .pro file\n"); */
	strcpy(ext, "pro");
	errorcount = consult(find_token((UCHAR *)new_fname));

	/*
	 * We need to check to see if any syntax errors were reported.
	 */
	if (errorcount) {
	    if (errorcount != 1)
		fprintf(stderr, "\n%d syntax errors found in file ``%s''.\n",
			errorcount, new_fname);
	    else
		fprintf(stderr, "One syntax error found in file ``%s''\n",
			new_fname);
	}

	/*
	 * Close .obp file.
	 * Delete .obp files if any syntax errors were reported.
	 */
#if OBP
	if (makeobp == 1) {
	    obp_close();
	    if (errorcount) {
		strcpy(ext, "obp");
		fprintf(stderr, "%s%s%s \n",
			"Warning: Since syntax errors were found, the file ",
			new_fname,
			" was not created. \n");
#ifdef	DOS
		c_unlink(new_fname);
#else  /* DOS */
#ifdef	VMS
		delete(new_fname);
#else  /* VMS */
#ifdef PURE_ANSI
		remove(new_fname);
#else
		unlink(new_fname);
#endif /*PURE_ANSI */
#endif /* VMS */
#endif /* DOS */
	    }
	}
#endif /* OBP */

	LOAD_RETURN(1)
    }

    LOAD_RETURN(0)

}

#else  /* not-OLDCLOAD */

int
load_file(char *fname, int options)
{
    strcat(fname, ".obp");

    /*
     *  Try to access the file as specified & make sure that
     *  it is not a directory. If it is accessable, load that file.
     */
    if (access(fname, R_OK) == 0 && !isdir(fname)) 
	{
		 	/* printf("Calling f_load#0(%s)\n",fname);  */
	    if (f_load(fname) != FLOAD_FAIL) 
			return(1);
	    else 
			return(0);					
    }
}


#endif /* OLDCLOAD */
