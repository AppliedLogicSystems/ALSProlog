/*=====================================================================*
 |		loadforeign.c
 |      Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |		-- dynamically load a .o file
 |
 | Author: Kevin A. Buettner
 | Creation: 6/6/87
 | Acknowledgements:
 |      The ffasl.c file of the Franz Lisp System provided a template from
 |      which this loader was created.
 *=====================================================================*/
#include "defs.h"

#ifdef DynamicForeign

extern char imagename[];	/* name of image we are running - initial path */
extern char imagedir[];		/* directory containing image with final slash */

typedef void (*PFV) PARAMS(( void ));		/* A function pointer type */

#if defined(HAVE_DLOPEN)

#include <dlfcn.h>

PFV
load_object(objname, libstr, entry)
    char *objname;		/* name of .so file      */
    char *libstr;
    char *entry;		/* entry point          */
{
    void *handle;
    void *funcaddr = NULL;

    if ((handle = dlopen(objname, RTLD_LAZY)) == NULL
     || (funcaddr = dlsym(handle, entry)) == NULL) {
	PI_app_printf(PI_app_printf_warning, "Warning: load_object: %s\n",
	              dlerror());
	if (handle != NULL)
	    (void) dlclose(handle);
	return 0;
     }
     else
	return (PFV) funcaddr;
}

void foreign_shutdown()
{
    /* nothing to do */
}

#elif defined(SunOS)

#include <stdio.h>
#include <a.out.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

char *mktemp();

#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

PFV
load_object(name, libstr, entry)
    char *name;			/* name of .o file      */
    char *libstr;
    char *entry;		/* entry point          */
{

    char  tempname[32];
    char  stabname[1088];
    char *loadaddr;
    char  loadcmd[256];
    struct exec header;
    int   readsize, totsize, tfd;

    /*
     * Make the name of a temporary file where we will put the output of ld
     */

    strcpy(tempname, "/tmp/alsld.tmp.XXXXXX");
    mktemp(tempname);


    strcpy(stabname, imagedir);
    strcat(stabname, imagename);

    /*
     * Set loadaddr to the place in memory to start loading at.
     */

    loadaddr = (char *) ss_fmalloc_start();

    /*
     * Set loadcmd to the ln command needed to create the object to load
     */

    sprintf(loadcmd, "ld -N -x -A %s -T %lx %s -e _%s -o %s %s -lc",
	    stabname,
	    (long) loadaddr,
	    name,
	    entry,
	    tempname,
	    libstr);

    if (system(loadcmd) != 0) {
	unlink(tempname);
	return 0;
    }

    if ((tfd = open(tempname, 0)) < 0) {
	return 0;
    }

    if (read(tfd, (char *) &header, sizeof (header)) != sizeof (header)) {
	close(tfd);
	unlink(tempname);
	return 0;
    }

    readsize = round(header.a_text, 4) + round(header.a_data, 4);
    totsize = readsize + header.a_bss;
    totsize = round(totsize, 512);

    if ((long) ss_fmalloc(totsize) <= 0) {
	close(tfd);
	unlink(tempname);
	return (0);
    }

    if (read(tfd, loadaddr, readsize) != readsize) {
	close(tfd);
	unlink(tempname);
	return 0;
    }

    close(tfd);
    unlink(tempname);
    return (PFV) header.a_entry;
}

void
foreign_shutdown()
{
    /* Do nothing. */
}

#elif defined(HAVE_LIBLD)

/*-----------------------------------------------------------------------*
 * System V Coff File Loader
 *
 * The following code was initially supplied by Darin Deforest, but has been
 * substantially rewritten and reorganized by Kevin Buettner
 *-----------------------------------------------------------------------*/

/* These cause problems on certain systems, so undef them */

#undef TP_INT
#undef TP_DOUBLE

#include <stdio.h>
#include <filehdr.h>
#include <ldfcn.h>
#include <scnhdr.h>
#include <syms.h>
#include <setjmp.h>
#include <string.h>

#ifdef MISSING_EXTERN_LDOPEN
extern	LDFILE *ldopen		PARAMS(( CONST char *, LDFILE * ));
#endif
extern	int	ldclose		PARAMS(( LDFILE * ));
extern	int	ldshread	PARAMS(( LDFILE *, unsigned int, SCNHDR * ));
extern	int	ldtbread	PARAMS(( LDFILE *, long, SYMENT * ));
extern	char *	ldgetname	PARAMS(( LDFILE *, CONST SYMENT * ));
extern	int	ldnshread	PARAMS(( LDFILE *, CONST char *, SCNHDR * ));
extern	char *	mktemp		PARAMS(( char * ));
extern	int	unlink		PARAMS(( CONST char * ));

/*  a symbol table file is an binary object */
typedef struct {
    char *FileName;		/* the name of the file */
    LDFILE *ldfile;		/* a pointer into the file */
} IMAGE;			/* images are relocatable or absolute object
				 * files
				 */

typedef struct {
    SYMENT Syment;		/* symbol entry for image file */
    char  Name[128];		/* high-level name */
    int   Found;		/* indication if entry was found or not */
} SYMBOL;
SYMBOL Symbol;			/* symbol table */

typedef struct {
    char *TempName;		/* Pathname of Temp to merge into */
    char *CoffName;		/* Pathname of Coff file */
    char *ExecName;		/* Pathname of the symbol table for current
				 * state
				 */
    long  TextStart;		/* Address at which text segment starts */
    long  DataStart;		/* Address of the data segment */
    long  PrevBrk;		/* Value of the previous brk */
    IMAGE *Image;		/* current symbol image */
} ENVIRONMENT;
ENVIRONMENT Environment;	/* program environment */

static	void	fatal		PARAMS(( char *, char * ));
static	IMAGE *	CreateImage	PARAMS(( char * ));
static	void	DestroyImage	PARAMS(( IMAGE * ));
static	void	DestroyImages	PARAMS(( void ));
static	void	TraverseSectionsImage PARAMS(( IMAGE * ));
static	long	FindSymbolValue	PARAMS(( char * ));
static	long	GetSectionStart	PARAMS(( char * ));
static	char *	xtext_and_xdata_strings PARAMS(( long, long , char *, long ));
static	char *	GenerateCoffFile PARAMS(( IMAGE *, IMAGE * ));
static	IMAGE *	CreateSymbolTable PARAMS(( IMAGE *, IMAGE *, char * ));
static	void	CreateEnvironment PARAMS(( void ));
static	void	Load		PARAMS(( char *, char * ));

jmp_buf fatalbuf;

static void
fatal(fcn, msg)
    char *fcn;
    char *msg;
{
    (void) printf("loadforeign: %s\n", msg);
    longjmp(fatalbuf, 1);
    /* NOTREACHED */
}

/*
 *  allocate and initialize a binary file from a pool of binary descriptors.
 *  NOTE: we only need three images at most, 2 for the source merge and 1 for
 *        the destination merge.
 *
 */

#define NUMBEROFIMAGES 3
static IMAGE ImageMemory[NUMBEROFIMAGES];

static IMAGE *
CreateImage(FileName)
    char *FileName;
{
    IMAGE *Result;
    int   i;
    for (i = NUMBEROFIMAGES - 1; i >= 0; i--)
	if (ImageMemory[i].FileName == (char *) 0)
	    goto found;

    fatal("CreateImage", "Too many images!!");

found:

    Result = &ImageMemory[i];
    Result->FileName = FileName;

    if ((Result->ldfile = ldopen(FileName, Result->ldfile)) == (LDFILE *) NULL) {
	fatal("CreateImage", "cannot open image file");
    }
    return (Result);
}

static void
DestroyImage(Image)
    IMAGE *Image;
{
    ldclose(Image->ldfile);
    Image->FileName = (char *) 0;
}

static void
DestroyImages()
{
    int   i;

    for (i = 0; i < NUMBEROFIMAGES; i++)
	if (ImageMemory[i].FileName)
	    DestroyImage(ImageMemory + i);
}


static void
TraverseSectionsImage(Image)
    IMAGE *Image;
{
    unsigned int NumOfSections = HEADER(Image->ldfile).f_nscns;
    unsigned counter;
    SCNHDR secthead;
    long totalsize = 0;

    for (counter = 1; counter <= NumOfSections; counter++) {
	if (ldshread(Image->ldfile, counter, &secthead) == FAILURE)
	    continue;

	if (   secthead.s_vaddr
	    && strcmp(secthead.s_name, ".xtext") != 0
	    && strcmp(secthead.s_name, ".xdata") != 0)
	    totalsize += secthead.s_size;
    }

    (void) ss_fmalloc((size_t)totalsize);

    for (counter = 1; counter <= NumOfSections; counter++) {

	if (ldshread(Image->ldfile, counter, &secthead) == FAILURE) {
	    continue;
	}

	if (strcmp(secthead.s_name, ".xtext") == 0 ||
	    strcmp(secthead.s_name, ".xdata") == 0)
	    ; /* do nothing */
	else if (strcmp(secthead.s_name, ".bss") == 0)
	    (void) memset((char *) secthead.s_vaddr, 0, secthead.s_size);
	else if (secthead.s_vaddr) {
	    (void) FSEEK(Image->ldfile, secthead.s_scnptr, 0);
	    (void) FREAD((void *) secthead.s_vaddr,
			 secthead.s_size, 1, Image->ldfile);
	}
    }
    return;
}

/*
 * FindSymbolValue is given the symbol name to find and looks up the value
 * (address) if found and returns it.  If not found, it returns 0.  Hopefully,
 * 0 won't be a valid address.
 */

static long
FindSymbolValue(symbol_name_to_find)
    char *symbol_name_to_find;
{
    unsigned int NumOfSymbols = HEADER(Environment.Image->ldfile).f_nsyms;
    int   i;
    SYMENT symbol;
    char *name;

    for (i = 0; i < NumOfSymbols; i++) {
	if (ldtbread(Environment.Image->ldfile, i, &symbol) == FAILURE)
	    fatal("FindSymbolValue", "can't read symbol");
	if ((name = ldgetname(Environment.Image->ldfile, &symbol)) != NULL &&
	    strcmp(symbol_name_to_find, name) == 0) {
		return symbol.n_value;
	}
    }
    fatal("FindSymbolValue", "symbol not found");
    return (0);			/* This statement is not reached */
}

static long
GetSectionStart(section_name)
    char *section_name;
{
    SCNHDR sh;

    if (ldnshread(Environment.Image->ldfile, section_name, &sh) == FAILURE)
	fatal("GetSectionStart", "can't find section");;
    return sh.s_vaddr;
}

/*******************************************************************************
 *
 * this function has the only harded coded constants
 * one for start of .data
 * one for start of .text
 ******************************************************************************/
static char CoffFileContents[] =
"SECTIONS\n\
{\n\
\tGROUP %#x:\n\
\t{\n\
\t\t.text : {%s(.text)}\n\
\t\t.data : {%s(.data)}\n\
\t\t.bss : {%s(.bss)}\n\
\t}\n\
%s\n\
}\n";

static char *
xtext_and_xdata_strings(TextStart, DataStart, FileName, DynoDiff)
    long  TextStart;
    long  DataStart;
    char *FileName;
    long  DynoDiff;
{
    static int entered = 0;
    static char buf[1024];

    if (!entered) {
	entered = 1;
	sprintf(buf, "\
\t.xtext %#lx (NOLOAD) : {%s(.text)} \n\
\t.xdata %#lx (NOLOAD) : {%s(.data,.bss)} \n",
		TextStart, FileName, DataStart, FileName);
    }
    else {
	sprintf(buf, "\
\t.xtext %#lx (NOLOAD): {%s(.xtext)} \n\
\t.xdata %#lx (NOLOAD): {%s(.xdata)\n\t\t. += %#lx;\n\t\t%s(.text,.data,.bss)\n}",
	      TextStart, FileName, DataStart, FileName, DynoDiff, FileName);

    }

    return buf;
}

static char *
GenerateCoffFile(RelocImage, ExecuImage)
    IMAGE *RelocImage;
    IMAGE *ExecuImage;
{
    FILE *CoffFile;

    CoffFile = fopen(Environment.CoffName, "w");
    (void) fprintf(CoffFile, CoffFileContents,
		   (int) ss_fmalloc_start(),
		   RelocImage->FileName,
		   RelocImage->FileName, RelocImage->FileName,
		   xtext_and_xdata_strings(Environment.TextStart,
					   Environment.DataStart,
					   ExecuImage->FileName,
					   ((int) ss_fmalloc_start())
					   - Environment.PrevBrk)
	);
    (void) fclose(CoffFile);
    return (Environment.CoffName);
}

static IMAGE *
CreateSymbolTable(ExecuImage, RelocImage, LibStr)
    IMAGE *ExecuImage;
    IMAGE *RelocImage;
    char *LibStr;
{
    char  buffer[1024];
    IMAGE *Result;

    (void) sprintf(buffer, "ld -o %s %s %s\n",
		   Environment.TempName,
		   GenerateCoffFile(RelocImage, ExecuImage),
		   LibStr);
    if (system(buffer) != 0)
	fatal("CreateSymbolTable", "system called failed");

    if (strncmp(Environment.ExecName, "/tmp/ATF", 8) == 0)
	unlink(Environment.ExecName);

    strcpy(Environment.ExecName, Environment.TempName);
    Result = CreateImage(Environment.TempName);
    return (Result);
}



#define TFILENAMESIZE 256	/* max length of temp file names */

static void
CreateEnvironment()
{
    char *Space;
    static char CoffFileName[TFILENAMESIZE];
    static char TempFileName[TFILENAMESIZE];
    static char ExecFileName[TFILENAMESIZE];

    if (Environment.ExecName == (char *) 0) {

	Environment.TempName =
	    mktemp(strcpy(TempFileName, "/tmp/ATFXXXXXX"));
	Environment.CoffName =
	    mktemp(strcpy(CoffFileName, "/tmp/ACFXXXXXX"));
	Environment.ExecName = strcpy(ExecFileName, imagedir);
	strcat(ExecFileName, imagename);
	Environment.Image = CreateImage(Environment.ExecName);
	Environment.TextStart = GetSectionStart(".text");
	Environment.DataStart = GetSectionStart(".data");
    }
    else {
	Environment.TempName = mktemp(strcpy(TempFileName, "/tmp/ATFXXXXXX"));
	Environment.Image = CreateImage(Environment.ExecName);
    }
#define K 1024
    /* get space allocated for I/O buffer's and implicit mallocs upfront
     * because we can't have the memory space changing on us.
     */
    Space = malloc((unsigned) (1024 * K));
    (void) free(Space);
#undef K
    return;
}

static void
Load(FileName, LibStr)
    char *FileName;
    char *LibStr;
{
    IMAGE *NewImage;
    IMAGE *LoadImage = CreateImage(FileName);

    NewImage = CreateSymbolTable(Environment.Image, LoadImage, LibStr);
    TraverseSectionsImage(NewImage);
    DestroyImage(Environment.Image);
    DestroyImage(LoadImage);
    Environment.Image = NewImage;
    Environment.PrevBrk = (long) ss_fmalloc_start();
    return;
}

PFV
load_object(name, libstr, entry)
    char *name;			/* name of .o file      */
    char *libstr;		/* -l library options */
    char *entry;		/* entry point          */
{
    long  retval;

    if (setjmp(fatalbuf) != 0) {
	DestroyImages();
	if (Environment.CoffName)
	    unlink(Environment.CoffName);
	if (Environment.TempName)
	    unlink(Environment.TempName);
	return 0;
    }
    CreateEnvironment();
    Load(name, libstr);
    retval = FindSymbolValue(entry);
    DestroyImage(Environment.Image);
    unlink(Environment.CoffName);
#define CRAIG
#ifdef CRAIG
    if (Environment.ExecName && strncmp(Environment.ExecName, "/tmp/ATF", 8) == 0)
	unlink(Environment.ExecName);
    Environment.ExecName = (char *) 0;
#endif
    return ((PFV) retval);
}


void
foreign_shutdown()
{
    if (Environment.ExecName && strncmp(Environment.ExecName, "/tmp/ATF", 8) == 0)
	unlink(Environment.ExecName);
}

#endif /* HAVE_LIBLD */

#endif /* DynamicForeign */
