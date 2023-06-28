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

/*#undef DynamicForeign */
#ifdef DynamicForeign

typedef void (*PFV) ( void );		/* A function pointer type */

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
#include <stdlib.h>
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

extern	int	ldclose		( LDFILE * );
extern	int	ldshread	( LDFILE *, unsigned int, SCNHDR * );
extern	int	ldtbread	( LDFILE *, long, SYMENT * );
extern	char *	ldgetname	( LDFILE *, const SYMENT * );
extern	int	ldnshread	( LDFILE *, const char *, SCNHDR * );
extern	char *	mktemp		( char * );
extern	int	unlink		( const char * );

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

static	void	fatal		( char *, char * );
static	IMAGE *	CreateImage	( char * );
static	void	DestroyImage	( IMAGE * );
static	void	DestroyImages	( void );
static	void	TraverseSectionsImage ( IMAGE * );
static	long	FindSymbolValue	( char * );
static	long	GetSectionStart	( char * );
static	char *	xtext_and_xdata_strings ( long, long , char *, long );
static	char *	GenerateCoffFile ( IMAGE *, IMAGE * );
static	IMAGE *	CreateSymbolTable ( IMAGE *, IMAGE *, char * );
static	void	CreateEnvironment ( void );
static	void	Load		( char *, char * );

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

#include "alspi_slib.h"
#include "cinterf.h"

#ifdef MacOS
static QDGlobals *GetQD(void)
{
    return &qd;
}
#endif

typedef unsigned long library_reference;

typedef struct {
   library_reference library;
   alspi_init_func library_init;
   library_func_ptrs library_funcs;
} library_info;

#ifdef macintosh
static void SIOUXSetEventVector(short (*handler)(EventRecord *))
{

}
#endif

const alspi_func_ptrs alspi_funcs = {
    PI_forceuia,
    PI_getan,
    PI_getargn,
    PI_gethead,
    PI_gettail,
    PI_getdouble,
    PI_getstruct,
    PI_getsymname,
    PI_getuianame,
    PI_getuiasize,
    PI_makedouble,
    PI_makelist,
    PI_makestruct,
    PI_makesym,
    PI_makeuia,
    PI_allocuia,
    PI_vprintf,
    PI_vaprintf,
#ifdef APP_PRINTF_CALLBACK
    PI_vapp_printf,
#else
    NULL,
#endif
    PI_rungoal,
    PI_rungoal_with_update,
    PI_rungoal_with_update_and_catch,
    PI_unify,
    PrologInit,
    CI_get_integer,
    CI_get_double,
    sym_insert_2long,
    sym_insert_dbl,
    find_callback,
    PI_throw,
    PI_getball,
    PI_interrupt,
    library_dir,
    executable_path
#ifdef macintosh
    ,
    SIOUXIsAppWindow,
    SIOUXHandleOneEvent,
    SIOUXSetEventVector,
    GetQD
#endif
};

typedef enum {
    no_error,
    type_error,
    file_not_found_error,
    permission_error,
    not_plugin_error,
    memory_error,
    version_error,
    init_error,
    unknown_error,
    error_count
} error_type;

const char *error_type_name[error_count] = {
    "no_error",
    "type_error",
    "file_not_found_error",
    "permission_error",
    "not_plugin_error",
    "memory_error",
    "version_error",
    "init_error",
    "unknown_error"
};

typedef struct {
    error_type type;
    double native_code;
    const char *native_message;
} plugin_error;

static plugin_error os_load_plugin(const char *lib_name,
			    library_reference *library, alspi_init_func *init);

/* load_plugin(name, lib_info, result_code, os_specific_result) */
int prolog_load_plugin(void)
{
    PWord v1, v2, v3, v4, uia, s, os;
    int t1, t2, t3, t4, uiat, st, ost;
    plugin_error result = {no_error, 0, NULL};
    const char *lib_name;
    library_info lib_info;
    
    PI_getan(&v1, &t1, 1);
    PI_getan(&v2, &t2, 2);
    PI_getan(&v3, &t3, 3);
    PI_getan(&v4, &t4, 4);

    if ((t1 != PI_SYM && t1 != PI_UIA) || t2 != PI_VAR || t3 != PI_VAR || t4 != PI_VAR)
    	result.type = type_error;

    /* Load the library and initilize the library infomation record. */
    if (result.type == no_error) {
    	getstring((UCHAR **)&lib_name, v1, t1);

    	result = os_load_plugin(lib_name, &lib_info.library, &lib_info.library_init);
    }
    
    /* Check the library version. */
    if (result.type == no_error) {
    	int version;
    	version = lib_info.library_init(NULL, NULL);
    	if (version != ALSPI_DLIB_VERSION) {
	  result.type = version_error;
	  result.native_code = version;
	}
    }
    
    /* Initialize the plugin. */
    if (result.type == no_error) {
    	int r;
    	r = lib_info.library_init(&alspi_funcs, &lib_info.library_funcs);
    	if (r != 0) result.type = init_error;
    }
    
    /* Initialize the library's prolog by calling pi_init() */
    if (result.type == no_error) {
    	lib_info.library_funcs.pi_init();
    }
    
    /* Return the library information in a UIA if no errors were encountered. */
    if (result.type == no_error) {
    	PI_allocuia(&uia, &uiat, sizeof(library_info));
    	/*memcpy((void *)uia, &lib_info, sizeof(lib_info)); */
    	
    	PI_unify(v2, t2, uia, uiat); 
    }
    
    /* Return general and OS specific error codes. */
    
    PI_makesym(&s, &st, error_type_name[result.type]);
    	
    PI_unify(v3, t3, s, st);
    	
    if (result.native_message) {
    	PI_makeuia(&os, &ost, result.native_message);
    } else {
        PI_makedouble(&os, &ost, result.native_code);
    }
    
    PI_unify(v4, t4, os, ost);

    PI_SUCCEED;
}

/* os_load_plugin() handles all the OS specific details of loading plugins.
   Different OS have different nameing and searching schemes, error reporting, etc.
*/

#if defined(MacOS)
#include <Errors.h>
#include <CodeFragments.h>
#elif defined(MSWin32)
#include <windows.h>
#elif defined(HAVE_DL_H)
#include <dl.h>
#elif defined(HAVE_DLFCN_H)
#include <errno.h>
#include <dlfcn.h>
#endif

static plugin_error os_load_plugin(const char *lib_name,
			    library_reference *library, alspi_init_func *library_init)
#if defined(MacOS)
{
    Str255 lib_name_p;
    FSSpec lib_spec;
    CFragConnectionID connID;
    OSErr err;
    Ptr mainAddr;
    Str255 errName;
    Ptr sym_ptr;
    CFragSymbolClass sym_class;
    plugin_error result = {no_error, noErr, NULL};

    c2pstrcpy(lib_name_p, lib_name);
    err = FSMakeFSSpec (0, 0, lib_name_p, &lib_spec);
    
    if (err == noErr) { 		
	err = GetDiskFragment (&lib_spec, 0, kCFragGoesToEOF, lib_name_p,
			       kLoadCFrag, &connID, &mainAddr, errName); 
    }
    
    if (err == noErr) {
	err = FindSymbol (connID, "\palspi_dlib_init", &sym_ptr, &sym_class);
        if (sym_class != kTVectorCFragSymbol) err = cfragLibConnErr;
    	
    }   	
    
    if (err == noErr) {
    	*library = (unsigned long) connID;
    	*library_init = (alspi_init_func)sym_ptr;
    } else {
    	switch (err) {
	case nsvErr:
	case fnfErr:
	case cfragNoLibraryErr:
	    result.type = file_not_found_error;
	    break;
	case afpAccessDenied:
	    result.type = permission_error;
	    break;
	case cfragFragmentFormatErr:
	case cfragLibConnErr:
	    result.type = not_plugin_error;
	    break;	
	case cfragUnresolvedErr:
	case cfragInitOrderErr:
	case cfragInitLoopErr:
	case cfragImportTooOldErr:
	case cfragImportTooNewErr:
	case cfragInitFunctionErr:
	    result.type = init_error;
	    break;
	case cfragNoPrivateMemErr:
	case cfragNoClientMemErr:
	    result.type = memory_error;
	    break;
	default:
    	    result.type = unknown_error;
    	    break;
    	}
    	result.native_code = err;
    }
    
    return result;
}
#elif defined(MSWin32)
{
    HINSTANCE instance;
    FARPROC proc_addr;
    DWORD err = NO_ERROR;
    plugin_error result = {no_error, NO_ERROR, NULL};

    instance = LoadLibrary(lib_name);
    if (instance == NULL) err = GetLastError();
    
    if (err == NO_ERROR) {
	proc_addr = GetProcAddress(instance, "alspi_dlib_init");
    	if (proc_addr == NULL) err = GetLastError();    
    }
    
    if (err == NO_ERROR) {
    	*library = (unsigned long)instance;
    	*library_init = (alspi_init_func)proc_addr;
    } else {
    	switch (err) {
	case ERROR_DLL_NOT_FOUND:
	    result.type = file_not_found_error;
	    break;
	case ERROR_ACCESS_DENIED:
	    result.type = permission_error;
	    break;
/*
	When you try to load a non-DLL file, you get a ERROR_GEN_FAILURE,
	which seems to broad, but I'll use it for not_plugin_error.
*/
	case ERROR_GEN_FAILURE:
	    result.type = not_plugin_error;
	    break;
	case ERROR_DLL_INIT_FAILED:
	    result.type = init_error;
	    break;
	case ERROR_NOT_ENOUGH_MEMORY:
	    result.type = memory_error;
	    break;
	default:
    	    result.type = unknown_error;
    	    break;
    	}
    	result.native_code = err;
    }

    return result;
}
#elif defined(HAVE_DL_H)
{
    shl_t object;
    void *sym_addr = NULL;
    char current_dir[1000];
    int err = 0;
    plugin_error result = {no_error, 0, NULL};

    /* Set the current directory to the library_path, so that shared
       objects in the executable's directory are loaded first. */

    if (getcwd(current_dir, 1000) == NULL) {
      result.type = unknown_error;
      goto exit;
    }

    if (chdir(library_dir) != 0) {
      result.type = unknown_error;
      result.native_code = errno;
      goto exit;
    }
    
    object = shl_load(lib_name, BIND_IMMEDIATE | BIND_VERBOSE, 0);
    if (object == NULL) err = errno;

    if (err == 0) {
    	int r;
	r = shl_findsym(&object, "alspi_dlib_init", TYPE_PROCEDURE, &sym_addr);
    	if (r != 0) err = errno;
    }
    
    if (err == 0) {
	*library = (unsigned long)object;
	*library_init = sym_addr;
    } else {
    	switch (err) {
	case ENOENT:
	    result.type = file_not_found_error;
	    break;
	case EACCES:
	    result.type = permission_error;
	    break;
	case ENOEXEC:
	    result.type = not_plugin_error;
	    break;	
	case ENOSYM:
	    result.type = init_error;
	    break;
	case ENOMEM:
	    result.type = memory_error;
	    break;
    	default:
    	    result.type = unknown_error;
    	}
    	result.native_code = err;
    }

    chdir(current_dir);

 exit:

    return result;
}
#elif defined(HAVE_DLFCN_H)
{
    void *object, *sym_addr = NULL;
    const char *err = NULL;
    char current_dir[1000], *full_name;
    plugin_error result = {no_error, 0, NULL};
    
    /* Set the current directory to the library_path, so that shared
       objects in the executable's directory are loaded first. */

    if (getcwd(current_dir, 1000) == NULL) {
      result.type = unknown_error;
      goto exit;
    }

    
    if (chdir(library_dir) != 0) {
      result.type = unknown_error;
      result.native_code = errno;
      goto exit;
    }

    /* Make a copy of the lib_name, so we can add prefixes. */

    full_name = malloc(strlen(lib_name)+3); /* leave room for "./" */
    if (full_name == NULL) {
    	result.type = memory_error;
	goto exit;
    }

    /* Solaris and Linux both have a funny rule about
       finding shared object libraries.  When the path
       is just a filename (i.e. "foo.so"), dlopen() will
       NOT look in the current directory.  Absolute and
       relative paths with at least one "/" work as
       expected.

       To get consistent behaviour across platforms,
       I'll prepend a "./" to filenames to force Solaris
       and Linux to look in the current directory.
     */

    full_name[0] = 0;
    
    if (!strchr(lib_name, '/')) {
    	strcpy(full_name, "./");
    }
    
    strcat(full_name, lib_name);

    object = dlopen(full_name, RTLD_LAZY /* | RTLD_GLOBAL*/);
    if (object == NULL) err = dlerror();

    if (err == NULL) {
    	sym_addr = dlsym(object, "alspi_dlib_init");
    	if (sym_addr == NULL) err = dlerror();
    }

    if (err == NULL) {
	*library = (unsigned long)object;
	*library_init = sym_addr;
    } else {
    	result.type = unknown_error;
    	result.native_message = err;
    }

    chdir(current_dir);

    exit:

    return result;
}
#else
{
    plugin_error result = {unknown_error, 0, NULL};
    
    return result;
}
#endif
