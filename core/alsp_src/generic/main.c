/*=============================================================*
 |			main.c       
 |      Copyright (c) 1986-1995 Applied Logic Systems
 |
 |			-- main driver for ALS Prolog Systems
 |
 | Author: Kevin A. Buettner
 | Creation: 6/19/85
 | Revision History:
 | 01/15/86 - K. Buettner -- PC Prolog Port
 | 08/14/86 - K. Buettner -- Sun Port
 | 11/09/88 - K. Buettner -- Motorola foreign interface
 | 11/13/89 - K. Buettner -- New alsdir conventions
 | 06/12/91 - I. Cicekli  -- Merged with 386 version
 | 06/03/92 - R. DiNapoli -- Macintosh Mods
 | 08/29/92 - R. DiNapoli -- Parameterized product name
 |                           in banner (see version.h)
 | 06/21/93 - P. Raman -- moved main() to pimain.c
 | 11/29/94 - C. Houpt -- Added OBP ifdef control around call to fix_MAGIC.
 |			-- Added winter.h include rather than redeclare wm_regs[][].
 |			-- Added trap patch to disable stack overflow errs.
 |			-- Made "builtins/builtins" general w.r.t. directory separator.
 *=============================================================*/
#include "defs.h"
#include <setjmp.h>
/* #include "winter.h" */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if 0			/* FIXME:  Can we get rid of this stuff? */
#ifdef BSDUNIX
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#endif

#ifdef SysV
#include <unistd.h>
#include <fcntl.h>
#ifdef arch_m88k
#include <sys/m88kbcs.h>
#endif /* arch_m88k */
#endif /* SysV */
#endif /* 0 */

#ifdef MacOS
#ifdef THINK_C
#include <LoMem.h>
#else
#include <LowMem.h>
#endif
#if defined(THINK_C) || defined(applec)
#define LMSetStackLowPoint(value) ((* (Ptr *) 0x0110) = (value))
#endif
#endif

#include "main.h"
#include "version.h"
#include "pckg.h"
#include "rinfo.h"
#include "module.h"

#ifdef DOS
#define X_OK 0
#define R_OK 1
#elif !defined(R_OK)
#define X_OK 1
#define R_OK 4
#endif

int   system_debugging = 0;	/* -D to set it to 1 */
int   gcbeep = 0;		/* -B to set it to 1 */

static int noautoload = 0;
static int pckgloaded = 0;

/*------------------------------------------------------------------------*
 | saved_state_image_offset is the offset to the saved state information
 | in the image file.  If this value is zero, there is no saved state
 | information and the builtins should be loaded from the standard place.
 | It is the responsiblity of the utility which merges saved states and
 | images to set this value appropriately in the resulting image.
 *------------------------------------------------------------------------*/
long  saved_state_image_offset = 0;

char  imagename[64];
char  imagedir[1024];
static char alsdir[1024];	/* directory where ALS system resides */

static char versionNum[] = SysVersionNum;	/* from version.h */
/* static char systemName[] = SysName;		from version.h */
static int exit_status = 0;
static jmp_buf exit_return; 

static	void	panic_fail	PARAMS(( void ));
#ifdef arch_m88k
static	void	panic_continue	PARAMS(( void ));
#endif
static	char *	isopt		PARAMS(( char *, char * ));
static	void	abolish_predicate PARAMS(( char *, char *, int ));
static	void	assert_sys_searchdir PARAMS(( char * ));
static	void	assert_als_system PARAMS(( char *, char *, char *, char *,
				    char *, char * ));
static	void	assert_command_line PARAMS(( int, char ** ));
static	void	assert_atom_in_builtins PARAMS(( char * ));
#ifndef MacOS
static	int	absolute_pathname PARAMS((CONST char * ));
#endif
static	void	whereami	PARAMS(( char * ));
static	void	autoload	PARAMS(( char * ));
static	void	chpt_init	PARAMS(( void ));

static void
panic_fail()
{
    fatal_error(FE_PANIC_FAIL, 0);
}

#ifdef arch_m88k
static void
panic_continue()
{
    fatal_error(FE_PANIC_CONTINUE, 0);
}
#endif /* arch_m88k */

void
heap_overflow()
{
    fatal_error(FE_OVER_HEAP, 0);
}

static char *
isopt(opt,str)
    char *opt;
    char *str;
{
    size_t len = strlen(opt);
    if (strncmp(opt,str,len) == 0)
	return str+len;
    else
	return 0;
}

/*---------------------------------*
 * Prolog initialization
 *---------------------------------*/

int
PI_prolog_init(win_str, argc, argv)
    char *win_str;
    int   argc;
    char **argv;
{
    unsigned long heapsize;
    unsigned long stacksize;
    unsigned long icbufsize;
    char *saved_state_filename;
    char *als_opts;
    int  saved_state_loaded;

#ifdef Portable
    extern Code *wm_panic;
#endif /* Portable */

    /*-------------------------------------------------------------------*
     * malloc and then free an area at the outset so that the malloc
     * allocator will have some space to work with.  The hope is that
     * the brk (on systems that have such a thing) will get moved at
     * this point and then will not get moved again until after we've
     * called als_mem_init.
     *-------------------------------------------------------------------*/

    free(malloc(8192));

#if defined(MacOS) && !defined(Portable)
	{
    char *punchaddr, *punchee;
	extern void rts_end(void);
    extern wm_fail(), wm_trust_fail();

    /*-------------------------------------------------------------------*
	 * The unusual nature of the way the Mac addresses C functions (via a
     * Jump Table, we must bypass the normal mechanism and get at the real
     * machine address of wm_fail().
     *-------------------------------------------------------------------*/

	/* call rts_end(), which does nothing, to insure that the jump
	   table entries for its segment are loaded. */
	rts_end();

    punchaddr = (char *) wm_trust_fail;
    punchaddr = (char *) *(long *) (punchaddr + 2);
    punchaddr += 4;
    punchee = (char *) wm_fail;
    punchee = (char *) *(long *) (punchee + 2);
    *(long *) punchaddr = (long) punchee;
    }

    /* Disable stack overflow checking. */
    
    LMSetStackLowPoint((Ptr)0);

    /* Initilize math dispatch tables. */

    init_math();

#endif /* MacOS */

#if defined(DOS)
    /*
     * Check the dos extender
     * Create a code window in data segment in Ergo Environment
     */
    if ((check_dos_extender() == 0) || (create_code_window() == 0))
	return (-1);
#endif /* DOS */

    /*
     * set up exit return to be used if not in PI_toplevel.
     */
    if (setjmp(exit_return))
	return (exit_status);

#ifdef NO_FAR_DATA
    /* Initilize global arrays that are too big for Think's compiler. */
    init_capturestructs();
    init_compiler_data();
    init_cinterf_data();
    init_varproc_data();
    init_expand_data();
    init_parser_data();
#endif

    heapsize = DEFAULT_HEAP_SIZE;
    stacksize = DEFAULT_STACK_SIZE;
    icbufsize = MIN_ICBUFSIZE;
    saved_state_filename = (char *) 0;

    als_opts = getenv("ALS_OPTIONS");

    if (als_opts) {
	char *opt, *val;
	als_opts = strdup(als_opts);
	if (als_opts == NULL)
	    fatal_error(FE_ALS_OPTIONS, 0);

	opt = strtok(als_opts, " ,");
	while (opt) {
	    if ( (val = isopt("heap_size:",opt)) )
		heapsize = atoi(val) * 1024 / 4;
	    else if ( (val = isopt("stack_size:",opt)) )
		stacksize = atoi(val) * 1024 / 4;
	    else if ( (val = isopt("saved_state:",opt)) ) {
		saved_state_filename = strdup(val);
		if (saved_state_filename == 0)
		    fatal_error(FE_ALS_OPTIONS, 0);
	    }
	    else if ( (val = isopt("icbuf_size:",opt)) )
		icbufsize = atoi(val) * 1024;
	    else if ( (val = isopt("debug_shell", opt)) && *val == 0)
		noautoload = 1;
	    else
		PI_app_printf(PI_app_printf_warning,
			      "unrecognized option: %s\n", opt);
	    opt = strtok(NULL, " ,");
	}

	free(als_opts);
    }

    /*
     * get the image directory; the call to whereami will initialize it and
     * the image name;
     */
    whereami(argv[0]);

    /*
     * Initialize space for ss_malloc.  This should be done fairly early
     * on before other space is allocated.  If we wait until the
     * heap is allocated, we might not end up in the right place if
     * we have a saved state.
     */
    
    if (saved_state_image_offset) {
	char *imagepath = (char *) malloc(strlen(imagename)+strlen(imagedir)+1);
	strcpy(imagepath,imagedir);
	strcat(imagepath,imagename);
	if (imagepath == NULL)
	    fatal_error(FE_ALS_MEM_INIT, 0);
	saved_state_loaded = als_mem_init(imagepath, saved_state_image_offset);
	free(imagepath);
    }
    else
	saved_state_loaded = als_mem_init(saved_state_filename,0);

    /*
     * Set up the alsdir variable.  First use the image directory and
     * attach alsdir to the path.  If this directory does not exist then
     * just use the image directory.  It is this directory which we search
     * to find the builtins.
     */
#ifdef MacOS
    {
	const char *s;
	s = getenv("ALSDirectory");
	if (s)
	{
	    strcpy(alsdir, s);
	} else {
	    strcpy(alsdir, imagedir);
	    strcat(alsdir, "alsdir");
	}
    }
    strcpy(alsdir, imagedir);
    strcat(alsdir, "alsdir");
#else
    strcpy(alsdir, imagedir);
    strcat(alsdir, "alsdir");
#endif /* MacOS */

#ifdef VMS
    strcat(alsdir, ".dir");
#endif
    if (access(alsdir, R_OK | X_OK) == -1) 
	{
	/* not accessible; just use image directory */
	strcpy(alsdir, imagedir);
    }
    else {
	char *chptr;

	/* accessible, attach the directory separator character */
#ifndef VMS
	chptr = alsdir + strlen(alsdir);
	*chptr++ = DIR_SEPARATOR;
	*chptr = '\0';
#else  /* VMS */
	strcpy(alsdir, imagedir);
	strcpy(alsdir + strlen(alsdir) - 1, ".alsdir]");
#endif
    }

    if (heapsize * 4 < DEFAULT_SAFETY * 2)
	fatal_error(FE_SMALLHEAP, 0);

    if (stacksize * 4 < 65536)
	fatal_error(FE_SMALLSTACK, 0);

    /* Perform some initial allocations */

#if 1
    wm_stackbot = allocate_prolog_heap_and_stack(stacksize + heapsize);
#endif 
    wm_heapbase = wm_stackbot + stacksize;

#ifdef MacOS
    wm_stackbot_safety = wm_stackbot + 50;
#endif

#ifdef	arch_m88k
    wm_CP = (long *) panic_continue;
#endif /* arch_m88k */

    wm_H = wm_HB = wm_SPB = wm_E = wm_SP = wm_heapbase;
    wm_B = (long *) 0;
    wm_gvbase = wm_trailbase = wm_TR = wm_heapbase + heapsize - 1;
    wm_gvfreelist = (PWord *) MMK_INT(-1);
#ifndef Portable
    wm_FAIL = (long *) panic_fail;
#else
    wm_FAIL = (long *) wm_panic;
#endif

    prs_area_init(heapsize / 8);


#if	defined(arch_i386) || defined(arch_sparc) || defined(arch_m68k)
    if (icbufsize < MIN_ICBUFSIZE || !init_icode_buf(icbufsize))
	fatal_error(FE_ICODEBUFINIT, 0);
#endif /* arch_i386 or arch_sparc */

    parser_init();
    w_initcode();
    fio_init();
#ifdef Portable
    wam_init();		/* This must come before module_init */
#endif
    module_init();
    chpt_init();

#ifdef PACKAGE
    if (system_pckg != (long *) -1) {
	noautoload = 1;
	pckgloaded = 1;
	builtin_addr_table_init();
	pckg_init();
    }
    else {
	builtin_addr_table_init();
	builtin_init();
    }
#else  /* PACKAGE */
    builtin_init();
#endif /* PACKAGE */

    time_cut_interrupt_init();
#ifdef OBP
/*    fix_MAGIC(); */		/* for loadfile.c */
#endif /* OBP */

    if (system_pckg != (long *) -1 || saved_state_loaded)
	pckg_run_init_goal();

    /*
     * Intialize Foreign Interface preds used by builtins
     */

    init_fsutils();		/* see fsunix.c, fsdos.c, etc. */
    cinterf_init();		/* see cinterf.c */

    if (system_pckg != (long *) -1 || saved_state_loaded) {
	abolish_predicate("builtins", "command_line", 1);
	abolish_predicate("builtins", "als_system", 1);
#if 0	/* should leave original sys_searchdir since image may be moved */
	abolish_predicate("builtins", "sys_searchdir", 1);
#endif
    }

    assert_command_line(argc, argv);
    assert_sys_searchdir(alsdir);

    /*---------------------------------------*
     | Set up the als_system fact.
     *---------------------------------------*/
    assert_als_system(OSStr, MinorOSStr, ProcStr,
		      SysManufacturer, versionNum, win_str);

    /*---------------------------------------*
     | Set up any conditional configuration
     | controls:
     *---------------------------------------*/
#ifdef INTCONSTR 
    assert_atom_in_builtins("intconstr");
#endif

    /*---------------------------------------*
     | Load the builtins
     *---------------------------------------*/
    if (!noautoload && !saved_state_loaded)
	{
    	char f[20];
    	size_t l;
    	
    	strcpy(f, "builtins");
    	l = strlen(f);
    	f[l] = DIR_SEPARATOR; f[l+1] = 0;
    	strcat(f, "builtins");
    	autoload(f);
    }

    /*---------------------------------------*
     * Establish the Control/C (or Control/BREAK) handler
     *---------------------------------------*/
    init_sigint();

    return (0);
}


/*
 * abolish_predicate abolishes the given predicate
 */
static void
abolish_predicate(module, pred, arity)
    char *module;
    char *pred;
    int   arity;
{
    char  command[2048];

    sprintf(command, "abolish(%s,%s,%d)", module, pred, arity);
    if (!exec_query_from_buf(command)) {
	sprintf(command, "%s:%s/%d", module, pred, arity);
	fatal_error(FE_ABOLISH_FAIL, (long) command);
    }
}



/*
 * assert_sys_searchdir is called to place the fact
 *      sys_searchdir(Dir)
 * in the Prolog database (builtins module).  Since the builtins have not yet
 * been loaded, it is necessary to use the most primitive version of assert.
 */
static void
assert_sys_searchdir(name)
    char *name;
{
    char  command[2048];
#ifdef DOS
    char  tbuf[2048], *tptr, *nptr;
#endif

    if (noautoload && !pckgloaded)
	return;

#ifdef DOS
    /* replace \ by \\ to make parser happy */

    for (tptr = tbuf, nptr = name; *nptr != '\0'; *tptr++ = *nptr++)
	if (*nptr == '\\')
	    *tptr++ = '\\';
    *tptr = '\0';
    name = tbuf;
#endif /* DOS */

    sprintf(command, "assertz(builtins,sys_searchdir('%s'),_,0)", name);
    if (!exec_query_from_buf(command)) {
	fatal_error(FE_ASSERT_SSD, (long) name);
    }
}

/*-----------------------------------------------------------------------------*
 *-----------------------------------------------------------------------------*/
static void
assert_atom_in_builtins(atom_name)
    char *atom_name;
{
    char  command[2048];
    sprintf(command, "assertz(builtins,%s,_,0)", atom_name);
    if (!exec_query_from_buf(command)) {
	fatal_error(FE_ASSERT_SYS, 0);
    }
}

/*-----------------------------------------------------------------------------*
 * assert_als_system is called to place certain information about the
 * system in the database prior to loading the builtins file.
 *-----------------------------------------------------------------------------*/
static void
assert_als_system(os, os_var, proc, man, ver, winstype)
    char *os, *os_var, *proc, *man, *ver, *winstype;
{
    char  command[2048];

    if (noautoload && !pckgloaded)
	return;

    sprintf(command,
	    "assertz(builtins,als_system([os='%s',os_variation='%s',processor='%s',manufacturer='%s',prologVersion='%s',wins='%s']),_,0)",
	    os,
	    os_var,
	    proc,
	    man,
	    ver,
	    winstype);
    if (!exec_query_from_buf(command)) {
	fatal_error(FE_ASSERT_SYS, 0);
    }
}


/*-----------------------------------------------------------------------------*
 * assert_command is called to place the fact
 *      command_line(ListofCommandLineArgs)
 * in the Prolog database (builtins module). Since the builtins have not yet
 * been loaded, it is necessary to use the most primitive version of assert.
 *-----------------------------------------------------------------------------*/

static void
assert_command_line(count, args)
    int   count;
    char **args;
{
    int   c;
    register char *from, *to;
    char  command[4096];

    if (noautoload && !pckgloaded)
	return;

    /*
     * Construct a prolog command for asserting the new command line
     */

    from = "assertz(builtins,command_line([";
    to = command;

    while ( (*to++ = *from++) ) /* copy initial part of command */
	;

    to--;			/* back up over the null terminator */

    while (count--) {
		from = *args;
		*args++ = (char *) 0;
		*to++ = '\'';
		while ( (c = *from++) ) {
	    	if (c == '\'') {
				*to++ = '\'';
	    	}
	    	*to++ = c;
		}
		*to++ = '\'';
		if (count) {
	    	*to++ = ',';
		}
    }

    from = "]),_,0)";
    while ( (*to++ = *from++) )    /* copy trailing part of command */
	;

    	/* Assert the new command line */

    if (!exec_query_from_buf(command)) {
		fatal_error(FE_ASSERT_COM, 0);
    }
}


/*-----------------------------------------------------------------------------*
 * absolute_pathname tests to see if we have an absolute pathname or not
 *-----------------------------------------------------------------------------*/

#if defined(DOS) || defined(AtariOS) || defined(__GO32__) || defined(OS2)

static int
absolute_pathname(name)
    const char *name;
{
    return (
	       (*name == DIR_SEPARATOR) ||
	 (((*name >= 'a' && *name <= 'z') || (*name >= 'A' && *name <= 'Z'))
	  && *(name + 1) == ':' && *(name + 2) == DIR_SEPARATOR)
	);
}

#elif defined(VMS)

static int
absolute_pathname(name)
    const char *name;
{
    char *n;

    for (n = name; *n && *n != ':'; n++) ;

    return (*n);

}

#elif defined(MacOS)

/* Moved to fsmac.c */

#else  /* default is Unix style path specification */

static int
absolute_pathname(name)
    const char *name;
{
    return ( *name == DIR_SEPARATOR);
}

#endif


/*--------------------------------------------------------------------*
 | whereami is given a filename f and returns the directory in which 
 | the executable file (containing this code) may be found.  A dot will be 
 | returned to indicate the current directory.
 *--------------------------------------------------------------------*/

static void
whereami(name)
    char *name;
{
    register char *cutoff = NULL;	/* stifle -Wall */
    register char *s;
    register char *t;
    int   cc;
    char  ebuf[4096];

    /*
     * See if the file is accessible either through the current directory
     * or through an absolute path.
     */

    if (access(name, R_OK) == 0) {

	/*-------------------------------------------------------------*
	 * The file was accessible without any other work.  But the current
	 * working directory might change on us, so if it was accessible
	 * through the cwd, then we should get it for later accesses.
	 *-------------------------------------------------------------*/

	t = imagedir;
	if (!absolute_pathname(name)) {
#ifdef DOS
	    int   drive;
	    char *newrbuf;

	    newrbuf = imagedir;
	    if (*(name + 1) == ':') {
		if (*name >= 'a' && *name <= 'z')
		    drive = (int) (*name - 'a' + 1);
		else
		    drive = (int) (*name - 'A' + 1);
		*newrbuf++ = *name;
		*newrbuf++ = *(name + 1);
		*newrbuf++ = DIR_SEPARATOR;
	    }
	    else {
		drive = 0;
		*newrbuf++ = DIR_SEPARATOR;
	    }
	    if (getcwd(newrbuf, drive) == 0) {	/* } */
#else  /* DOS */
#ifdef HAVE_GETWD
	    if (getwd(imagedir) == 0) {		/* } */
#else  /* !HAVE_GETWD */
	    if (getcwd(imagedir, 1024) == 0) {
#endif /* !HAVE_GETWD */
#endif /* DOS */
		fatal_error(FE_GETCWD, 0);
	    }
	    for (; *t; t++)	/* Set t to end of buffer */
		;
	    if (*(t - 1) == DIR_SEPARATOR)	/* leave slash if already
						 * last char
						 */
		cutoff = t - 1;
	    else {
		cutoff = t;	/* otherwise put one in */
		*t++ = DIR_SEPARATOR;
	    }
	}

	/*-------------------------------------------------------------*
	 * Copy the rest of the string and set the cutoff if it was not
	 * already set.  If the first character of name is a slash, cutoff
	 * is not presently set but will be on the first iteration of the
	 * loop below.
	 *-------------------------------------------------------------*/

	for (s = name;;) {
	    if (*s == DIR_SEPARATOR)
		cutoff = t;
	    if (!(*t++ = *s++))
		break;
	}

    }
    else {

	/*-------------------------------------------------------------*
	 * Get the path list from the environment.  If the path list is
	 * inaccessible for any reason, leave with fatal error.
	 *-------------------------------------------------------------*/

#ifdef MacOS
	if ((s = getenv("Commands")) == (char *) 0)
#else
	if ((s = getenv("PATH")) == (char *) 0)
#endif
	    fatal_error(FE_PATH, 0);

	/*
	 * Copy path list into ebuf and set the source pointer to the
	 * beginning of this buffer.
	 */

	strcpy(ebuf, s);
	s = ebuf;

	for (;;) {
	    t = imagedir;
	    while (*s && *s != PATH_SEPARATOR)
		*t++ = *s++;
	    if (t > imagedir && *(t - 1) == DIR_SEPARATOR) 
		;		/* do nothing -- slash already is in place */
	    else
		*t++ = DIR_SEPARATOR;	/* put in the slash */
	    cutoff = t - 1;	/* set cutoff */
	    strcpy(t, name);
	    if (access(imagedir, R_OK) == 0)
		break;

	    if (*s)
		s++;		/* advance source pointer */
	    else
		fatal_error(FE_INFND, 0);
	}

    }

    /*-------------------------------------------------------------*
     | At this point the full pathname should exist in imagedir and
     | cutoff should be set to the final slash.  We must now determine
     | whether the file name is a symbolic link or not and chase it down
     | if it is.  Note that we reuse ebuf for getting the link.
     *-------------------------------------------------------------*/

#ifdef HAVE_SYMLINK
    while ((cc = readlink(imagedir, ebuf, 512)) != -1) {
	ebuf[cc] = 0;
	s = ebuf;
	if (*s == DIR_SEPARATOR) {
	    t = imagedir;
	}
	else {
	    t = cutoff + 1;
	}
	for (;;) {
	    if (*s == DIR_SEPARATOR)
		cutoff = t;	/* mark the last slash seen */
	    if (!(*t++ = *s++))	/* copy the character */
		break;
	}
    }

#endif /* HAVE_SYMLINK */

    strcpy(imagename, cutoff + 1);	/* keep the image name */
    *(cutoff + 1) = 0;		/* chop off the filename part */
}

/*-------------------------------------------------------------*
 | autoload will attempt to load the named file from
 | the alsdir directory.  It will print a warning if
 | unable to do so, and exit with a fatal error.
 *-------------------------------------------------------------*/

static void
autoload(f)
    char *f;
{
    int   status = 0;
    char  fext[1024];

    strcpy(fext, alsdir);
    strcat(fext, f);
/*
#ifndef OLDCLOAD
    strcat(fext, ".obp");
#endif 
*/
    status = load_file(fext, 0);  

    if (!status) {
	PI_app_printf(PI_app_printf_warning,
		      "autoload: unable to load '%s'\n", f);
/*	fatal_error(FE_PANIC_FAIL, 0);    */
    }
}

int
PI_toplevel()
{
    if (noautoload && !pckgloaded) {
	if (!setjmp(exit_return)) {
	    read_loop(nt_query, PMPT_QUERY);
	    return (0);
	}
    }
    else {
	if (!setjmp(exit_return)) {
	    PWord mv, gv;
	    int   mt, gt;

	    PI_makesym(&mv, &mt, "builtins");
	    PI_makesym(&gv, &gt, "$start");
	    PI_rungoal(mv, gv, gt);
	    return (0);
	}
    }

    return (exit_status);
}

void
als_exit(status)
    int   status;
{
    exit_status = status;

    longjmp(exit_return, 1);
}

void
PI_shutdown()
{
#ifdef DynamicForeign
    foreign_shutdown();		/* cleanup temp files created by foreign
				 * interface
				 */
#endif

    /*
     * Restore  the Control/C (or Control/BREAK) handler
     */
    reset_sigint();
}

#ifdef DOS
extern int _access();

int
access(fname, mode)
    char *fname;
    int   mode;
{
    return (_access(fname, mode));
}

#endif /* DOS */

static void
chpt_init()
{
    wm_E = wm_SP - 2;
    wm_E[0] = (PWord) wm_SP;
    wm_E[1] = 0;		/* mark_fromretaddr checks for this */
    wm_SPB = wm_SP = wm_E;

    wm_TR -= chpt_SIZE;
    chpt_NextClause(wm_TR) = (Code *) panic_fail;
    chpt_SPB(wm_TR) = (PWord *) wm_SPB;
    chpt_HB(wm_TR) = (PWord *) wm_H;
    chpt_B(wm_TR) = wm_B;
    wm_B = wm_TR;
}

/*-------------------------------------------------------------------*
 | string.h replacements for some of the functions which are not
 | universally available.
 *-------------------------------------------------------------------*/

#ifndef HAVE_STRDUP
/*-------------------------------------------------------------------*
 | strdup returns a pointer to a new string which is a duplicate of the
 | string pointed to by s1.  The space for the new string is obtained using
 | malloc.  If the new string can not be created, a NULL pointer is returned.
 *-------------------------------------------------------------------*/

char *
strdup(s1)
    CONST char *s1;
{
    char *dup;

    if (s1 == NULL)
	return NULL;

    dup = malloc(strlen(s1));
    if (dup == NULL)
	return NULL;

    strcpy(dup, s1);
    return dup;
}
#endif	/* HAVE_STRDUP */

#ifndef HAVE_STRSPN
/*-------------------------------------------------------------------*
 | strspn returns the length of the initial segment of string s1 which
 | consists entirely of characters from string s2.
 *-------------------------------------------------------------------*/

size_t
strspn(s1, s2)
    CONST char *s1;
    CONST char *s2;
{
    register size_t count;
    register int c;
    register CONST char *p;

    count = 0;

    while ( (c = *s1++) ) {
	p = s2;
	while (*p != c)
	    if (*p++ == 0)
		goto done;
	count++;
    }
done:
    return count;
}
#endif /* HAVE_STRSPN */

#ifndef HAVE_STRCSPN
/*-------------------------------------------------------------------*
 | strcspn returns the length of the initial segment of string s1 which
 | consists entirely of characters not from string s2.
 *-------------------------------------------------------------------*/

size_t strcspn(s1, s2)
    CONST char *s1;
    CONST char *s2;
{
    register size_t count;
    register int c;
    register CONST char *p;

    count = 0;

    while ( (c = *s1++) ) {
	p = s2;
	while (*p)
	    if (*p++ == c)
		goto done;
	count++;
    }
done:
    return count;
}
#endif /* HAVE_STRCSPN */

#ifndef HAVE_STRTOK
/*-------------------------------------------------------------------*
 | strtok considers the string s1 to consist of a sequence of zero or more
 | text tokens separated by spans of one or more characters from the
 | separator string s2.  The first call (with pointer s1 specified) returns
 | a pointer to the first character of the first token, and will have written
 | a null character into s1 immediately following the returned token.  The
 | function keeps track of its position in the string between separate calls,
 | so that subsequent calls (which must be makde with the first argument a
 | NULL pointer) will work throught the string s1 immediately following that
 | token.  In this way subsequent calls will work through the string s1 until
 | no tokens remain.  The separator string s2 may be different from call to
 | call.  When no token remains in s1, a NULL pointer is returned.
 *-------------------------------------------------------------------*/

char *
strtok(s1,s2)
    char *s1;
    CONST char *s2;
{
    static char *oldpos = 0;
    char *start, *end;

    if (s1)
	start = s1;
    else
	start = oldpos;
    
    if (start) {
	start += strspn(start,s2);
	end = start + strcspn(start,s2);
	if (*end) {
	    *end = 0;
	    oldpos = end+1;
	}
	else
	    oldpos = NULL;
	if (*start == 0)
	    start = NULL;
    }
    return start;
}
#endif	/* HAVE_STRTOK */


/*-------------------------------------------------------------------*
 | copyright is not called anywhere, but defining it this way will put a
 | copyright string into the executable in addition to getting -Wall off our
 | backs.  Who knows, maybe someday we will want to call it.
 *-------------------------------------------------------------------*/

extern	char *	copyright	PARAMS(( void ));

char *
copyright()
{
    static char copyright_[] = "Copyright (c) 1994-5 Applied Logic Systems, Inc";
    return copyright_;
}
