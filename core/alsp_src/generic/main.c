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
 | For code moved from pimain.c
 | 11/20/94 - C. Houpt -- Added Think/MetroWerks ccommand() call to allow
 |			      command line arguments for non-MPW versions.
 |			   -- Added include of pi_init header file to provide prototype.
 |			   -- Added PI_yield_time() to give other programs time.
*=============================================================*/
#include "defs.h"
#include <setjmp.h>
/* #include "winter.h" */

#include <limits.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef MacOS
#include <TextUtils.h>
#include <Processes.h>
#include <Errors.h>

#ifdef MPW_TOOL
#else
#include <unix.h>
#endif
#include <LowMem.h>
#endif

#ifdef MSWin32
#include <windows.h>
#include "fswin32.h"
#include "ctype.h"
#endif

#include "main.h"
#include "version.h"
#include "pckg.h"
#include "rinfo.h"
#include "module.h"

#ifdef DOS
#define X_OK 0
#define R_OK 1
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
 | images to set this value appropriately in the resulting image (alsmics)
 *------------------------------------------------------------------------*/
long  saved_state_image_offset = 0;

char library_path[PATH_MAX];
char library_dir[PATH_MAX];
char executable_path[PATH_MAX];

static char alsdir[PATH_MAX];	/* directory where ALS system resides */

#ifdef MSWin32
#if defined (WIN32)
	#define IS_WIN32 TRUE
#else
	#define IS_WIN32 FALSE
#endif
#define IS_NT      IS_WIN32 && (BOOL)(GetVersion() < 0x80000000)
#define IS_WIN32S  IS_WIN32 && (BOOL)(!(IS_NT) && ((GetVersion() & 0xFF)<4))
#define IS_WIN95 (BOOL)(!(IS_NT) && !(IS_WIN32S)) && IS_WIN32

const char *MinorOSStr = "mswindows";
int win32s_system = 0;
#endif

#ifdef UNIX
const char *version[2] = {
  "@(#)(c) 1997 Applied Logic Systems, Inc.",
  "@(#)ALS Prolog " VERSION_STRING " for " UNIX_NAME,
  };
#endif

static char versionNum[] = VERSION_STRING;	/* from version.h */
static char versionYear[] = VERSION_YEAR;	/* from version.h */
/* static char systemName[] = SysName;		from version.h */
static int exit_status = 0;
static jmp_buf exit_return; 

#ifdef __LP64__
static int heapWordBytes = 8;
#else
static int heapWordBytes = 4;
#endif

static	void	panic_fail	( void );
#ifdef arch_m88k
static	void	panic_continue	( void );
#endif
static	void	abolish_predicate ( const char *, const char *, int );
static	void	assert_sys_searchdir ( char * );
static	void	assert_als_system (const char *, const char *,
					  const char *, const char *,
					  const char *, const char *, int);
static	void	assert_atom_in_module ( const char*, const char * );


static	void	autoload	( char * );
static	void	chpt_init	( void );

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


/*---------------------------------*
 * Prolog initialization
 *---------------------------------*/

#ifdef Threaded
extern int run_wam(Code *startaddr);
#endif

/* ALS Prolog Command Line Development Evnvironment */

static int PI_prolog_init0(const PI_system_setup *setup)
{
    unsigned long heapsize;
    unsigned long stacksize;
#if	defined(arch_i386) || defined(arch_sparc) || defined(arch_m68k)
    unsigned long icbufsize;
#endif
    const char *state_path = NULL;
    int  saved_state_loaded;
    int offset = 0;

#ifdef MSWin32
    win32s_system = IS_WIN32S;
#endif

    /* Put arg and argv in globals so they can be used by the
       builtin get_argc_argv/2 */

    argcount = setup->argc;
    argvector = setup->argv;

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

    heapsize = setup->heap_size ? setup->heap_size : DEFAULT_HEAP_SIZE;
    stacksize = setup->stack_size ? setup->stack_size :DEFAULT_STACK_SIZE;
#if	defined(arch_i386) || defined(arch_sparc) || defined(arch_m68k)
    icbufsize = setup->icbuf_size ? setup->icbuf_size : MIN_ICBUFSIZE;
#endif    
    
    /*
     * get the image directory; the call to locate_executable() will initialize it and
     * the image name;
     */

#ifndef PURE_ANSI
    locate_library_executable(setup->argc, setup->argv);
#endif /* PURE_ANSI */

    /*
     * Initialize space for ss_malloc.  This should be done fairly early
     * on before other space is allocated.  If we wait until the
     * heap is allocated, we might not end up in the right place if
     * we have a saved state.
     */

    if (setup->saved_state) {
    	state_path = setup->saved_state;
    	offset = 0;
    } else if (setup->load_executable_state) {
    	offset = ss_image_offset(executable_path);
    	if (offset) state_path = executable_path;
    	else state_path = NULL;
    } else {
      offset = ss_image_offset(library_path);
      if (offset) state_path = library_path;
      else state_path = NULL;
    }    
    	
    /*
     * Set up the alsdir variable.  First use the image directory and
     * attach alsdir to the path.  If this directory does not exist then
     * just use the image directory.  It is this directory which we search
     * to find the builtins.
     */
#ifndef PURE_ANSI
	if (setup->alsdir) {
		strcpy(alsdir, setup->alsdir);
    } else {
    	strcpy(alsdir, library_dir);
    	strcat(alsdir, "alsdir");
    }

#ifdef VMS
    strcat(alsdir, ".dir");
#endif
    if (access(alsdir, R_OK | X_OK) == -1) 
	{
	/* not accessible; just use image directory */
	strcpy(alsdir, library_dir);
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
#endif /* PURE_ANSI */

#ifdef Threaded
    /* make sure that wam_instr is setup */
    run_wam(0);
#endif

	init_prolog_globals();
	init_prolog_engine(&current_engine, stacksize, heapsize);
    saved_state_loaded = als_mem_init(state_path, offset);

#if 0
//    if (heapsize * 4 < DEFAULT_SAFETY * 2)
//	fatal_error(FE_SMALLHEAP, 0);

//#ifndef KERNAL
//    if (stacksize * 4 < 65536)
//	fatal_error(FE_SMALLSTACK, 0);
//#endif /* KERNAL */

    /* Perform some initial allocations */

//    wm_stackbot = allocate_prolog_heap_and_stack(stacksize + heapsize);

//    wm_heapbase = wm_stackbot + stacksize;
//#ifdef MacOS
//    wm_stackbot_safety = wm_stackbot + 50;
//#endif
#endif

#ifdef	arch_m88k
    wm_CP = (long *) panic_continue;
#endif /* arch_m88k */

#if 0
//    wm_H = wm_HB = wm_SPB = wm_E = wm_SP = wm_heapbase;
//    wm_B = (long *) 0;
//    wm_gvbase = wm_trailbase = wm_TR = wm_heapbase + heapsize - 1;
//    wm_gvfreelist = (PWord *) MMK_INT(-1);
#ifndef Portable
//    wm_FAIL = (long *) panic_fail;
#else
//    wm_FAIL = (long *) wm_panic;
#endif

//    prs_area_init(heapsize / 8);
#endif
    prs_area_init(0);

#if	defined(arch_i386) || defined(arch_sparc) || defined(arch_m68k)
    if (icbufsize < MIN_ICBUFSIZE || !init_icode_buf(icbufsize))
	fatal_error(FE_ICODEBUFINIT, 0);
#endif /* arch_i386 or arch_sparc */

    parser_init();

    w_initcode();
#ifndef KERNAL
    fio_init();
#endif /* KERNAL */
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

#ifndef KERNAL
    if (system_pckg != (long *) -1 || saved_state_loaded)
	pckg_run_init_goal();
#endif /* KERNAL */

    /*
     * Intialize Foreign Interface preds used by builtins
     */

    init_fsutils();		/* see fsunix.c, fsdos.c, etc. */
#ifndef KERNAL
    cinterf_init();		/* see cinterf.c */
#endif /* KERNAL */

#ifndef KERNAL
    if (system_pckg != (long *) -1 || saved_state_loaded) {
	abolish_predicate("builtins", "als_system", 1);
#if 0	/* should leave original sys_searchdir since image may be moved */
	abolish_predicate("builtins", "sys_searchdir", 1);
#endif
    }

    assert_sys_searchdir(alsdir);
#endif /* KERNAL */

    /*---------------------------------------*
     | Set up the als_system fact.
     *---------------------------------------*/
     
#ifdef MSWin32


    if (IS_NT) MinorOSStr = "mswinnt";
    else if (IS_WIN32S) MinorOSStr = "mswin32s";
    else if (IS_WIN95) MinorOSStr = "mswin95";

#endif

#ifndef KERNAL
    assert_als_system(OSStr, MinorOSStr, ProcStr,
		      SysManufacturer, versionNum, versionYear, heapWordBytes);

    /*-------------------------------------------*
     | Set up conditional configuration controls:
     *-------------------------------------------*/


#if defined(__DJGPP__)
    assert_atom_in_module("syscfg","djgpp2");

#elif defined(__GO32__)
    assert_atom_in_module("syscfg","djgpp2");

#elif defined(MacOS)
    assert_atom_in_module("syscfg","macos");

#endif


#ifdef INTCONSTR 
    assert_atom_in_module("builtins","intconstr");
    assert_atom_in_module("syscfg","intconstr");
#endif

#ifdef FREEZE 
    assert_atom_in_module("builtins","freeze");
    assert_atom_in_module("syscfg","freeze");
#endif
#endif /* KERNAL */

#ifdef USE_IEEE_FP 
    assert_atom_in_module("syscfg","ieee_fp");
#endif

    /*---------------------------------------*
     | Load the builtins
     *---------------------------------------*/
    if (!noautoload && !saved_state_loaded)
	{
    	char f[20];
    	size_t l;

#ifdef KERNAL
	f_load("builtins.obp");
	if (argc >= 2) {
	    f_load(argv[1]);
	} else f_load("application.obp");
#else
    	strcpy(f, "builtins");
#ifndef PURE_ANSI
    	l = strlen(f);
    	f[l] = DIR_SEPARATOR; f[l+1] = 0;
    	strcat(f, "builtins");
#endif /* PURE_ANSI */
    	autoload(f);
#endif /* KERNAL */
    }

#if 0
#ifdef MacOS
#ifndef CW_PLUGIN
    /* load the autoload files, call the initilize routine. */
    {
    	Str255 pfile;
    	char file[256];
    	int status, i;
    	for (i = 1, GetIndString(pfile, 128, i); pfile[0]; i++, GetIndString(pfile, 128, i)) {
	    strncpy(file, pfile+1, pfile[0]);
	    file[pfile[0]] = 0; 
    	    status = obpres_load(file);
	    if (status != 1) fatal_error(FE_AUTOLOAD, (long)file);
    	}
    }
    
    {
	StringHandle p;
	char init[256];
	
	p = GetString(128);
     
	if (p && **p) {
	    PWord mv, gv;
	    int   mt, gt;
    	    strncpy(init, *p+1, **p);
    	    init[**p] = 0; 
	    PI_makesym(&mv, &mt, "user");
	    PI_makesym(&gv, &gt, init);
	    PI_rungoal(mv, gv, gt);
	}	
    }
#endif
#endif
#endif

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
    const char *module;
    const char *pred;
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
    PWord c, a1, a2, a4, b, s, p;
    int ct, a1t, a2t, a4t, bt, st, pt;
	
    PI_makesym(&c, &ct, "assertz");
    PI_makestruct(&c, &ct, c, 4);
	
    PI_getargn(&a1, &a1t, c, 1);
    PI_getargn(&a2, &a2t, c, 2);
    PI_getargn(&a4, &a4t, c, 4);

    PI_makesym(&b, &bt, "builtins");
    PI_unify(b, bt, a1, a1t);
	
    PI_makesym(&s, &st, "sys_searchdir");
    PI_makestruct(&s, &st, s, 1);
    PI_getargn(&a1, &a1t, s, 1);
    PI_makeuia(&p, &pt, name);
    PI_unify(p, pt, a1, a1t);
    
    PI_unify(s, st, a2, a2t);
    
    PI_unify(0, PI_INT, a4, a4t);
	
    if (!PI_rungoal(b, c, ct)) {
	fatal_error(FE_ASSERT_SSD, (long) name);
    }
}

/*-----------------------------------------------------------------------------*
 |	assert_atom_in_module(mod_name,atom_name)
 |	-- called to various 0-ary facts in the database prior to loading the 
 | 	builtins file; primarily used for modules builtins and syscfg.
 *-----------------------------------------------------------------------------*/
static void
assert_atom_in_module(mod_name,atom_name)
    const char *mod_name;
    const char *atom_name;
{
    char  command[2048];

    sprintf(command, "assertz(%s,%s,_,0)", mod_name,atom_name);
    if (!exec_query_from_buf(command)) {
		fatal_error(FE_ASSERT_SYS, 0);
    	}
}

/*-----------------------------------------------------------------------------*
 | assert_als_system 
 |	-- called to create the als_system/1 fact in the database prior to 
 |	loading the builtins file.
 *-----------------------------------------------------------------------------*/
static void
assert_als_system(os, os_var, proc, man, ver, year, hwb)
    const char *os, *os_var, *proc, *man, *ver, *year; int hwb;
{
    char  command[2048];

    if (noautoload && !pckgloaded)
		return;

    sprintf(command,
	    "assertz(builtins,als_system([os='%s',os_variation='%s',processor='%s',manufacturer='%s',prologVersion='%s',prologYear='%s',heapWordBytes=%d]),_,0)",
	    os,
	    os_var,
	    proc,
	    man,
	    ver,
	    year,
	    heapWordBytes);
    if (!exec_query_from_buf(command)) {
		fatal_error(FE_ASSERT_SYS, 0);
    	}
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
#ifdef MacOS
    status = obpres_load(fext);
    if (status != 1) status = load_file(fext, 0); 
#else
    status = load_file(fext, SUPPRESS_OBP);
#endif
    if (!status) {
/*
	PI_app_printf(PI_app_printf_warning,
		      "autoload: unable to load '%s'\n", f);
*/
	fatal_error(FE_AUTOLOAD, (long)f);
    }
}


EXPORT ALSPI_API(void)
PI_toplevel(void)
{
	int result;
	PI_status_toplevel(&result);
}

EXPORT ALSPI_API(int)
PI_status_toplevel(int *result)
{
#ifndef KERNAL
    if (noautoload && !pckgloaded) {
	if (!setjmp(exit_return)) {
	    read_loop(nt_query, PMPT_QUERY);
	    return (0);
	}
    }
    else
#endif /* KERNAL */
    {
	if (!setjmp(exit_return)) {
	    PWord mv, gv;
	    int   mt, gt;

#ifdef MacOS
	    {
		StringHandle p;
		char start[256];
		
		p = GetString(129);
	     
		if (p && **p) {
	    	    strncpy(start, *p+1, **p);
	    	    start[**p] = 0; 
		    PI_makesym(&mv, &mt, "user");
		    PI_makesym(&gv, &gt, start);
		    PI_rungoal(mv, gv, gt);
		    return (0);
		}	
	    }
#endif
#ifdef KERNAL
	    PI_makesym(&mv, &mt, "user");
#else
	    PI_makesym(&mv, &mt, "builtins");
#endif /* KERNAL */
	    PI_makesym(&gv, &gt, "$start");
	    {
	    PWord func, t, call, a;
	    int funct, tt, callt, at;
		PI_makesym(&func, &funct, "catch");
		PI_makesym(&t, &tt, "true");
		PI_makestruct(&call, &callt, func, 3);
		PI_getargn(&a, &at, call, 1);
		PI_unify(a, at, gv, gt);
		PI_getargn(&a, &at, call, 3);
		PI_unify(a, at, t, tt);
	    if (PI_rungoal_with_update(mv, &call, &callt)) {
	    	PI_getargn(&a, &at, call, 2);
	    	if (at == PI_VAR) *result = 1;
	    	else *result = 2;
	    } else *result = 0;
	    }
	    return (0);
	} else {
	    /* Set result to success on longjmp */
	    *result = 1;
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

static void
PI_shutdown0(void)
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
 | copyright is not called anywhere, but defining it this way will put a
 | copyright string into the executable in addition to getting -Wall off our
 | backs.  Who knows, maybe someday we will want to call it.
 *-------------------------------------------------------------------*/

extern	char *	copyright	( void );

char *
copyright()
{
    static char copyright_[] = "Copyright (c) 1994-6 Applied Logic Systems, Inc";
    return copyright_;
}



#ifdef MacOS
#ifdef HAVE_GUSI
#include <GUSI.h>
#endif

#include <CursorCtl.h>

int MPW_Tool;

#ifdef __MWERKS__
#include <console.h>
#include <SIOUX.h>

#include <Events.h>
#endif
#endif	/* MacOS */

#ifdef MSWin32
#ifdef HAVE_SOCKET
#include <winsock.h>
#endif

#if defined (WIN32)
	#define IS_WIN32 TRUE
#else
	#define IS_WIN32 FALSE
#endif
#define IS_NT      IS_WIN32 && (BOOL)(GetVersion() < 0x80000000)
#define IS_WIN32S  IS_WIN32 && (BOOL)(!(IS_NT) && ((GetVersion() & 0xFF)<4))
#define IS_WIN95 (BOOL)(!(IS_NT) && !(IS_WIN32S)) && IS_WIN32




#ifdef __MWERKS__
#include <signal.h>
#endif

HANDLE InteruptCompleteEvent;

BOOL CtrlHandler(DWORD fdwCtrlType);
BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType) {
    case CTRL_C_EVENT:
	raise(SIGINT);
	SetEvent(InteruptCompleteEvent);
	return TRUE;
    case CTRL_BREAK_EVENT:
   	abort();
	return TRUE;
    default:
    	return FALSE;
    }
}

#endif

#ifdef APP_PRINTF_CALLBACK
void app_printf(int messtype, va_list args);
#endif

#if defined(KERNAL) && defined(__MWERKS__) && defined(macintosh)
#include <console.h>
#endif

#ifdef macintosh
#pragma export on
#endif

EXPORT ALSPI_API(int)
PI_prolog_init(int argc, char *argv[])
{
  PI_system_setup setup;

    /* Fill setup struct with defaults */
    setup.heap_size = 0;
    setup.stack_size = 0;
    setup.icbuf_size = 0;
    setup.alsdir = NULL;
    setup.saved_state = NULL;
    setup.load_executable_state = 0;
    setup.argc = argc;
    setup.argv = argv;
#ifdef WIN32
    setup.hInstance = NULL;
    setup.hPrevInstance = NULL;
    setup.lpCmdLine = NULL;
    setup.nCmdShow = 1;
#endif
  
    return PI_startup(&setup);
}

#ifdef MacOS
static void StartCoop(void);
static void EndCoop(void);
#endif

#ifdef DEMO
extern void demo_check(void);
#endif

EXPORT ALSPI_API(int)
PI_startup(const PI_system_setup *setup)
{
#ifdef DEMO
	demo_check();
#endif
#ifdef MacOS
{
    /* Determine if we are running under MPW. */
    ProcessSerialNumber PSN;
    ProcessInfoRec info;
    OSErr err;
	
    PSN.highLongOfPSN = 0;
    PSN.lowLongOfPSN = kCurrentProcess;
	
    info.processInfoLength = sizeof(ProcessInfoRec);
    info.processName = NULL;
    info.processAppSpec = NULL;
	
    err = GetProcessInformation(&PSN, &info);
    if (err != noErr) return err;
 
    MPW_Tool = (info.processSignature == 'MPS '); 

    if (MPW_Tool) {
	InitGraf((Ptr) &qd.thePort);
	InitCursorCtl(NULL);
    }

#ifdef HAVE_GUSI
    GUSISetup(GUSIwithAppleTalkSockets);
    GUSISetup(GUSIwithInternetSockets);
    GUSISetup(GUSIwithPAPSockets);
    GUSISetup(GUSIwithPPCSockets);
    GUSISetup(GUSIwithUnixSockets);
    if (!MPW_Tool) GUSISetup(GUSIwithSIOUXSockets);
#endif

   StartCoop();
}
#endif /* MacOS */

#ifdef APP_PRINTF_CALLBACK 
    PI_set_app_printf_callback(app_printf);
#endif

#ifdef MSWin32
	
// This should be moved to the dev system code
    if (!(IS_WIN32S)) {
	if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE))
	    PI_app_printf(PI_app_printf_warning, "SetConsoleCtrlHandler failed !\n");
    }
    
    InteruptCompleteEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    
#ifdef HAVE_SOCKET
    {
	WORD wVersionRequested = MAKEWORD(1, 1);
	WSADATA wsaData;
	int r, success;
	
	r = WSAStartup(wVersionRequested, &wsaData);
	
	success = (r == 0);
	
	if (success && wsaData.wVersion != wVersionRequested) {
	    WSACleanup();
	    success = 0;
	}

	if (!success) {
	    PI_app_printf(PI_app_printf_warning, "WinSock init failed !\n");
	}
    }
#endif
    
#endif

#ifdef EXP_DATE
    if ((unsigned long) time(0) >= EXP_DATE) {
	PI_app_printf(PI_app_printf_error, "System validity date passed !\n");
	exit(1);
	}
#endif

    {
    int result = PI_prolog_init0(setup);

    return result;
    }

}

EXPORT ALSPI_API(void)
PI_shutdown(void)
{

    PI_shutdown0();

#ifdef MSWin32
#ifdef HAVE_SOCKET
    if (WSACleanup() != 0) {
	PI_app_printf(PI_app_printf_warning, "WinSock cleanup failed !\n");
    }
#endif
#endif

#ifdef MacOS
	EndCoop();
#endif

}

#ifdef macintosh
#pragma export reset
#endif


#ifdef MacOS
/* PI_yield_time() is called periodically during the execution of prolog code to
   allow other processes to execute.  The MacOS uses cooperative multitasking, rather
   than preemptive.
   
   yield_interval is the number of prolog procedures executed between calls to
   PI_yield_time. yield_interval may be adjusted to provide more or fewer yields.
   
   yield_counter is used as a count-down until the next yield.
 */

long yield_interval = 100;
long yield_counter = 100;

static long last_yield = 0;

long coop_interupt = 0;

void (*yield_proc)(void) = NULL;

ALSPI_API(void) PI_set_yield_proc(void (*p)(void))
{
	yield_proc = p;
}

void	PI_yield_time(void)
{
    long tick;
    tick = TickCount();	
    
    if (yield_proc) yield_proc();
    
    //if (MPW_Tool) SpinCursor(1);
    //else SIOUXHandleOneEvent(NULL);

    /* Adjust the yield interval upwards until there are at least 3 ticks between
       yields.  */
    if (tick - last_yield <= 3) yield_interval += 100;
    last_yield = tick;
    coop_interupt = 0;
}

#include <Timer.h>

TimerUPP TimeProcUPP;

TMTask gTask;
#define foo 1000

static pascal void TimeProc(TMTaskPtr task)
{
 	wm_safety = -1;
    coop_interupt = 1;
   
    task->tmAddr = TimeProcUPP;
    task->tmWakeUp = 0;
    task->tmReserved = 0;
    
    InsTime((QElemPtr)task);
    PrimeTime((QElemPtr)task, foo);
}

static void StartCoop(void)
{
	TimeProcUPP = NewTimerProc(TimeProc);
	
    gTask.tmAddr = TimeProcUPP;
    gTask.tmWakeUp = 0;
    gTask.tmReserved = 0;

    //InsTime((QElemPtr)&gTask);
    //PrimeTime((QElemPtr)&gTask, foo);
}

static void EndCoop(void)
{
    //RmvTime((QElemPtr)&gTask);
}



#if defined( __MWERKS__) && !defined(HAVE_GUSI)

#ifdef MPW_TOOL
#if __POWERPC__
#include <fcntl.h>
#else
#include <unix.h>
#endif
#else
#include <unix.h>
#endif

/* This a patch for metrowerk's open() function.  
   Hopefully this can be removed in a future release.
*/
int metrowerks_open_patch(const char *filename, int mode);
int metrowerks_open_patch(const char *filename, int mode)
{
#if defined(MPW_TOOL) && !__POWERPC__
  int mpw_mode;

/* MPW's open() mode values taken from {CIncludes}FCntl.h. */
#define MPW_O_RDONLY		 0 		/* Bits 0 and 1 are used internally */
#define MPW_O_WRONLY		 1 		/* Values 0..2 are historical */
#define MPW_O_RDWR 			 2		/* NOTE: it goes 0, 1, 2, *!* 8, 16, 32, ... */
#define MPW_O_APPEND	(1<< 3)		/* append (writes guaranteed at the end) */
#define MPW_O_RSRC 		(1<< 4)		/* Open the resource fork */
#define MPW_O_ALIAS		(1<< 5)		/* Open alias file */
#define MPW_O_CREAT		(1<< 8)		/* Open with file create */
#define MPW_O_TRUNC		(1<< 9)		/* Open with truncation */
#define MPW_O_EXCL 		(1<<10) 	/* w/ O_CREAT:  Exclusive "create-only" */
#define MPW_O_BINARY	(1<<11) 	/* Open as a binary stream */
#define MPW_O_NRESOLVE	(1<<14)		/* Don't resolve any aliases */

  switch(mode & 3) {
  case O_RDWR: mpw_mode = MPW_O_RDWR; break;
  case O_RDONLY: mpw_mode = MPW_O_RDONLY; break;
  case O_WRONLY: mpw_mode = MPW_O_WRONLY; break;
  };
  
  if (mode & O_APPEND) mpw_mode |= MPW_O_APPEND;
  if (mode & O_CREAT) mpw_mode |= MPW_O_CREAT;
  if (mode & O_EXCL) mpw_mode |= MPW_O_EXCL;
  if (mode & O_TRUNC) mpw_mode |= MPW_O_TRUNC;
  if (mode & O_BINARY) mpw_mode |= MPW_O_BINARY;
	
  mode = mpw_mode;
#endif

  return open(filename, mode);
}

#endif
#endif


/*
 * PI_app_printf is called from the prolog environment to display error and
 * warning messages.  The first parameter, messtype, describes the type
 * of message.  These types are defined in alspi.h.  The message type may be
 * used to route the message supplied in va_alist to the place appropriate
 * for the application.
 *
 * Kev's note to ALS implementers:  
 *	We should be careful to only call PI_app_printf once for each
 *	particular message from Prolog.  Also, we should not make
 *	too many assumptions about what kind of device we are writing
 *	to.  In other words, line control information such as \n should
 *	probably be removed from most of our messages.  It will then
 *	be the responsiblity of PI_app_printf to output newlines or
 *	pop up windows or whatever.  It should also be the responsiblity
 *	of PI_app_printf to prepend information about the type of message.
 *	See the fatal error case (below) as an example.
 */


/*VARARGS0 */
#ifdef APP_PRINTF_CALLBACK
void app_printf(int messtype, va_list args)
#else
EXPORT ALSPI_API(void)
PI_app_printf(int messtype, ...)
#endif
{
#ifndef APP_PRINTF_CALLBACK
    va_list args;
#endif
    char *fmt;
#ifdef MSWin32
    FILE *f;
    char s[500];
#endif

#ifndef APP_PRINTF_CALLBACK
    va_start(args, messtype);
#endif /* APP_PRINTF_CALLBACK */

    fmt = va_arg(args, char *);

#ifdef MSWin32
    switch (messtype) {
	case PI_app_printf_banner :
	case PI_app_printf_informational :
	    f = stdout;
	    vsprintf(s, fmt, args);
	    break;
	case PI_app_printf_fatal_error :
	    f = stderr;
	    strcpy(s,"\nFatal Error: ");
	    vsprintf(s+strlen(s), fmt, args);
	    strcat(s,"\n");
	    break;
	case PI_app_printf_warning :
	case PI_app_printf_error :
	default :
	    f = stderr;
	    vsprintf(s, fmt, args);
	    break;
    }
    
    if (IS_WIN32S) {
	MessageBox(GetFocus(), s, "ALS Prolog", 0);
    } else {
    	fprintf(f, "%s", s);
    }
#else
    switch (messtype) {
	case PI_app_printf_banner :
	case PI_app_printf_informational :
	    vfprintf(stdout, fmt, args);
	    break;
	case PI_app_printf_fatal_error :
	    fprintf(stderr,"\nFatal Error: ");
	    vfprintf(stderr, fmt, args);
	    fprintf(stderr,"\n");
	    break;
	case PI_app_printf_warning :
	case PI_app_printf_error :
	default :
	    vfprintf(stderr, fmt, args);
	    break;
    }
#endif
}

/* PI_get_options returns a string containing option settings for ALSPro.
   Usually this string comes from getenv(), except on the Mac where it
   is stored in a preferences file.
*/
EXPORT ALSPI_API(const char *)
PI_get_options(void)
{
#ifdef MacOS
    if (MPW_Tool) {
    	return getenv("ALS_OPTIONS");
    } else {
	/* This is a very simple version at the moment.  The preferences file
	   must be in the same directory as the application.  This should be
	   extended to also look in the preferences folder and system folder.
	   The preferences file consists of a single line with the standard
	   ALSPro options.
	*/
	FILE *pref_file;
	static char pref_str[256];

	pref_file = fopen("ALSPro Prefs", "r");

	if (pref_file) {
	    fgets(pref_str, 255, pref_file);
	    fclose(pref_file);
	    return pref_str;
	} else return NULL;
    }
#else
    return getenv("ALS_OPTIONS");
#endif
}
