/*
 *	config.h 
 *	Copyright (c) 1988-91 Applied Logic Systems, Inc.
 *
 *| Configuration file for 386/486 systems.
 */


 
/*
 * Define the processor (als_m68k, als_m88k, als_i386, als_vax, als_sparc)
 * ProcStr is the processor reported to Prolog through als_system.
 */
#define als_i386	1
#define ProcStr		"i386"  


/* 
 * Define the operating system 
 * 		DOS for MS-DOS
 * 		SunOS for the sun operating system
 *		SysV for system V unix
 *		Xenix for xenix operating system
 * OSStr is a string denoting the operating system (such as unix, dos, etc)
 *
 * Define the DOS Extender for DOS 
 * 		PharLap for PharLap DOS-Extender
 * 		Ergo    for Ergo DOS-Extender
 * MinorOSStr is a string denoting a type or version of the operating system
 * such as "pharlap", "ergo", "xenix", "sysV", "sun" etc.
 */

#define UNIX        1
#define SysV		1
#define SCO_UNIX    1

#define OSStr		"unix"  
#define MinorOSStr	"sysV"  


/* 
 * Define OBP if .obp files should be handled. 
 */
#define OBP	1


/*
 * Define SlowCut if code for performing cut is not expanded inline; that is
 * cut is a goal like any other goal.
 */
#define SlowCut 1



/* 
 * Define InMath if inline math 
 */
#define InMath 	1


/* 
 * Define FMath if linline floating math 
 */


/* 
 * Define DynamicForeign if dynamic loading of foreign code 
 */


/* 
 * Define CMeta if certain meta-builtins are written in C 
 */
#define CMeta	1


/* 
 * Define SPY if spypoints work 
 */
#define SPY 	1


/* 
 * Define Indexing if indexing works 
 */
#define Indexing 1


/* Define AUTOINDEXING if stable procedures will generate indexing for
 * themselves.
 */
#define AUTOINDEXING 1

/* 
 * Define CodeGC if the code garbage collector works 
 */
#define CodeGC 	1


/*
 * Define SymLink if the operating system supports symbolic links and the
 * readlink call
 *
 */
#ifdef SunOS
#define SymLink 1
#endif
 

/*
 * Define ChangeStack if we are going to use our own stack as the system stack
 */
#ifdef DOS
#define ChangeStack 1
#endif


/*
 * Define the system dependent directory and path separators for parsing path
 * lists
 *
 *          		UNIX    VMS 	DOS 	Atari
 * Path Separator:  :   	,   	;   	,
 * Dir Separator:   /   	]   	\  	 	\
 */
#ifdef DOS
#define PATH_SEPARATOR ';'
#define DIR_SEPARATOR '\\'
#else
#define PATH_SEPARATOR ':'
#define DIR_SEPARATOR '/'
#endif
 

/* 
 * Define PACKAGE if packaging system is going to be included 
 */
#define PACKAGE  1


#ifdef PACKAGE
/*
 * Define PCKG_NO_UNDERBAR if C compiler doesn't put "_" in front of names
 * (It is defined when als_i386 is defined)
 *
 * Define one of
 *
 *      PCKG_OBJFORMAT_COFF         when als_i386 and SysV are defined
 *      PCKG_OBJFORMAT_OMF86        when als_i386 and DOS are defined
 *      PCKG_OBJFORMAT_AOUT         when als_m68k is defined
 *      PCKG_OBJFORMAT_88KBCS       when als_m88k is defined
 *      PCKG_OBJFORMAT_SPARC_AOUT   when als_sparc is defined
 *
 * depending on object file formats.
 */
 
#ifdef als_i386
#ifdef DOS
#define PCKG_OBJFORMAT_OMF86    	1
#define PCKG_NO_UNDERBAR        	1
#else   /* DOS */
#define PCKG_OBJFORMAT_COFF     	1
#define PCKG_NO_UNDERBAR        	1
#endif  /* DOS */
#endif  /* als_i386 */
 
#ifdef als_m68k
#define PCKG_OBJFORMAT_AOUT     	1
#endif  /* als_m68k */
 
#ifdef als_m88k
#define PCKG_OBJFORMAT_88KBCS   	1
#endif  /* als_m88k */
 
#ifdef als_sparc
#define PCKG_OBJFORMAT_SPARC_AOUT   1
#endif  /* als_sparc */
 
#endif  /* PACKAGE*/
 

#define BIG_STRUCT 1
