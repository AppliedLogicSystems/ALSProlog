/*
 *	config.h for Vax system.
 *
 */

/*
 * Define the processor (m68k, m88k, i386, Vax)
 * ProcStr is the processor reported to Prolog through als_system.
 */

#define als_Vax	1	
#define ProcStr "vax"

/* Define the operating system
 *	SysV for system V (unix)
 *	SunOS for the sun operating system (probably any bsd system too)
 *	DOS for MS-DOS
 *	VMS for VMS
 * OSStr is a string denoting the operating system (such as unix, dos, etc)
 * MinorOSStr is a string denoting a type or version of the operating system
 *	such as "sysV", "sun", "bsd4.3", etc.
 *
 */

#define VMS 1
#define OSStr	"VMS"
#define MinorOSStr "VMS"


/*
 * Define WINS if there is a windowing system
 * and specify the default windowing system
 *      MOTIFWINS, OLWINS, NEXTWINS, DECWINS, DOSWINS etc.
 */

#define WINS 1
#define DECWINS 1

/* Define InMath if inline math */

/* Define FMath if inline floating math */

/* Define DynamicForeign if dynamic loading of foreign code */

/* Define CMeta if certain meta-builtins are written in C */

/* Define CODEGC if Code Garbage collection is implemented */

#define CodeGC 1

/* Define AUTOINDEXING if stable procedures will generate indexing for
 * themselves.
 */

#define AUTOINDEXING 1

/* Define SPY if implementation is to the point where spy points can be
 * installed in the procedure entry
 */

#define SPY 1

/*
 * Define OBP if .obp files are to be supported.
 */

#define OBP 1

/*
 * Indexing indicates that indexing code is generated.
 */

#define Indexing 1

/* 
 * Define SymLink if the operating system supports symbolic links and the
 * readlink call
 * 
 */


/*
 * Define DoubleType if the floating point types are implemented in a direct
 * fashion.  If DoubleType is not defined, floats and doubles are represented
 * as $double(D1,D2,D3,D4) where D1-D4 are 16 bit pieces of the double.
 */

/*
 * Define MotorolaMath if the Motorola math builtins are defined and
 * implemented for the system.
 */


/*
 * Define the system dependent directory and path separators for parsing path
 * lists
 *
 *			UNIX	VMS	DOS	Atari
 * Path Separator:	:	,	;	,
 * Dir Separator:	/	]	\	\
 */

#define PATH_SEPARATOR ','
#define DIR_SEPARATOR ']'
