/*
 *	config.h for the Portable Prolog system
 */

/* Define the processor */

#define Portable  1
#define ProcStr	"portable"

/* Define the size of opcode : LongCode, ShortCode, ByteCode */

#define ByteCode  1

/*
 * Define the operating system 
 * Currently one of : DOS, SunOS, SysV, Xenix, VMS, AtariOS 
 */

#define VMS	1
#define OSStr	"vms"
#define MinorOSStr "vms"


/*
 * Define WINS if there is a windowing system
 * and specify the default windowing system
 *      MOTIFWINS, OLWINS, NEXTWINS, DECWINS, DOSWINS etc.
 */

#define WINS 1
#define DECWINS 1

/*
 * Define WAM register model 
 * LargeRegModel  for 88k machines
 * SmallRegModel  for i386 machines
 * DataRegModel   for m68k, Vax machines
 * NoRegModel	  for test purposes
 */ 

#define DataRegModel 1

/* Define OBP if .obp files should be handled. */

#define OBP 1

/* Define InMath if inline math */

/* Define FMath if linline floating math */

/* Define DynamicForeign if dynamic loading of foreign code */

/* Define CMeta if certain meta-builtins are written in C */

#define CMeta	1

/* Define SPY if spypoints work */

#define SPY 1

/* Define Indexing if indexing works */

#define Indexing 1

/* Define CodeGC if the code garbage collector works */

#define CodeGC 1

/* Define AUTOINDEXING if stable procedures will generate indexing for
 * themselves.
 */

#define AUTOINDEXING 1


/* Define PACKAGE if packaging system is going to be included */

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
