/*
 * aconfig.h for the Portable Prolog system
 *
 * These parameters may be overridden by the machine dependent include file,
 * mconfig.h.
 */


#define Portable  1
#define ProcStr	"portable"

/* Define the size of opcode : LongCode, ShortCode, ByteCode */
#define LongCode	1

/* Define Threaded to be 1 if threaded code interpreter is desired -- must
 * use gcc to get threaded code interpreter
 */
#define Threaded	1

/*
 * Define WAM register model 
 * LargeRegModel  for m88k, sparc, mips, and other RISCS
 * SmallRegModel  for i386 machines
 * DataRegModel   for m68k, Vax machines
 * NoRegModel	  for test purposes
 */ 

#define LargeRegModel	1	/* may want to undef this in mconfig.h */
#define CMeta		1


/*
 * Don't have inline math code (yet!)
 */
#undef NewMath
#undef InMath
#undef FMath
