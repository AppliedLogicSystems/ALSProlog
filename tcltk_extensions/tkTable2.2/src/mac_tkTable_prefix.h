/*
 * Macintosh Tcl must be compiled with certain compiler options to
 * ensure that it will work correctly.  The following pragmas are 
 * used to ensure that those options are set correctly.  An error
 * will occur at compile time if they are not set correctly.
 */

#if !__option(enumsalwaysint)
#error Tcl requires the Metrowerks setting "Enums always ints".
#endif

#if !defined(__POWERPC__)
#if !__option(far_data)
#error Tcl requires the Metrowerks setting "Far data".
#endif
#endif

#if !defined(__POWERPC__)
#if !__option(fourbyteints)
#error Tcl requires the Metrowerks setting "4 byte ints".
#endif
#endif

#if !defined(__POWERPC__)
#if !__option(IEEEdoubles)
#error Tcl requires the Metrowerks setting "8 byte doubles".
#endif
#endif

/*
 * The define is used most everywhere to tell Tcl (or any Tcl
 * extensions) that we are compiling for the Macintosh platform.
 */

#define MAC_TCL

/*
 * The following defines control the behavior of the Macintosh
 * Universial Headers.
 */

#define SystemSevenOrLater 1
#define STRICT_CONTROLS 1
#define STRICT_WINDOWS  1

#define MAC_TCL
#define TBL_VERSION "2.2"
#define TBL_COMMAND "table"
