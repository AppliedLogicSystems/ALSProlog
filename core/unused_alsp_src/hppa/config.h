/*
 * config.h for HPPA system
 *
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Creation: 3/25/93
 * Author: Kevin A. Buettner
 *
 * This file contains the various configuration parameters.  It defines
 * such things as the operating system, processor and degree to which
 * the system is implemented in assembler.
 */


/*
 * Define the processor (als_m68k, als_m88k, als_i386, als_vax, als_sparc,
 *	or als_hppa)
 *
 * ProcStr is the processor reported to Prolog through als_system
 */

#define	als_hppa	1
#define	ProcStr		"hppa"


/*
 * Define the operating system:
 *	SysV for system V (unix)
 *	SunOS for the sun operating system (probably any bsd system too)
 *	DOS for MS-DOS
 *	VMS for VMS
 *
 * OSStr is the string denoting the operating system (such as unix, dos, etc)
 * MinorOSStr is a string denoting a type or version of the operating system
 *	such as "sysV", "sun", "bsd4.3", etc.
 */

#define UNIX	1

/*
 * Need to complete config.h file later
 */
