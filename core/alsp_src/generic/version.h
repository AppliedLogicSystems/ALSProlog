/*
 *	version.h		
 *	Copyright (c) 1990-1993 by Applied Logic Systems, Inc.
 *
 *| Version numbers for various systems
 *
 * Author: Kevin Buettner
 * Created: 1/25/90
 * Revision History:
 *   8/29/92, by Ron:   Since the Macintosh product will be called
 *                      ALS-Prolog/Native Code, I parameterized the product
 *                      name in main.c so that the Mac's unique product name
 *                      could be incorporated into the Merge technology...
 *                      Banner format is now:
 *                        %s Version %s     Serial Number: %s
 *                        SysName, SysVersionNum, SerialNum
 */


#ifdef arch_m68k
#ifdef MacOS
#define SysName "ALS-Prolog/Native Code"
#else
#define SysName "ALS-Prolog"
#endif
#define SysManufacturer "generic"
#endif 


#ifdef arch_m88k
#define SysName "ALS-Prolog"
#define SysManufacturer "generic"
#endif 


#ifdef arch_vax
#define SysName "ALS-Prolog"
#define SysManufacturer "generic"
#endif


#ifdef arch_sparc
#define SysName "ALS-Prolog"
#define SysManufacturer "generic"
#endif


#ifdef Portable
#ifdef Threaded
#define SysName "ALS-Prolog (Threaded)"
#else	/* !Threaded  -> Byte */
#define SysName "ALS-Prolog (Byte)"
#endif	/* Threaded */
#define SysManufacturer "generic"
#endif


#ifdef arch_i386
#define SysName "ALS-Prolog"
#define SysManufacturer  "generic"
#endif

#ifndef SysVersionNum
#define SysVersionNum "1.66B"
#endif /* SysVersionNum */
