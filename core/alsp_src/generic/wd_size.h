/*===================================================================*
 |		wd_size.h
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |		Very Low-level word-size-specific stuff
 |
 | Extracted from various other files;  this file is probably
 | an intermediate step along the way to smoothly handling both
 | 32 bit and 64 bit architectures.
 |
 | Author: Ken Bowen
 | Created: 03/01/95 
 *===================================================================*/

/*
#ifndef BITS32 	

#else  

*/

/*-----------------------------------------------------------------------
 | Maximum and Minimum Prolog integers (originally from arith.h)
 *-----------------------------------------------------------------------*/

#ifdef __LP64__
#define MAXPROLOGINT (PWord)0x07ffffffffffffff		/* 60 bit integers */
#define MINPROLOGINT (PWord)0xf800000000000000
#else
#define MAXPROLOGINT (PWord)0x07ffffff		/* 28 bit integers */
#define MINPROLOGINT (PWord)0xf8000000
#endif

/*-----------------------------------------------------------------------
 | Arity which indicates that we have a big structure
 *-----------------------------------------------------------------------*/

#define ESCAPE_ARITY 255

/*-----------------------------------------------------------------------
 | Shifting amount & masks for functor name extraction
 *-----------------------------------------------------------------------*/

#ifdef __LP64__
#define FCTRSHIFT 56
#define FCTRTOKMASK 0xffffffffffffff
#else
#define FCTRSHIFT 24
#define FCTRTOKMASK 0xffffff
#endif
#define FCTRARTYMASK 0xff

/* #endif  BITS32/64 */
