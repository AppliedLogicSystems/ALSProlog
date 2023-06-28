/*=======================================================================*
 |		fatal.h
 |	Copyright (c) 1990-1995 Applied Logic Systems, Inc.
 |
 |		-- fatal error codes
 |
 | Creation: 12/17/90
 | Author: Kevin A. Buettner
 |
 | Description:
 |	Defines the fatal error codes for the system.  If part of the system
 |	must die in a non-graceful manner, it should call fatal_error with
 |	the error code provided and one string argument if necessary.
 |
 |	The error messages are in fatal.c and provide a convenient 
 |	mechanism for changing them if necessary.
 *=======================================================================*/

#ifndef _FATAL_H_INCLUDED_
#define _FATAL_H_INCLUDED_ 1

extern	void	fatal_error	(int, long);

/* all error codes moved to alspi.h */

#endif /* _FATAL_H_INCLUDED_ */

