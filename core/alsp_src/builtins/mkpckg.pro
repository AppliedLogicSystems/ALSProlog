/*======================================================================*
 * 		mkpckg.pro 	
 *		Copyright (c) 1991-92 Applied Logic Systems, Inc.
 *
 *		Create packages files containing ALS Prolog predicates
 *
 * Author: Ilyas Cicekli
 * Date  : 2/13/91
 *======================================================================*/

/*
 * Two .o files are created to represent the package "tblt".
 *
 *		bltns.o		Packaged predicates in modules "builtins","user"
 *		bltnsenv.o	Package token table of the package "tblt"
 */


:- force_libload_all.

:-	package(bltns).

