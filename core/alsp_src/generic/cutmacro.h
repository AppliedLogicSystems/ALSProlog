/*
 * cutmacro.h
 *	Copyright (c) 1990-1993 Applied Logic Systems, Inc.
 *
 * Creation: 12/12/90
 * Author: Kevin A. Buettner
 *
 * isCutMacro is given an token id and arity and returns true if the pair
 * forms a cutmacro, false otherwise.  This definition depends on the order
 * or tokens appearing in tokini.h
 */

#define isCutMacro(tok,arity) ((tok) <= TK_CUT && \
				cut_macro_arities[(tok)] == (arity))

extern short cut_macro_arities[];
