/*
 * codegen.c	-- assist functions for macros in codegen.h
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/8/93
 * Revision History:
 *
 * Description:
 *	This file contains functions needed by the code generation macros
 *	in codegen.h.  The reason that the functions herein are coded
 *	as functions instead of macros is because
 *		1) The parameter(s) is/are used more than once
 *	or
 *		2) They are complicated enough to be coded as functions
 */

/*
 * CG_im14(i)
 *
 *	The parameter i will be converted to a 14 bit immediate with
 *	the sign bit as the low order bit in the word.  Yes, this seems
 *	goofy to me, but it is the format in which fourteen bit immediates
 *	are stored in the instruction word on HPPA.  It gets worse later
 *	on.
 */

int CG_im14(i)
    int i;
{
    return ((i & 0x2000) >> 13) | ((i & 0x1fff) << 1);
}

/*
 * CG_im5(i)
 *
 *	The parameter i will be converted to a 5 bit immediate with the
 *	sign bit as the low order bit of the word.
 */

int CG_im5(i)
    int i;
{
    return ((i & 0x10) >> 4) | ((i & 0xf) << 1);
}


/*
 * CG_im21(i)
 *	The parameter i will be converted to the form which the HPPA
 *	architecture expects a 21 bit signed immediate to be stored in
 *	an instruction word.
 *
 *	We must reverse the following transformation:
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|09|10|11|12|13|14|15|16|17|18|19|05|06|00|01|02|03|04|07|08|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *
 */

long CG_im21(i)
    long i;
{
    return(	((i & 0x100000) >> 20) |
		((i & 0x0ffe00) >> 8) |
		((i & 0x000180) << 7) |
		((i & 0x00007c) << 14) |
		((i & 0x000003) << 12)  );
}


/*
 * CG_im17(i)
 *	The parameter i (actually a displacement) will be converted to
 *	the form which the HPPA expects for a signed 17 bit displacement
 *	stored in an instruction word.
 *
 *	We must reverse the following transformation:
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|20|20|20|20|00|01|02|03|04|18|08|09|10|11|12|13|14|15|16|17|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 */

long CG_im17(i)
    long i;
{
    return(	((i & 0x10000) >> 16) |
		((i & 0x0f800) << 5) |
		((i & 0x00400) >> 8) |
		((i & 0x003ff) << 3)  );
}


/*
 * CG_spacereg(i)
 *
 *	Builds the representation for a space register and shifts it
 *	the appropriate amount for storing in an instruction word.
 */

long CG_spacereg(i)
    long i;
{
    return( ((i&4)<<11) | ((i&3)<<14) );
}


/*
 * CG_im12(i)
 *	The parameter i (actually a displacement) will be converted to
 *	the form which the HPPA expects for a signed 12 bit displacement
 *	stored in an instruction word.
 *
 *	We must reverse the following transformation:
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|20|20|20|20|20|20|20|20|20|18|08|09|10|11|12|13|14|15|16|17|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+

long CG_im12(i)
    long i;
{
    return(	((0x800 & i) >> 11) |
		((0x400 & i) >> 8) |
		((0x3ff & i) << 3)	);
}

/*
 * CG_im11(i)
 *	The parameter i is converted to a suitable form for storing in
 *	an HPPA instruction word.
 */

long CG_im11(i)
    long i;
{
    return(	((0x400 & i) >> 10) |
		((0x3ff & i) << 1)	);
}
