/*================================================================*
 |		cutmacro.c   
 |	Copyright (c) 1992-1995 Applied Logic Systems, Inc.
 |
 |		-- table of arities for the cutmacros
 |
 | Author:		Kevin A. Buettner
 | Creation:    4/17/92
 *================================================================*/

const short cut_macro_arities[] =
{
    0,				/* unused */
    1,				/* call */
    2,				/* callWithDelayedInterrupt */
    2,				/* dbg_call */
    3,				/* $semicolon */
    3,				/* $comma */
    3,				/* $arrow */
    2,				/* ; */
    2,				/* , */
    2,				/* : */
    2,				/* | */
    2,				/* -> */
    0,				/* ! */
};
