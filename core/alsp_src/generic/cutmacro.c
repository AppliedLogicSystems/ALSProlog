/*
 * cutmacro.c   -- table of arities for the cutmacros
 *      Copyright (c) 1992-1993 Applied Logic Systems, Inc.
 *
 * Author:      Kevin A. Buettner
 * Creation:    4/17/92
 */

short cut_macro_arities[] =
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
