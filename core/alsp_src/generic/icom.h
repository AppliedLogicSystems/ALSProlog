/*===========================================================*
 |			icom.h		
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- ICode command numbers
 |
 | Author: Kevin A. Buettner
 | Created: 2/12/87
 | 10/26/94,	C. Houpt -- Ifdefed out makeobp with OBP.
 *===========================================================*/

#ifndef	_ICOM_H_INCLUDED_
#define	_ICOM_H_INCLUDED_ 1


#define IC_INIT		-1		/* initialize icode buffer	*/
#define IC_ENDCLAUSE	-2		/* end a clause			*/
#define IC_ASSERTZ	-3		/* assertz the icode buffer	*/
#define IC_ASSERTA	-4		/* asserta the icode buffer	*/
#define IC_EXECQUERY	-5		/* execute a query		*/
#define IC_EXECCOMMAND	-6		/* execute a command		*/
#define IC_CHANGEMOD	-7		/* switch modules		*/
#define	IC_ADDUSE	-8		/* add a use declaration to the */
					/*   current module		*/
#define IC_ENDMODULE	-9		/* end the current module	*/
#define IC_NEWMODULE	-10		/* start a different module	*/
#define IC_EXPORTPRED	-11		/* export a predicate from 	*/
					/*   current module		*/
#define IC_1STARG	-12		/* key for first argument is 	*/
					/*   passed in			*/
#define IC_BEGINMACRO	-13		/* begin macro expansion	*/
#define IC_ENDMACRO	-14		/* end macro expansion		*/
#define IC_PUTMACRO	-15		/* puts down macro in defn area */
#define IC_ADDCLAUSE	-16		/* similar to assertz	*/
#define	IC_ICRESET	-18		/* Reset the .obp file		*/
#define IC_ADDTO_AUTOUSE -21
#define IC_ADDTO_AUTONAME -22		/* add to auto name		*/
#define IC_CREMODCLOSURE -24		/* creates a module closure	*/
#define IC_BEGINALLOC	-25
#define IC_ENDALLOC	-26

#ifdef OBP
extern	int	makeobp;
#endif /* OBP */

#endif	/* _ICOM_H_INCLUDED_ */
