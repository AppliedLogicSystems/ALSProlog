/*===============================================================*
 |			alloc.h              
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1993 by Applied Logic Systems, Inc.
 |
 |			-- runtime structure definitions and accessing macros 
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/15/85
 | Revision History:
 | 08/31/85,  Kevin A. Buettner -- Merged alloc32.h with alloc.h
 | 01/13/86,  Kevin A. Buettner -- PC mods
 | 06/24/87,  Kevin A. Buettner -- SUN changes
 *===============================================================*/
#ifndef ALLOC_I
#define ALLOC_I 1

/* Allocation constants */

/* #define PARSER_AREASIZ 65536  */   

#ifdef KERNAL
#define PARSER_AREASIZ 5000
#else
#define PARSER_AREASIZ 131072     /* size of parser's scratch area; input
                                  * transformations are also done in this
                                  * area
                                  */
#endif /* KERNAL */

/*
 * A pword is the basic prolog word out of which all compiler data structures
 * are constructed.  It is always a pointer to a tag and a tag dependent
 * value.
 */

/* Also defined by Win32 headers. */
typedef unsigned char tag_byte;
typedef struct pwrd_ {
                  tag_byte tag;
                  struct pwrd_ *val;
               } pwrd;
typedef pwrd * pword;



/*
 * Other memory areas
 */

extern pword prs_area;

/*
 * Types of objects
 */

#define TP_TERM 0	/* term						*/
#define TP_INT  1	/* integer					*/
#define TP_LIST 2	/* list cell					*/
#define TP_SYM  3	/* constants and functors			*/
#define TP_RULE 4	/* flattened rule which compiler uses		*/
#define TP_VO   5	/* variable offset (used in compiler)		*/
#define TP_NIL	6	/* internal constant nil (not the same as [])	*/
#define TP_UIA	7	/* uninterned atoms				*/
#define TP_DOUBLE 8	/* floating point numbers			*/


#define TYPEOF(ob) (((pword) (ob))->tag)



#define PWADDR(w) ((pword) (w))

/* 
 * Object Accessing Macros:
 *
 * With the possible exception of the byte (word?) code interpreter,
 * the following macros should be used for creating and accessing all
 * data types.
 * 
 * The MK_ macros are generally used to make or build an object, while
 * the object_ macros are used to get information on an object.  It should
 * not be assumed that the object_ macros will work for building an object,
 * even though the macro definition looks like it will work.
 */


/*
 * TERMS:
 */

#define TERM_FUNCTOR(t) (PWADDR(t)->val)
#define TERM_ARGN(t,n)  ((PWADDR(t)+n)->val)
#define TERM_ARITY(t)   (FUNCTOR_ARITY(TERM_FUNCTOR(t)))
#define MK_TERM(a)   (mk_term((long)(a)))       /* a is the arity */


/*
 * FUNCTORS and CONSTANT SYMBOLS:
 *
 * Functors and constant symbols have the same representation even though
 * it is more efficient to special case the symbols.
 */

#define FUNCTOR_VAL(f) ((long) PWADDR(f)->val)
#define FUNCTOR_ARITY(f) ((long) (PWADDR(f)+1)->val)
#define FUNCTOR_TOKID(f) FUNCTOR_VAL(f)
#define FUNCTOR_NAME(f) (TOKNAME(FUNCTOR_TOKID(f)))
#define MK_FUNCTOR(t,a) (mk_functor((long)(t),(long)(a)))
                /* t is index into token table, a is arity */
#define MK_SYM(t) (MK_FUNCTOR(t,0))


/*
 * LISTS:
 */

#define LIST_CAR(d) (PWADDR(d)->val)
#define LIST_CDR(d) ((PWADDR(d)+1)->val)
#define MK_LIST(car,cdr) mk_list((car),(cdr))


/*
 * INTEGERS:
 */

#define INT_VAL(o) ((long) (PWADDR(o)->val))
#define MK_INT(i) mk_int((long)(i))



/*
 * NIL:         (internal nil, not [])
 */

extern pwrd nil_val;
#define NIL_VAL ((pword) &nil_val)

/*
 * Variable Offsets:  (Originally used in interpreter, now used in compiler)
 */

#define VO_VAL(v) ((long) (PWADDR(v)->val))
#define MK_VO(vo) (mk_vo((long)(vo)))

/*
 * CLAUSES
 */

#define RULE_NVARS(r) ((PWADDR(r)+1)->tag)
#define RULE_HEAD(r) ((PWADDR(r)+1)->val)
#define RULE_GOALN(r,n) ((PWADDR(r)+1+n)->val)
#define RULE_NGOALS(r) ((long) (PWADDR(r)->val))
#define MK_RULE(n) (mk_rule((long)(n)))
        /* n is the number of goals .. used for allocation of space */


/*
 * Doubles
 *
 * Doubles will have different representations depending upon the implementation
 * If DoubleType is defined (config.h), doubles will ocuupy two pwords.  The
 * value in the first word will contain the least significant part of the
 * double.  The value in the second will contain the most significant part.
 *
 * If DoubleType is not defined, then doubles are represented as terms,
 * $double(d1,d2,d3,d4) where d1-d4 are the parts of the double.
 *
 */

#define MK_DOUBLE(v) mk_double((double) (v))

#ifdef DoubleType
#define DOUBLE_VAL1(v) ((long) PWADDR(v)->val)
#define DOUBLE_VAL2(v) ((long) (PWADDR(v)+1)->val)
#define DOUBLE_VAL(v) double_val(v)
#endif /* DoubleType */



/*
 * UnInterned Atoms
 */


#define MK_UIA(s) mk_uia(s)
#define UIA_NAME(v) ((char *) ((pword) (v)+1))

extern	void	prs_area_init	( unsigned long );
extern	void	alc_rst		( void );
extern	int	is_double	( double *, pword );
extern	pword	mk_term		( long );
extern	pword	mk_functor	( long, long );
extern	pword	mk_list		( pword, pword );
extern	pword	mk_int		( long );
extern	pword	mk_vo		( long );
extern	pword	mk_rule		( long );
extern	pword	mk_double	( double );
extern	double	double_val	( pword );
extern	long	functor_id_of_term ( pword );
extern	long	arity_of_term	( pword );
extern	pword	mk_uia		( char * );

#endif /* ALLOC_I */
