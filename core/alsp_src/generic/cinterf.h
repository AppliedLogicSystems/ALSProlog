/*
 * cinterf.h
 */

#ifndef CINTERF_INCLUDED
#define CINTERF_INCLUDED  1


/*
 * Information on the fields of a C-structure/C-union is kept in
 * an array (one per structure/union) that has the following layout.
 * (The array is initialized with the structure information by the
 * interface generator ).
 */

typedef struct {
  char *fname;
  unsigned int foffset;
  short  ftype;    /* integer identifying C type; 0 for struct/union */
  char *typename;  /* when ftype is 0, typename is set to struct name */
  int  arraysz;    /* N=0 => not an array, N>0 => array of size N */
} FieldEntry;      /* For array, currently, this is set to 1000   */
                   /* Later, when ctrans is modified to recognize */
                   /* array size, this field with be set correctly*/
                   /* What about multi-dimensional arrays ?       */

#define CI_BEGARRAY(name) static FieldEntry name[]= {

#define CI_FIELD(fieldstr,field,structname,typeid,typename) \
  { (fieldstr), ((unsigned int) &(((structname *)0)->field)), \
	  (typeid), (typename), 0 }

#define CI_ARRAYFIELD(fieldstr,field,structname,typeid,typename,size) \
  { (fieldstr), ((unsigned int) &(((structname *)0)->field[0])), \
	  (typeid), (typename), (size) }

#define CI_ENDARRAY  };


/*
 * The C interface maintains its own symbol table for
 * storing C-constants, C-type and C-structure information.
 * The following tags are used to identify the nature of
 * entries in the symbol table
 */

#define CI_LONGTYPE		0
#define CI_PTRTYPE		1
#define CI_INTTYPE		2
#define CI_STRINGTYPE	3
#define CI_SHORTTYPE	4
#define CI_CHARTYPE		5
#define CI_FLOATTYPE	6
#define CI_DOUBLETYPE	7
#define CI_RCONSTTYPE	8
#define CI_STRUCTTYPE	9
#define CI_CTYPE		10


/* 
 * The following macros are used to load up the symbol table at
 * interface initialization time with info on C-constants,
 * C-types, and C-structures (the initialization routine as
 * well as each of the macro invocation code are output by
 * interface generator ).
 */

extern	int	sym_insert_2long PARAMS(( char *, int, long, long ));
extern	int	sym_insert_dbl	PARAMS(( char *, int, double ));
/* FIXME: Do these need to be extern?? */
extern	void	CI_makefar	PARAMS(( PWord *, int *, unsigned short * ));
extern	int	CI_get_integer	PARAMS(( unsigned long *, unsigned long ));
extern	int	CI_get_double	PARAMS(( double *, unsigned long, unsigned long ));

#define CI_INTCONST(name,val) 	\
	sym_insert_2long(name,CI_INTTYPE,(long)val,0);

#define CI_SHORTCONST(name,val) 	\
	sym_insert_2long(name,CI_SHORTTYPE,(long)val,0);

#define CI_CHARCONST(name,val) 	\
	sym_insert_2long(name,CI_CHARTYPE,(long)val,0);

#define CI_LONGCONST(name,val) 	\
	sym_insert_2long(name,CI_LONGTYPE,(long)val,0);

#define CI_PTRCONST(name,val) 	\
	sym_insert_2long(name,CI_PTRTYPE,(long)val,0);

#define CI_STRINGCONST(name,val) 	\
	sym_insert_2long(name,CI_STRINGTYPE,(long)val,0);

#define CI_FLOATCONST(name,val) 	\
	sym_insert_dbl(name,CI_FLOATTYPE,(double)val);

#define CI_DOUBLECONST(name,val) 	\
	sym_insert_dbl(name,CI_DOUBLETYPE,(double)val);

#define CI_STRUCT(name,cname,defnptr)	\
  sym_insert_2long(name,CI_STRUCTTYPE,sizeof(cname),(long)defnptr);
/*  sym_insert_2long(name,CI_STRUCTTYPE,sizeof(cname),defnptr); */
/*  sym_insert_2long(name,CI_STRUCTTYPE,sizeof(cname),(long)(defnptr)); */

#define CI_CTYPEDEF(name,ctype,typeid) \
  sym_insert_2long(name,CI_CTYPE,sizeof(ctype),typeid);

#define CI_RCONST(name,val)		\
	sym_insert_2long(name,CI_RCONSTTYPE,(long)val,0);


#endif  /* CINTERF_INCLUDED */

