/*===============================================================*
 |			index.c              
 |      Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |			-- indexing functions for full first argument indexing
 |
 | Author: Kevin A. Buettner
 | Creation Date: 2/11/86
 | Revision History:
 | 07/31/90 - kev -- Made generic
 | 10/26/94 - C. Houpt -- Fixed l-value cast problem in do_indexing().
 *===============================================================*/
#include "defs.h"

#ifdef Indexing

#include <setjmp.h>
#include "wintcode.h"
#include "icodegen.h"

/*
 * defines for indexing structure
 */

#define TOP 0
#define BOT 1
#define VAR 2
#define TVAL 3
#define TPTR 4

/*
 * Types that we are indexing over
 */

#define ITP_VAR 0
#define ITP_CONST 1
#define ITP_STRUCT 2
#define ITP_LIST 3

/*
 * TBENTSIZE, RETRYSIZE, TBOVERHEAD, and TRYOVERHEAD are in chpt.h.
 */


/*
 * Structure definitions
 */

typedef struct index_str {
    tag_byte  tag;
    long  key;			/* machine type this node represents or */
    					/* if tag is TOP then number of entries */
    struct index_str *llink;
    struct index_str *rlink;
    union {
	Code * val;
	struct index_str *ptr;
    } addr;
} index_node;

/* If the size of this changes, the wm_sw_const instruction must change */

typedef struct {
    long  key;
    Code *addr;
} switchTableEntry;

#define NULLNODE ((index_node *) 0)

typedef struct {
    index_node *ip;		/* pointer to indexing structure  */
    int   size;			/* total size (in words) of block */
    int   topnodes;		/* number of top level nodes    */
    int   lowergroups;		/* number of lower groups       */
    int   lowernodes;		/* number of lower nodes        */
    Code *startaddr;		/* starting address             */
} index_header;

#define INIT_IH(ih) (ih).ip = NULLNODE; \
                    (ih).size = 0; \
		    (ih).lowergroups = 0; \
		    (ih).lowernodes = 0; \
		    (ih).startaddr = (Code *) 0


static long *buf_ptr;

static long nargs;
static long emaskagg;		/* the emask aggregate */

static index_node *free_ptr;
static int nodes_left;
static jmp_buf alloc_overflow;

static	void	initialize_nodeallocator ( void );
static	index_node * alloc_node	( void );
static	void	insert_node	( index_header *, long, int, Code * );
static	void	compute_size	( index_header *, int, Code * );
static	int	computetablesize ( int );
static	void	addtobuf	( int, index_header * );
static	switchTableEntry * walk_tree ( index_node *, switchTableEntry *,
					      int );
static	void	installtreeoverhead ( int, int, switchTableEntry ** );
static	void	walk_bottom	( index_node *, int );
static	long *	get_clause_information
				( long *, int *, long *, Code ** );


static void
initialize_nodeallocator()
{
    free_ptr = (index_node *) prs_area;
    nodes_left = PARSER_AREASIZ * sizeof (pwrd) / sizeof (index_node);
}

static index_node *
alloc_node()
{
    if (nodes_left <= 0) {

	/*
	 * We are out of indexing nodes.  We may possibly wish to print
	 * a message at this point.  For the time being we will silently
	 * jump back and return without generating any indexing.
	 */

	longjmp(alloc_overflow, 1);
    }

    nodes_left--;
    return free_ptr++;
}


void
indexproc(m, p, a)
    PWord m, p;
    int   a;
{
    ntbl_entry *ent;

    if ( (ent = w_nameprobe(m, p, a)) )
	do_indexing(ent);
}

#define SYSDEB(a)		 /* nothing */

void
do_indexing(ent)
    ntbl_entry *ent;
{
    long *curaddr, *firstaddr;
    int   tp;
    long  key; 
    Code *clausestart;
    index_header const_ih, struct_ih, list_ih;
    int   nclauses;
    int   totalsize;

    if ((ent->flags & NMSK_USAGE) != NFLG_MULTIPLE)
	return;

    if (setjmp(alloc_overflow))
	return;			/* catch allocator overflow */

    initialize_nodeallocator();
    nclauses = 0;
    nargs = ent->nargs;

    firstaddr = curaddr = ent->first_clause;

    INIT_IH(const_ih);
    INIT_IH(struct_ih);
    INIT_IH(list_ih);

    while (curaddr != 0) {
	curaddr = get_clause_information(curaddr, &tp, &key, &clausestart);
	nclauses++;

	SYSDEB(printf("Clause %d, tp=%d, key=%x, clausestart=%x\n",
		      nclauses, tp, key, clausestart));

	switch (tp) {
	    case ITP_CONST:
		insert_node(&const_ih, key, BOT, clausestart);
		break;
	    case ITP_LIST:
		insert_node(&list_ih, key, VAR, clausestart);
		break;
	    case ITP_STRUCT:
		insert_node(&struct_ih, key, BOT, clausestart);
		break;
	    default:
		insert_node(&const_ih, key, VAR, clausestart);
		insert_node(&list_ih, key, VAR, clausestart);
		insert_node(&struct_ih, key, VAR, clausestart);
		break;
	}
    }

    emaskagg = 0;

    compute_size(&const_ih, nclauses, choiceEntry(firstaddr));
    compute_size(&list_ih, nclauses, choiceEntry(firstaddr));
    compute_size(&struct_ih, nclauses, choiceEntry(firstaddr));
    totalsize = const_ih.size + list_ih.size + struct_ih.size;

    SYSDEB(printf("totalsize=%d words\n", totalsize));

    if (totalsize > MAX_INDEX_BLOCK_SIZE)
	longjmp(alloc_overflow, 1);

    buf_ptr = (long *) 0;

    if (totalsize != 0) {

	buf_ptr = (long *) w_alloccode(totalsize);

	/*
	 * Note: w_alloccode will not return if space is not available.
	 * Previous versions of this code had us checking the return value
	 * for zero and exitting.
	 */

	ent->index_block = buf_ptr;
	buf_ptr += WCI_CLAUSECODE;	/* skip over memory management stuff */

	/*
	 * The order of the following procedure calls is somewhat important
	 * and will probably vary between wam implementations
	 */

	addtobuf(ITP_LIST, &list_ih);
	addtobuf(ITP_STRUCT, &struct_ih);
	addtobuf(ITP_CONST, &const_ih);

    }

    /* Output switch_on_term if at least one type can gain from indexing. */

    if (struct_ih.startaddr != choiceEntry(firstaddr) ||
	list_ih.startaddr != choiceEntry(firstaddr) ||
	const_ih.startaddr != choiceEntry(firstaddr)) {

	/* install try_me at varaddr prior to installing
	 * sw_on_term (because on RISCs we might try to suck
	 * some instructions from here to fill delay slots)
	 */
	ic_install_try_me(choiceCode(firstaddr),
			  (long)choiceEntry(nextClauseAddr(ent->first_clause)),
			  nargs);

	/* Install the switch */

	ic_install_switch_on_term(ent,
				  choiceEntry(firstaddr),
				  struct_ih.startaddr,
				  list_ih.startaddr,
				  const_ih.startaddr,
				  emaskagg);
	/* Install emaskagg if possible */
	if (ent->index_block)
			/* Was:
	    		emaskCode(ent->index_block) = emaskagg;
			 * But thats not ANSI
			 */
	{
		register int *p = emaskCodeAddr(ent->index_block);
		*p = emaskagg;
	}

	/* Mark the procedure as having an index block */
	ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SWITCH;
    }
}

static void
insert_node(ihp, key, tag, val)
    index_header *ihp;
    long  key;
    int   tag;
    Code *val;
{
    index_node *p, *q;
    long *sizeptr = NULL;	/* Initialized to stifle -Wall complaint */

    if (ihp->ip == NULLNODE) {
	p = alloc_node();
	p->tag = tag;
	p->key = key;
	p->addr.val = val;
	p->llink = p;
	p->rlink = p;
	ihp->ip = p;
    }
    else {
	p = ihp->ip->llink;	/* set p to last node in list */
	for (;;) {
	    if (tag == VAR || p->tag == VAR) {
		q = alloc_node();
		q->tag = tag;
		q->key = key;
		q->addr.val = val;
		q->llink = p;
		q->rlink = p->rlink;
		p->rlink->llink = q;
		p->rlink = q;
		break;
	    }
	    else if (p->tag == BOT) {
		q = alloc_node();
		q->tag = TVAL;
		q->key = p->key;
		q->addr.val = p->addr.val;
		p->addr.ptr = q;
		p->tag = TOP;
		p->key = 1;	/* one tree node */
		q->llink = NULLNODE;
		q->rlink = NULLNODE;
		sizeptr = &(p->key);
		p = q;
	    }
	    else if (p->tag == TOP) {
		sizeptr = &(p->key);
		p = p->addr.ptr;
	    }
	    else if (p->key == key) {
		if (p->tag == TVAL) {
		    q = alloc_node();
		    q->llink = q;
		    q->rlink = q;
		    q->tag = BOT;
		    q->key = key;
		    q->addr.val = p->addr.val;
		    p->tag = TPTR;
		    p->addr.ptr = q;
		    (ihp->lowergroups)++;
		    (ihp->lowernodes)++;
		}
		p = (p->addr.ptr)->llink;
		q = alloc_node();
		q->llink = p;
		q->rlink = p->rlink;
		p->rlink->llink = q;
		p->rlink = q;
		q->tag = BOT;
		q->key = key;
		q->addr.val = val;
		(ihp->lowernodes)++;
		break;
	    }
	    else if (((unsigned) key) < ((unsigned) p->key)) {
		if (p->llink == NULLNODE) {
		    q = alloc_node();
		    q->llink = NULLNODE;
		    q->rlink = NULLNODE;
		    q->tag = TVAL;
		    q->key = key;
		    q->addr.val = val;
		    p->llink = q;
		    (*sizeptr)++;
		    break;
		}
		else
		    p = p->llink;
	    }
	    else {		/* (key > p->key) */
		if (p->rlink == NULLNODE) {
		    q = alloc_node();
		    q->llink = NULLNODE;
		    q->rlink = NULLNODE;
		    q->tag = TVAL;
		    q->key = key;
		    q->addr.val = val;
		    p->rlink = q;
		    (*sizeptr)++;
		    break;
		}
		else
		    p = p->rlink;
	    }
	}			/* for */
    }				/* else */
}				/* insert_node */


static void
compute_size(ihp, nclauses, startaddr)	/* also fill in things for size 0 */
    index_header *ihp;
    int   nclauses;
    Code *startaddr;
{
    int   toplevelcount;
    int   tablesize;
    index_node *s, *w;		/* w is the wanderer */

    toplevelcount = 0;
    tablesize = 0;
    s = w = ihp->ip;

    if (s == NULLNODE) {
	ihp->size = 0;
	ihp->startaddr = (Code *) 0;
	/* set start addr to 0 when there are no
	 * clauses of the given type.  The switch
	 * on term installer will  convert this into
	 * a jump to fail
	 */
    }
    else {
	/* Figure out the total number of words needed for all
	 * hash tables for this index structure. Also count the
	 *  number of top level nodes.
	 */

	do {
	    if (w->tag == TOP)
		tablesize += computetablesize(w->key);
	    toplevelcount++;
	    w = w->rlink;
	} while (w != s);
    }

    ihp->topnodes = toplevelcount;

    if (toplevelcount == 1) {
	/* If only one clause, jump to determinate start of clause */
	if (tablesize == 0) {
	    long *ca;

	    ca = (long *) ihp->ip->addr.val;
	    ihp->startaddr = clauseCode(ca) + (fstartCode(ca) & 0xffff);
	    emaskagg |= *(ca + WCI_EMASK);
	}
	toplevelcount = 0;	/* don't want this node counting for space
				 * allocation purposes
				 */
    }
    else if (toplevelcount == nclauses) {
	ihp->startaddr = startaddr;	/* no need to do indexing, current */
	toplevelcount = 0;	/* chain is already sufficient     */
    }

    ihp->size = (toplevelcount ? TRYOVERHEAD : 0)
	+ toplevelcount * RETRYSIZE
	+ tablesize
	+ ihp->lowergroups * TRYOVERHEAD
	+ ihp->lowernodes * RETRYSIZE;
}


static int
computetablesize(nentries)	/* returns table size in words */
    int   nentries;
{
    return (nentries * TBENTSIZE + TBOVERHEAD);
}


#define TREEALONE 2

static void
addtobuf(tp, ihp)
    int   tp;			/* type */
    index_header *ihp;
{
    switchTableEntry *tree_ptr;

    if (ihp->size == 0)
	return;

    if (ihp->topnodes == 1) {	/* then we got a tree as the only node */
	ihp->startaddr = (Code *) buf_ptr;
	tree_ptr = (switchTableEntry *) buf_ptr;
	buf_ptr += computetablesize(ihp->ip->key);
	installtreeoverhead(tp, ihp->ip->key, &tree_ptr);
	tree_ptr = walk_tree(ihp->ip->addr.ptr, tree_ptr, TREEALONE);
    }
    else {
	int   i, n;
	index_node *w;
	long *toplev_ptr;
	Code *addr;
	int   emask;

	w = ihp->ip;
	n = ihp->topnodes;
	addr = (Code *) 0;

	/* Tell index node where this index patch will be located */
	ihp->startaddr = (Code *) buf_ptr;

	/* We need a pointer to where the next bit of code in the
	 * current index patch will go
	 */
	toplev_ptr = buf_ptr;

	/* Move to where next index patch will go */
	buf_ptr += (n * RETRYSIZE + TRYOVERHEAD);

	/* Go down chain, putting code into the indexing patch. */
	for (i = 1; i <= n; i++, w = w->rlink) {

	    if (w->tag == TOP) {
		tree_ptr = (switchTableEntry *) buf_ptr;

		/* Next patch goes at buf_ptr */
		addr = (Code *) tree_ptr;

		/* Save room for next patch */
		buf_ptr += computetablesize(w->key);

		/* Put in code */
		installtreeoverhead(tp, w->key, &tree_ptr);

		tree_ptr = walk_tree(w->addr.ptr, tree_ptr, i == n);
		emask = 0;
	    }
	    else {
		emask = emaskCode(w->addr.val);

		/* If last one, need determinate entry */
		if (i == n)
		    addr = clauseCode(w->addr.val) + dstartCode(w->addr.val);
		else
		    addr = clauseCode(w->addr.val);
	    }

	    if (i == 1) {
		toplev_ptr = ic_install_try(toplev_ptr, addr, nargs);
	    }
	    else if (i == n) {
		toplev_ptr = ic_install_trust(toplev_ptr, addr, nargs, emask);
	    }
	    else {
		toplev_ptr = ic_install_retry(toplev_ptr, addr, nargs, emask);
	    }
	}			/* for */
    }				/* else */
}


/*
 * We are now going to walk the tree and output the table to be searched by the
 * swith_on_const or switch_on_term instructions. The table is currently
 * searched by a binary search, so we do an inorder traversal of the tree
 * to get the keys out in ascending search order.
 *
 * If there is only one clause which has a given constant, then the address to
 * the deterministic entry of that clause is used. For example, if the procedure
 * is
 *
 *      a(a).
 *      a(foo).
 *      a(b).
 *
 * then the table entry for the code address for each atom (a, foo, b) will point
 * at the clause. If the item has multiple occurences in the table, the code
 * address points at a try/retry chain for getting each occurence, e.g.
 *
 *      a(a).
 *      a(foo).
 *      a(b).
 *      a(foo).
 *
 * foo's table entry points at a try/retry chain.
 */

switchTableEntry *
walk_tree(ip, tp, isdet)
    index_node *ip;
    switchTableEntry *tp;	/* address of roving tree pointer */
    int   isdet;
{
    if (ip != NULLNODE) {

	/*
	 * Do an inorder traversal of the tree
	 */

	/* Do left side */
	tp = walk_tree(ip->llink, tp, isdet);

	/* Handle this node. Output key/address pair. */
	tp->key = ip->key;

	if (ip->tag == TVAL) {
	    /* Only one entry. Use clause address. */
	    tp->addr = clauseCode(ip->addr.val) +
		(isdet ? dstartCode(ip->addr.val) : 0);
	    if (isdet == TREEALONE)
		emaskagg |= emaskCode(ip->addr.val);
	}
	else {
	    /* More than one. Output try/retry chain. */

	    /* Code entry points where try/retry chain is going */
	    tp->addr = (Code *) buf_ptr;
	    walk_bottom(ip->addr.ptr, isdet);
	}

	/* Move to next table location */
	tp++;

	return walk_tree(ip->rlink, tp, isdet);
    }
    else
	return tp;
}


#ifdef Portable
#define wm_sw_const    W_SW_CONST
#define wm_sw_struct   W_SW_STRUCT
#else
extern	void	wm_sw_const	( void );
extern	void	wm_sw_struct	( void );
#endif

/*
 * installtreeoverhead installs code for the switch_on_constant and
 * switch_on_structure sequences.
 */

static void
installtreeoverhead(tp, nentries, lptr)
    int   tp;			/* type of the entry */
    int   nentries;		/* number of entries */
    switchTableEntry **lptr;	/* address of pointer to where the tree
				 * overhead will go
				 */
{
    long *swaddr = NULL;	/* Initialized to stifle -Wall */

    switch (tp) {
	case ITP_CONST:
	    swaddr = (long *) wm_sw_const;
	    break;
	case ITP_STRUCT:
	    swaddr = (long *) wm_sw_struct;
	    break;
	default:
	    fatal_error(FE_IN_INDEX1, 0);
	    break;
    }

    *lptr = (switchTableEntry *) 
		ic_install_tree_overhead(swaddr, nentries, (Code *) *lptr);
}


/*
 * Walk along the bottom of the tree, putting down the final index patch for
 * a given constant or functor. We have the situation of having found the
 * match for a constant, say foo, but there are multiple instances of foo
 * in the first argument of the procedure, so we need a try/retry chain for
 * the constant foo.
 *
 *         a(a).
 *         a(foo).
 *         a(b).
 *         a(foo).
 */

static void
walk_bottom(ip, isdet)
    index_node *ip;
    int   isdet;		/* The "is determinate" flag */
{
    index_node *w;

    w = ip;

    do {
	if (w == ip) {
	    buf_ptr = ic_install_try(buf_ptr, clauseCode(w->addr.val), nargs);
	}
	else if (w->rlink == ip) {
	    buf_ptr = ic_install_trust(
					  buf_ptr,
					  clauseCode(w->addr.val) +
				      (isdet ? dstartCode(w->addr.val) : 0),
					  nargs,
					  emaskCode(w->addr.val));
	}
	else {
	    buf_ptr = ic_install_retry(buf_ptr,
				       clauseCode(w->addr.val),
				       nargs,
				       emaskCode(w->addr.val));
	}


	w = w->rlink;
    } while (w != ip);
}


long *
get_clause_information(addr, tp, key, clausestart)
    long *addr;			/* Pointer to clause entry for the code */
    int  *tp;			/* Index type of the first argument */
    long *key;			/* Actual Prolog type of 1st argument */
    Code **clausestart;		/* Where the clause code itself starts */
{
    long  k;

    /* Get the machine type for the 1st argument */
    *key = k = *(addr + WCI_FIRSTARGKEY);

    /* And figure out what the indexing type is */
    switch (k & MTP_TAGMASK) {
	case MTP_UNBOUND:
	    *tp = ITP_VAR;
	    break;
	case MTP_LIST:
	    *tp = ITP_LIST;
	    break;
#ifdef MTP_CONST
	case MTP_CONST:
	    if ((k & MTP_CONSTMASK) == MTP_SYM && MFUNCTOR_ARITY(k) != 0)
		*tp = ITP_STRUCT;
	    else
		*tp = ITP_CONST;
	    break;
#else
	case MTP_SYM:
	    if (MFUNCTOR_ARITY(k) != 0)
		*tp = ITP_STRUCT;
	    else
		*tp = ITP_CONST;
	    break;
	case MTP_INT:
	    *tp = ITP_CONST;
	    break;
#endif
	default:
	    *tp = ITP_VAR;
	    break;
    }

    /* Where the actual clause code starts */
    *clausestart = (Code *) addr;

    /* And return a pointer to the next clause entry in the procedure */
    return (long *) *(addr + WCI_NEXTCLAUSEADDR);
}

#endif /* Indexing */
