/*
 * index.c              -- indexing functions for full first argument indexing
 *      Copyright (c) 1986 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation Date: 2/11/86
 * Revision History:
 *      11/88   kmh     Documented the thing. Modified for 386.
 *
 */

#include <stdio.h>

#include "defs.h"

#ifdef Indexing

#include "coerce.h"
#include "tokens.h"
#include "main.h"
#include "wintcode.h"

/*
 * defines for indexing structure
 */

#define TOP 0
#define BOT 1
#define VAR 2
#define TVAL 3
#define TPTR 4

/*
 * Types that the indexer understands. Derived from machine types.
 */

#define ITP_VAR 0
#define ITP_CONST 1
#define ITP_STRUCT 2
#define ITP_LIST 3

/*
 * Sizes of some things
 */

#define TBENTSIZE 2		/* size of a table entry in long words     */
#undef RETRYSIZE
#define RETRYSIZE 3		/* size of a retry or trust instruction in
				 * long words
				 */
#define TBOVERHEAD 3		/* overhead required for table entry in long
				 * words
				 */

/*
 * Structure definitions
 */

typedef struct index_str {
    byte  tag;			/* Type of index node */
    /* Either machine type this node represents or number of entries if tag
     * is TOP
     */
    long  key;
    struct index_str *llink;
    struct index_str *rlink;
    union {
	CodePtr val;
	struct index_str *ptr;
    } addr;
} index_node;

/* If the size of this changes, the wm_sw_const instruction must change */

typedef struct {
    long  key;
    CodePtr addr;
} switchTableEntry;

#define NULLNODE ((index_node *) 0)

typedef struct {
    index_node *ip;		/* pointer to indexing structure  */
    long  size;			/* total size (in words) of block */
    long  topnodes;		/* number of top level nodes    */
    long  lowergroups;		/* number of lower groups       */
    long  lowernodes;		/* number of lower nodes        */
    CodePtr startaddr;		/* starting address             */
} index_header;

#define INIT_IH(ih) (ih).ip = NULLNODE; \
                    (ih).size = 0; \
		    (ih).lowergroups = 0; \
		    (ih).lowernodes = 0; \
		    (ih).startaddr = (CodePtr) 0


long *buf_ptr;

/*
 * The following are not really functions, but convenient ways to get
 * at some addresses of things which we'd like to have addresses for.
 */

#define TRY      ((CodePtr) abinst(W_TRY))
#define RETRY    ((CodePtr) abinst(W_RETRY))
#define TRUST    ((CodePtr) abinst(W_TRUST))

/*
 * The following are real.  (See above comment).
 */

CodePtr icIndexPatch();

static index_node *free_ptr;
static long nodes_left;

static
initialize_nodeallocator()
{
    free_ptr = (index_node *) prs_area;
    nodes_left = PARSER_AREASIZ * sizeof (pwrd) / sizeof (index_node);
}

static index_node *
alloc_node()
{
    if (nodes_left <= 0) {
	fprintf(stderr, "Out of indexing nodes.\n Bye\n");
	als_exit(1);
    }

    nodes_left--;

    return free_ptr++;
}


ntbl_entry *w_nameprobe();
long *get_clause_information();
long *w_alloccode();

indexproc(m, p, a)
    PWord m, p;
    int   a;

{
    ntbl_entry *ent;

    if (ent = w_nameprobe(m, p, a))
	do_indexing(ent);
}

#define SYSDEB(a)		/* nothing */

do_indexing(ent)
    ntbl_entry *ent;

{
    long *curaddr;
    int   tp;
    long  key;
    CodePtr clausestart;
    index_header const_ih, struct_ih, list_ih;
    int   nclauses = 0;
    int   totalsize;

    /* No sense indexing if no multiple clauses or already indexed. */
    if ((ent->flags & NMSK_USAGE) != NFLG_MULTIPLE)
	return;

    /* Get the address of the clause block for the first clause */
    curaddr = ent->first_clause;

    SYSDEB(printf("In do_indexing: first clause addr=%x\n", curaddr));

    initialize_nodeallocator();

    INIT_IH(const_ih);
    INIT_IH(struct_ih);
    INIT_IH(list_ih);

    while (curaddr != 0) {
	curaddr = get_clause_information(curaddr,
					 &tp, &key, &clausestart);
	nclauses++;

	SYSDEB(printf("Clause %d, tp=%d, key=%x, clausestart=%lx\n",
		      nclauses, tp, key, clausestart));

	switch ((int) tp) {
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

    compute_size(&const_ih, nclauses);
    compute_size(&list_ih, nclauses);
    compute_size(&struct_ih, nclauses);
    totalsize = const_ih.size + list_ih.size + struct_ih.size;

    SYSDEB(printf("totalsize=%d words\n", totalsize));

    buf_ptr = (long *) 0;
    if (totalsize != 0) {
	if ((buf_ptr = w_alloccode(totalsize)) == 0) {
	    fprintf(stderr, "No code space for indexing block.\n");
	    als_exit(1);
	}

	/* Make procedure block point to the indexing block of code */
	ent->index_block = buf_ptr;

	/* skip over memory management stuff */
	buf_ptr += WCI_CLAUSECODE;

	/*
	 * The order of the following procedure calls is somewhat
	 * important and will probably vary between wam implementations
	 */

	addtobuf(ITP_LIST, &list_ih);
	addtobuf(ITP_STRUCT, &struct_ih);
	addtobuf(ITP_CONST, &const_ih);
    }

    /* Only output switch_on_term if at least one type needs an index block. */
    if (struct_ih.startaddr != (CodePtr) (-1) ||
	list_ih.startaddr != (CodePtr) (-1) ||
	const_ih.startaddr != (CodePtr) (-1)) {

	/* Install the switch */
	ic_install_switch_on_term(ent,
				  struct_ih.startaddr,
				  list_ih.startaddr,
				  const_ih.startaddr);

	/* Mark the procedure as having an index block */
	ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SWITCH;

/*
 * printf(":- index_proc(user,%s,%d).\n",
 * TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
 * MFUNCTOR_ARITY(ent->tokid_arity));
 */
    }
    else {			/* Must not need the indexing block. Let it
				 * go.
				 */
	if (totalsize != 0) {
	    w_freecode(ent->index_block);
	    ent->index_block = 0;
	}
    }
    SYSDEB(printf("do_indexing() done.\n"));
}


insert_node(ihp, key, tag, val)
    index_header *ihp;
    long  key;
    int   tag;
    CodePtr val;

{
    index_node *p, *q;
    long *sizeptr;

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
	    else if (((unsigned long) key) < ((unsigned long) p->key)) {
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


/*
 * Figure out how much room this index group will take.
 *
 * Also fill in things for size 0.
 */

compute_size(ihp, nclauses)
    index_header *ihp;
    int   nclauses;

{
    int   toplevelcount;
    int   tablesize;
    index_node *s, *w;		/* w is the wanderer */

    toplevelcount = 0;
    tablesize = 0;
    s = w = ihp->ip;

    if (s == NULLNODE) {
	ihp->size = 0;

	/* set start addr to 0 when there are no clauses of the given type.
	 * The switch on term installer will convert this into a jump to fail
	 */
	ihp->startaddr = (CodePtr) 0;
    }
    else {
	/* Figure out the total number of words needed for all hash tables
	 * for this index structure. Also count the number of top level
	 * nodes.
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
	    ihp->startaddr =
		(CodePtr) (((CodePtr) ca) +
			   *(ca - WCI_CLAUSECODE + WCI_FSTART));
	}
	/* don't want this node counted for space allocation purposes */
	toplevelcount = 0;
    }
    else if (toplevelcount == nclauses) {
	/* no need to do indexing, current chain naive is already sufficient */
	ihp->startaddr = (CodePtr) (-1);
	toplevelcount = 0;
    }

    /* Calculate total size of indexing patches and struct/term tables */
    ihp->size = toplevelcount * RETRYSIZE
	+ tablesize
	+ ihp->lowernodes * RETRYSIZE;
}

computetablesize(nentries)	/* returns table size in words */
    long  nentries;

{
    return (nentries * TBENTSIZE + TBOVERHEAD);
}

switchTableEntry *walk_tree();

addtobuf(tp, ihp)
    int   tp;			/* type */
    index_header *ihp;

{
    /* If no code to emit, stop */
    if (ihp->size == 0)
	return;

    if (ihp->topnodes == 1) {	/* then we got a tree as the only node */
	switchTableEntry *tree_ptr;

	ihp->startaddr = (CodePtr) buf_ptr;
	tree_ptr = (switchTableEntry *) buf_ptr;
	buf_ptr += computetablesize(ihp->ip->key);
	installtreeoverhead(tp, ihp->ip->key, (CodePtr *) (&tree_ptr));
	tree_ptr = walk_tree(ihp->ip->addr.ptr, tree_ptr);
    }
    else {
	long  i, n;
	index_node *w;
	CodePtr toplev_ptr;
	switchTableEntry *tree_ptr;

	w = ihp->ip;
	n = ihp->topnodes;

	/* Tell index node where this index patch will be located */
	ihp->startaddr = (CodePtr) buf_ptr;

	/* We need a pointer to where the next bit of code in the current
	 * index patch will go
	 */
	toplev_ptr = (CodePtr) buf_ptr;

	/* Move to where next index patch will go */
	buf_ptr += n * RETRYSIZE;

	/* Go down chain, putting code into the indexing patch. */
	for (i = 1; i <= n; i++, w = w->rlink) {
	    CodePtr where, choiceInstr;

	    /* Decide kind of choice point instruction needed */
	    if (i == 1)
		choiceInstr = TRY;
	    else if (i == n)
		choiceInstr = TRUST;
	    else
		choiceInstr = RETRY;

	    if (w->tag == TOP) {
		tree_ptr = (switchTableEntry *) buf_ptr;

		/* Next patch goes at buf_ptr */
		where = (CodePtr) buf_ptr;

		/* Save room for next patch */
		buf_ptr += computetablesize(w->key);

		/* Put in code */
		installtreeoverhead(tp, w->key,
				    (CodePtr *) (&tree_ptr));
		tree_ptr = walk_tree(w->addr.ptr, tree_ptr);
	    }
	    else {
		where = w->addr.val;

		/* If last one, need determinate entry */
		if (i == n)
		    where += *((long *) where - WCI_CLAUSECODE
			       + WCI_DSTART);
	    }

	    /* Put in instruction into current index patch */
	    toplev_ptr = icIndexPatch(toplev_ptr,
				      choiceInstr, where);
	}
    }
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
 * a(a).
 * a(foo).
 * a(b).
 *
 * then the table entry for the code address for each atom (a, foo, b) will point
 * at the clause. If the item has multiple occurences in the table, the code
 * address points at a try/retry chain for getting each occurence, e.g.
 *
 * a(a).
 * a(foo).
 * a(b).
 * a(foo).
 *
 * foo's table entry points at a try/retry chain.
 */

switchTableEntry *
walk_tree(ip, tp)
    index_node *ip;
    switchTableEntry *tp;	/* address of roving tree pointer */

{
    if (ip != NULLNODE) {

	/*
	 * Do an inorder traversal of the tree
	 */

	/* Do left side */
	tp = walk_tree(ip->llink, tp);

	/* Handle this node. Output key/address pair. */
	tp->key = ip->key;

	if (ip->tag == TVAL)
	    /* Only one entry. Use clause address. */
	    tp->addr = ip->addr.val;
	else {
	    /* More than one. Output try/retry chain. */

	    /* Code entry points where try/retry chain is going */
	    tp->addr = (CodePtr) buf_ptr;
	    walk_bottom(ip->addr.ptr);
	}

	/* Move to next table location */
	tp++;

	return walk_tree(ip->rlink, tp);
    }
    else
	return tp;
}

/*
 * Walk along the bottom of the tree, putting down the final index patch for
 * a given constant or functor. We have the situation of having found the
 * match for a constant, say foo, but there are multiple instances of foo
 * in the first argument of the procedure, so we need a try/retry chain for
 * the constant foo.
 *
 * a(a).
 * a(foo).
 * a(b).
 * a(foo).
 */

walk_bottom(ip)
    index_node *ip;

{
    index_node *w;
    CodePtr choiceInstr;

    w = ip;

    do {
	if (w == ip)
	    choiceInstr = TRY;
	else if (w->rlink == ip)
	    choiceInstr = TRUST;
	else
	    choiceInstr = RETRY;

	buf_ptr = (long *) icIndexPatch((CodePtr) buf_ptr,
					choiceInstr,
					w->addr.val);

	w = w->rlink;
    } while (w != ip);
}

/*
 * installtreeoverhead installs the following code for switch_on_const
 *      and switch_on_struct:
 *
 *      Size            Instr
 *      ----            ---------
 *      5               mov     EAX,N
 *      6               call    [swConstPtr]    (or swStructPtr)
 *      1               nop     ; Long word align
 *
 *      Total Size: 12 bytes = 3 long words
 *
 */

installtreeoverhead(tp, nentries, lptr)
    int   tp;			/* type */
    long  nentries;
    CodePtr *lptr;		/* address of pointer to place tree overhead */

{
    CodePtr p;
    long *longptr;

    p = *lptr;

    switch ((int) tp) {
	case ITP_CONST:
	    *p++ = abinst(W_SW_CONST);
	    break;

	case ITP_STRUCT:
	    *p++ = abinst(W_SW_STRUCT);
	    break;

	default:
	    fprintf(stderr, "installtreeoverhead: Bad type.\n");
	    als_exit(1);
	    break;
    }

    p += (sizeof (long) - sizeof (Code)) / sizeof (Code);
    longptr = (long *) p;
    *longptr++ = nentries;
    p = (CodePtr) longptr;

    *lptr = p;
}

long *
get_clause_information(addr, tp, key, clausestart)
    long *addr;			/* Pointer to clause entry for the code */
    int  *tp;			/* Index type of the first argument */
    long *key;			/* Actual Prolog type of 1st argument */
    CodePtr *clausestart;	/* Where the clause code itself starts */

{
    long  k;

    /* Get the machine type for the 1st argument */
    *key = k = *(addr + WCI_FIRSTARGKEY);

    /* And figure out what the indexing type is */
    switch ((int) (k & MTP_TAGMASK)) {
	case MTP_UNBOUND:
	    *tp = ITP_VAR;
	    break;
	case MTP_LIST:
	    *tp = ITP_LIST;
	    break;
	case MTP_CONST:
	    if ((k & MTP_CONSTMASK) == MTP_SYM &&
		MFUNCTOR_ARITY(k) != 0)
		*tp = ITP_STRUCT;
	    else
		*tp = ITP_CONST;
	    break;
	default:
	    *tp = ITP_VAR;
	    break;
    }

    /* Where the actual clause code starts */
    *clausestart = (CodePtr) (addr + WCI_CLAUSECODE);

    /* And return a pointer to the next clause entry in the procedure */
    return (long *) *(addr + WCI_NEXTCLAUSEADDR);
}

#endif
