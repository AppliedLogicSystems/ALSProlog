/*
 * index.c		-- indexing functions for full first argument indexing
 *	Copyright (c) 1986 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation Date: 2/11/86
 * Revision History:
 *	Revised: 7/31/90	kev	-- Made generic
 *	Revised: mm/dd/yy
 *
 */

#include <stdio.h>
#include <setjmp.h>

#include "config.h"
#include "mtypes.h"
#include "alloc.h"
#include "parser.h"
#include "tokens.h"
#include "types.h"
#include "wintcode.h"
#include "chpt.h"

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
	byte tag;
	int  key;
	struct index_str *llink;
	struct index_str *rlink;
	union {
		int val;
		struct index_str *ptr;
	} addr;
} index_node;

#define NULLNODE ((index_node *) (int *) 0)

typedef struct {
	index_node *ip;		/* pointer to indexing structure  */
	int  size;		/* total size (in words) of block */
	int  topnodes;		/* number of top level nodes 	*/
	int  lowergroups;	/* number of lower groups	*/
	int  lowernodes;	/* number of lower nodes	*/
	int  startaddr;		/* starting address		*/
} index_header;

#define INIT_IH(ih) (ih).ip = (index_node *) (int *) 0; \
                    (ih).size = 0; \
		    (ih).lowergroups = 0; \
		    (ih).lowernodes = 0; \
		    (ih).startaddr = (int) 0


static long *buf_ptr;

static long nargs;
static long emaskagg;		/* the emask aggragate */

static index_node *free_ptr;
static int nodes_left;
static jmp_buf alloc_overflow;

static initialize_nodeallocator()
{
    free_ptr = (index_node *) prs_area;
    nodes_left = PARSER_AREASIZ * sizeof(pwrd) / sizeof(index_node);
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
	
	longjmp(alloc_overflow);
    }

    nodes_left--;
    return free_ptr++;
}


ntbl_entry *w_nameprobe();
long *get_clause_information();
long *w_alloccode();

indexproc(m,p,a)
{
    ntbl_entry *ent;

    if (ent=w_nameprobe(m,p,a)) {
	do_indexing(ent);
    }
}

#define SYSDEB(a) /* nothing */

do_indexing(ent)
    ntbl_entry *ent;
{
    long *firstaddr, *curaddr;
    int tp, key, clausestart;
    index_header const_ih, struct_ih, list_ih;
    int nclauses = 0;
    int totalsize;

    if ((ent->flags & NMSK_USAGE) != NFLG_MULTIPLE) return;

    if (setjmp(alloc_overflow)) return;		/* catch allocator overflow */

    firstaddr = ent->first_clause;

    SYSDEB(
	printf("In do_indexing: firstaddr=%x\n",firstaddr));

    initialize_nodeallocator();
    nargs = ent->nargs;

    curaddr = firstaddr;

    INIT_IH(const_ih);
    INIT_IH(struct_ih);
    INIT_IH(list_ih);

    while (curaddr != 0) {
	curaddr = get_clause_information(curaddr, &tp, &key, &clausestart);
	nclauses++;

	SYSDEB(printf("Clause %d, tp=%d, key=%x, clausestart=%x\n",
					    nclauses,tp,key,clausestart));

	switch (tp) {
	    case ITP_CONST :
		insert_node(&const_ih, key, BOT, clausestart);
		break;
	    case ITP_LIST :
		insert_node(&list_ih, key, VAR, clausestart);
		break;
	    case ITP_STRUCT :
		insert_node(&struct_ih, key, BOT, clausestart);
		break;
	    default :
		insert_node(&const_ih, key, VAR, clausestart);
		insert_node(&list_ih, key, VAR, clausestart);
		insert_node(&struct_ih, key, VAR, clausestart);
		break;
	}
    }

    emaskagg = 0;

    compute_size(&const_ih,nclauses,firstaddr+WCI_CHOICEENTRY);
    compute_size(&list_ih,nclauses,firstaddr+WCI_CHOICEENTRY);
    compute_size(&struct_ih,nclauses,firstaddr+WCI_CHOICEENTRY);
    totalsize = const_ih.size + list_ih.size + struct_ih.size;

    SYSDEB(printf("totalsize=%d words\n",totalsize));

    buf_ptr = (long *) 0;
    if (totalsize != 0) {
	if ((buf_ptr = w_alloccode(totalsize)) == 0) {
	    fprintf(stderr,"Out of code space.\nBye.\n");
	    als_exit(1);
	}
	ent->index_block = buf_ptr;
	buf_ptr += WCI_CLAUSECODE;	/* skip over memory management stuff */

	/*
	 * The order of the following procedure calls is somewhat important
	 * and will probably vary between wam implementations
	 */

	addtobuf(ITP_LIST,&list_ih);
	addtobuf(ITP_STRUCT,&struct_ih);
	addtobuf(ITP_CONST,&const_ih);

    }

    if (!(firstaddr+WCI_CHOICEENTRY == (long *) struct_ih.startaddr &&
	 firstaddr+WCI_CHOICEENTRY == (long *) list_ih.startaddr &&
	 firstaddr+WCI_CHOICEENTRY == (long *) const_ih.startaddr)) {
	/* Install the switch */

	ic_install_switch_on_term(ent,
				firstaddr+WCI_CHOICEENTRY,
				struct_ih.startaddr,
				list_ih.startaddr,
				const_ih.startaddr,
				emaskagg);
	ic_install_try_me(firstaddr+WCI_CHOICECODE,
		((long *) *(firstaddr+WCI_NEXTCLAUSEADDR)+WCI_CHOICEENTRY),
		nargs);
	ent->flags = (ent->flags & ~NMSK_USAGE) | NFLG_SWITCH;
    }
}


insert_node(ihp, key, tag, val)
    index_header *ihp;
    int val, tag, key;
{
    index_node *p, *q;
    int *sizeptr;

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
	p = ihp->ip->llink;		/* set p to last node in list */
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
		p->key = 1;			/* one tree node */
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
		    q= alloc_node();
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
	    else {   /* (key > p->key) */
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
	} /* for */
    } /* else */
} /* insert_node */


compute_size(ihp,nclauses,startaddr) /* also fill in things for size 0 */
    index_header *ihp;
    int nclauses;
    int startaddr;
{
    int toplevelcount;
    int tablesize;
    index_node *s,*w;		/* w is the wanderer */

    toplevelcount = 0;
    tablesize = 0;
    s = w = ihp->ip;

    if (s == NULLNODE) {
        ihp->size = 0;
        ihp->startaddr = 0;	/* set start addr to 0 when there are no
				 * clauses of the given type.  The switch
				 * on term installer will  convert this into
				 * a jump to fail
				 */
    }
    else {
        do {
	    if (w->tag == TOP)
		tablesize += computetablesize(w->key);
	    toplevelcount++;
	    w = w->rlink;
        } while (w != s);
    }

    ihp->topnodes = toplevelcount;

    if (toplevelcount == 1) {
        if (tablesize == 0) {
	    long *ca;
	    ca = (long *) ihp->ip->addr.val;
	    ihp->startaddr = (int) (((Code *) (ca + WCI_CLAUSECODE)) + 
					     (*(ca+WCI_FSTART) & 0xffff));
	    emaskagg |= *(ca+WCI_EMASK);
        }
        toplevelcount = 0;		/* don't want this node counting for
					   space allocation purposes */
    }
    else if (toplevelcount == nclauses) {
        ihp->startaddr = startaddr;	/* no need to do indexing, current */
        toplevelcount = 0;		/* chain is already sufficient	   */
    }

    ihp->size = (toplevelcount ? TRYOVERHEAD : 0)
		  + toplevelcount * RETRYSIZE
		  + tablesize
		  + ihp->lowergroups * TRYOVERHEAD
		  + ihp->lowernodes * RETRYSIZE;
}


computetablesize(nentries)	/* returns table size in words */
    int nentries;
{
    return (nentries*TBENTSIZE + TBOVERHEAD);
}


long *ic_install_try();
long *ic_install_retry();
long *ic_install_trust();
long *walk_tree();

#define TREEALONE 2

addtobuf(tp,ihp)
    int tp;			/* type */
    index_header *ihp;
{
    if (ihp->size == 0) return;

    if (ihp->topnodes == 1) {	/* then we got a tree as the only node */
        long *tree_ptr;

        ihp->startaddr = (long) buf_ptr;
        tree_ptr  = buf_ptr;
        buf_ptr += computetablesize(ihp->ip->key);
        installtreeoverhead(tp,ihp->ip->key,&tree_ptr);
        tree_ptr = walk_tree(ihp->ip->addr.ptr, tree_ptr, TREEALONE);
    }
    else {
        int i,n;
        index_node *w;
        long *toplev_ptr;
        long *tree_ptr;
        Code *addr;
        int  emask;

        w = ihp->ip;
        n = ihp->topnodes;
        addr = 0;

        ihp->startaddr = (long) buf_ptr;
        toplev_ptr = buf_ptr;
        buf_ptr += (n*RETRYSIZE + TRYOVERHEAD);

        for (i=1; i<=n; i++,w=w->rlink) {

	    if (w->tag == TOP) {
		tree_ptr = buf_ptr;
		addr = (Code *) tree_ptr;
		buf_ptr += computetablesize(w->key);
		installtreeoverhead(tp,w->key,&tree_ptr);
		tree_ptr = walk_tree(w->addr.ptr, tree_ptr, i==n);
		emask = 0;
            }
	    else {
		emask = *(((long *) w->addr.val) + WCI_EMASK);
		if (i == n)
		    addr = clauseCode(w->addr.val) + dstartCode(w->addr.val);
		else
		    addr = (Code *) (((long *) w->addr.val) + WCI_CLAUSECODE);
	    }
	 
	    if (i == 1) {
		toplev_ptr = ic_install_try(toplev_ptr, addr, nargs);
	    }
	    else if (i==n) {
		toplev_ptr = ic_install_trust(toplev_ptr, addr, nargs, emask);
	    }
	    else {
		toplev_ptr = ic_install_retry(toplev_ptr, addr, nargs, emask);
	    }
        } /* for */
    } /* else */
}  /* addtobuf */


long *walk_tree(ip,tp,isdet)
    index_node *ip;
    long *tp;		/* address of roving tree pointer */
{
    if (ip != NULLNODE) {

        /*
         * Do an inorder traversal of the tree
         */

        tp = walk_tree(ip->llink,tp,isdet);


        *tp++ = ip->key;
	if (ip->tag == TVAL) {
	    *tp++ = (long) (clauseCode(ip->addr.val) + 
				(isdet ? dstartCode(ip->addr.val) : 0));
	    if (isdet == TREEALONE)
		emaskagg |= emaskCode(ip->addr.val);
	}
	else {
	    *tp++ = (long) buf_ptr;
	    walk_bottom(ip->addr.ptr,isdet);
	}

	return walk_tree(ip->rlink,tp,isdet);
    }
    else
	return tp;
}


extern Code *ic_install_tree_overhead();

extern wm_sw_const(), wm_sw_struct();

/*
 * installtreeoverhead installs code for the switch_on_constant and
 * switch_on_structure sequences.
 */

static installtreeoverhead(tp, nentries, lptr)
    int tp;		/* type of the entry */
    int nentries;	/* number of entries */
    long **lptr;	/* address of pointer to where the tree overhead */
			/* will go */
{
    long *swaddr;
    switch (tp) {
	case ITP_CONST :
	    swaddr = (long *) wm_sw_const;
	    break;
	case ITP_STRUCT :
	    swaddr = (long *) wm_sw_struct;
	    break;
	default :
            fprintf(stderr,"Error:  Bad type in installtreeoverhead\n");
	    als_exit(1);
            break;
    }

    *lptr = (long *) ic_install_tree_overhead(swaddr,nentries,*lptr);
}



walk_bottom(ip,isdet)
   index_node *ip;
   int isdet;		/* The "is determinate" flag */
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
			emaskCode(w->addr.val)  );
	}
	else {
	    buf_ptr = ic_install_retry( buf_ptr,
					clauseCode(w->addr.val),
					nargs,
					emaskCode(w->addr.val) );
	}


	w = w->rlink;
    } while (w != ip);
}


long *get_clause_information(addr,tp,key,clausestart)
    long *addr;
    int *tp;
    int *key;
    long *clausestart;
{
    int k;
    *key = k = *(addr + WCI_FIRSTARGKEY);
    switch (k & MTP_TAGMASK) {
        case MTP_UNBOUND : 
	    *tp = ITP_VAR;
	    break;
        case MTP_LIST    : 
	    *tp = ITP_LIST;
	    break;
#ifdef MTP_CONST
        case MTP_CONST :
 	    if ((k & MTP_CONSTMASK) == MTP_SYM && MFUNCTOR_ARITY(k) != 0)
		*tp = ITP_STRUCT;
	    else
		*tp = ITP_CONST;
	    break;
#else
        case MTP_SYM :
	    if (MFUNCTOR_ARITY(k) != 0)
		*tp = ITP_STRUCT;
	    else
		*tp = ITP_CONST;
	    break;
        case MTP_INT :
	    *tp = ITP_CONST;
	    break;
#endif
        default :
	    *tp = ITP_VAR;
	    break;
    }
    *clausestart = (long) addr;
    return (long *) *(addr + WCI_NEXTCLAUSEADDR);
}
