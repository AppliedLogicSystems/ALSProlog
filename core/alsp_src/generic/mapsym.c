/*=====================================================================*
 |			mapsym.c             
 |      Copyright (c) 1990-1995 Applied Logic System, Inc.
 |
 |			-- maps token ids to contiguous integers
 |             for use in loadfile.c
 |
 | Creation: 12/17/90
 | Author: Kevin A. Buettner
 | Revision History:
 | 10/26/94, C. Houpt -- Fixed long word alignment bug in alloc().
 |---------------------------------------------------------------
 | Description:
 |
 |      When creating a .obp file, we will occassionally need to save a
 | representation of a symbol in the .obp file.  Since other files may have
 | already been loaded and since our source file that we are loading may
 | contain both symbols which have been used before as well as new symbols
 | which are not yet in the symbol table, the symbol indexes will not
 | necessarily be contiguous.  Moreover, we wish to save only those symbols
 | that are actually needed by the .obp file we are creating (i.e, it would
 | not do to save all symbols presently in the system).
 |
 | Therefore we need to have a representation for symbols that we can
 | easily put down in the .obp file.  The representation that we have
 | chosen is to map the symbols (assuming that there are N symbols that we
 | care about) to integers in the range 0 thru N-1.
 |
 | Prior to changing the symbol table so that it is expandable (the latter
 | part of 1990), loadfile.c would accomplish this mapping by allocating
 | an array of shorts with as many entries as the symbol table.  The array
 | of shorts was initialized to -1 and a number-of-tokens counter was
 | initialized to zero.  When we wished to map a symbol, we would simply
 | look in the array. If we found a non-negative value, this was the value
 | for use in saving the obp file.  If -1 was found, then the symbol had
 | not been seen yet by the code which built .obp files and so we would
 | use the present value of the number-of-tokens counter as the map value
 | of the symbol.  The array entry for this symbol would of course be
 | filled in with this value after which the number-of-tokens counter
 | would be incremented.
 |
 | On the PC implementation, this was ideal because the symbol table was
 | relatively small (907 entries) and was fixed in size.  Therefore it took
 | less than 2K bytes of memory to represent the map.  When the implementation
 | was moved to the SUN and other machines, the symbol table size was increased
 | but still fixed so this scheme of using an array was still appropriate, but
 | perhaps not so (space) efficient.
 |
 | Now that the symbol table is expandable (it will double in size
 | periodically), it is no longer appropriate to allocate a mapping
 | array as before.  Firstly, because the symbol table is not fixed in
 | size we don't know how large of an array to allocate.  We could just
 | assume that things will get as bad as they can and allocate an array
 | as large as the largest symbol table can be.  On the SUN, this is
 | over 1 million entries; so the mapping array (of longs) will require
 | four megabytes of memory.  Or we could just assume that we won't
 | exceed either the current size or possibly the next size up, but this
 | too is also unsatisfactory in that it is possible to construct programs
 | which would use more than this amount of space.
 |
 | Most of the time, however, there are a relatively small number of symbols
 | in any given .obp file.  In the builtins for example (at the time
 | of this writing) the largest number of symbols in any .obp file was
 | just 159 (debugger.obp).  Scott Medeiros has a module in his German
 | translator with nearly 800 symbols.  This was the largest example that
 | I could find on my system. It seems that mapping arrays allocated in the
 | manner described above will be sparse.
 |
 | So this gives us an idea for an alternate data structure -- a sparse
 | array data structure.  Essentially we will construct a hash table
 | which will work on our token indexes.  Instead of using open hashing,
 | we will resolve collisions by chasing a pointer to a bucket of a limited
 | number of entries.  If the bucket fills up, we modify the last entry
 | to point at a new bucket and so on.
 |
 | We also need to be aware of the problem of recursive consults.  If we
 | use a fixed static area for our data structure, we will get into trouble.
 | So we have a little stack along with functions to push and pop the stack.
 */

#include "defs.h"


/*
 * storage allocation:
 *
 * We have portability concerns: malloc can not be counted on to return
 * a long word aligned object (which is essential on certain machines).
 *
 * to_free is a list of areas which have been allocated and must eventually
 * be freed.  When we allocate storage, we will allocate more space than
 * necessary (3 long words). Alignment may consume up to 3 bytes.  The first
 * aligned (long) word will contain the actual block address to free
 * (non-aligned).  The second aligned long word will be a pointer to the
 * next block to free.
 *
 */


/*
 * alloc will allocate the required number of bytes and put its new block
 * on the to_free list.  The block returned will be longword aligned.
 */

static long * alloc	( size_t, long ** );
static long *
alloc(size, to_free_ptr)
    size_t size;
    long **to_free_ptr;
{
    char *unaligned = malloc(size + 12);
    long *aligned;

    if (unaligned == (char *) 0) {
	perror("alloc");
	fatal_error(FE_MAPSYM, 0);
    }

    {
    	unsigned long al, ua = (unsigned long) unaligned;
    	
    	al = ua + (4-(ua & 3)) % 4;
    	aligned = (long *) al;
    }
    *aligned = (long) unaligned;
    *(aligned + 1) = (long) *to_free_ptr;
    *to_free_ptr = aligned;
    return aligned + 2;
}


/*
 * free_em will free up the space allocated by successive calls to alloc.
 */

static	void	free_em		( long * );
static void
free_em(to_free)
    long *to_free;
{
    long *tbf;

    while (to_free) {
	tbf = to_free;
	to_free = (long *) *(tbf + 1);
	free((char *) *tbf);
    }
}

/*
 * More storage allocation:
 *
 * We will initially allocate in one big chunk, a space large enough
 * for our mapping descriptor, the hash array(s) and a number of bucket blocks.
 *
 * In both the hash arrays and the bucket blocks there will be a key/pointer
 * field and a map/tag value.  The key/pointer field will be a long/long * and
 * map/tag value will be an unsigned short.  One would conventionally use
 * a structure which might be represented as follows:
 *
 *              struct {
 *                      long            key_ptr;
 *                      unsigned short  map_tag;
 *              }
 *
 * This may work ok on certain machines, but machines which require that
 * long values be longword aligned will waste space in an array by padding
 * the second field so that the above structure requires eight bytes instead
 * of six.  Alternately, the machine may store the structure efficiently,
 * but may require extra cpu cycles to access the misaligned long words.
 *
 * To handle this problem, we separate the fields into two separate arrays
 * in both the bucket blocks and the hash array.
 *
 */

#define BBSZ 4			/* number of buckets in a bucket block */
#define HASHSZ 512		/* number of entries in hash table */
#define HASHMASK (HASHSZ-1)
			/* mask to use for HASHSZ a power of two */
#define NIBB 4			/* number of initial bucket blocks to
				 * allocate
				 */
#define NBB 32			/* number of bucket blocks to allocate later */

#define KEY_EMPTY 65535		/* map_tag value indicating empty slot */
#define KEY_BBPTR 65534		/* map_tag value indicating bucket block
				 * pointer
				 */

struct bucket_block {
    long  key_ptr[BBSZ];
    unsigned short map_tag[BBSZ];
};

static struct map_descr {
    struct map_descr *prev;
    long *mem_allocated;
    long  next_mapval;
    struct bucket_block *next_bb;
    struct bucket_block *last_bb;
    long  key_ptr[HASHSZ];
    unsigned short map_tag[HASHSZ];
    struct bucket_block initial_bbs[NIBB];
}    *mapstack;


/*
 * push_symmap is externally available and is called to push the current
 * symbol map and allocate a new one.
 */

void
push_symmap()
{
    long *to_free;
    struct map_descr *new_map;
    register int i;

    to_free = (long *) 0;
    new_map = (struct map_descr *) alloc(sizeof (struct map_descr), &to_free);

    new_map->prev = mapstack;
    new_map->mem_allocated = to_free;
    new_map->next_bb = new_map->initial_bbs;
    new_map->last_bb = new_map->initial_bbs + NIBB - 1;
    new_map->next_mapval = 0;

    for (i = 0; i < HASHSZ; i++)
	new_map->map_tag[i] = KEY_EMPTY;

    mapstack = new_map;
}


/*
 * pop_symmap is called externally to pop and deallocate the symbol-map stack
 */

void
pop_symmap()
{
    struct map_descr *old_map;

    old_map = mapstack;
    mapstack = old_map->prev;
    free_em(old_map->mem_allocated);
}


/*
 * new_bucket is called internally to get a new bucket block
 */

static	struct bucket_block * new_bucket ( void );

static struct bucket_block *
new_bucket()
{
    if (mapstack->next_bb > mapstack->last_bb) {
	mapstack->next_bb = (struct bucket_block *)
	    alloc(NBB * sizeof (struct bucket_block), &mapstack->mem_allocated);
	mapstack->last_bb = mapstack->next_bb + NBB - 1;
    }

    return mapstack->next_bb++;
}


/*
 * symmap is called to map a symbol
 */

long
symmap(tokid)
    long  tokid;
{
    int   hi = tokid & HASHMASK;
    register int i;
    register unsigned short *mt;
    register long *kp;
    long  v;

    i = BBSZ - 1;
    mt = mapstack->map_tag + hi - i;
    kp = mapstack->key_ptr + hi - i;

    for (;;) {
	v = mt[i];
	if (v == KEY_EMPTY) {
	    mt[i] = (unsigned short) (v = mapstack->next_mapval++);
	    kp[i] = tokid;
	    if (i != BBSZ - 1)
		mt[i + 1] = KEY_EMPTY;
	    break;
	}
	else if (v == KEY_BBPTR) {
	    mt = ((struct bucket_block *) kp[i])->map_tag;
	    kp = ((struct bucket_block *) kp[i])->key_ptr;
	    i = 0;
	}
	else if (kp[i] != tokid) {
	    if (i == BBSZ - 1) {
		struct bucket_block *new = new_bucket();

		new->key_ptr[0] = kp[i];
		new->map_tag[0] = v;
		new->key_ptr[1] = tokid;
		new->map_tag[1] = (v = mapstack->next_mapval++);
		new->map_tag[2] = KEY_EMPTY;
		kp[i] = (long) new;
		mt[i] = KEY_BBPTR;
		break;
	    }
	    else
		i++;
	}
	else
	    break;
    }

    return v;
}


/*
 * sym_order is called after all of the symbols have been mapped to obtain
 * a vector of the symbol ids that were encountered.  Index N in this vector
 * corresponds to map value N;  i.e, (N == symmap(vec[N])).
 */

long *
sym_order(szp)
    long *szp;
{
    long *rv;
    int   hi;
    register int i;
    register unsigned short *mt;
    register long *kp;
    register long v;

    *szp = mapstack->next_mapval;
    rv = alloc((sizeof (long)) * mapstack->next_mapval, &mapstack->mem_allocated);

    for (hi = 0; hi < HASHSZ; hi++) {
	i = BBSZ - 1;
	mt = mapstack->map_tag + hi - i;
	kp = mapstack->key_ptr + hi - i;

	while (i < BBSZ) {
	    v = mt[i];
	    if (v == KEY_EMPTY)
			i = BBSZ;	/* force exit on next iteration */
	    else if (v == KEY_BBPTR) {
			mt = ((struct bucket_block *) kp[i])->map_tag;
			kp = ((struct bucket_block *) kp[i])->key_ptr;
			i = 0;
	    }
	    else {
			rv[v] = kp[i];
			i++;
	    }
	}
    }

    return rv;
}
