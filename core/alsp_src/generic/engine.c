#include "defs.h"
#include <errno.h>
#include "module.h"

#include "engine.h"

#undef wm_heapbase
#undef wm_stackbot
#undef wm_trailbase

#undef wm_gvbase
#undef wm_gvfreelist
#undef gv_setcnt

#undef wm_B
#undef wm_HB
#undef wm_SPB
#undef wm_E
#undef wm_TR
#undef wm_H
#undef wm_SP
#undef wm_FAIL

#undef als_mem

#undef builtins_initialized
#undef pegvnum
#undef top_module
#undef module_stack
#undef top_clausegroup
#undef clausegroup_stack
#undef module_table
#undef use_table
#undef nmods
#undef nuses
#undef default_uses
#undef default_usemax
#undef default_procs
#undef default_procmax
#undef ts_allocidx
#undef ts_allocated
#undef ts_prime
#undef ts_cutoff
#undef ts_next
#undef toktable
#undef hashtable
#undef strings
#undef strings_last
#undef strings_next
#undef char_to_tok_map
#undef w_freeptr
#undef w_totspace
#undef w_totspace
#undef w_timestamp
#undef w_reconstamp
#undef w_tofreelist
#undef w_tofreesize
#undef w_nametable
#undef codeblock_list
#undef ane_entbase
#undef ane_entptr

#undef perrcount
#undef prs_erc_index
#undef obp_stack_top

#undef wm_fail
#undef wm_trust_fail
#undef wm_return_success
#undef wm_special
#undef wm_rungoal_code
#undef wm_panic
#undef wm_cutaddr
#undef wm_overcode
#undef rungoal_modpatch
#undef rungoal_goalpatch

#undef wm_normal
#undef wm_safety
#undef wm_trigger
#undef wm_interrupt_caught
#undef wm_in_Prolog
#undef wm_spying

#undef wm_aborted


#define THROW_GENERIC_ERROR(n) Failure((n), 0)
#define Register_Stack_Overflow 0
#define Register_Stack_Underflow 0

#define CHECK(x) ASSERT(x)

void init_prolog_globals(void)
{
	os_init_prolog_globals();
	thread_init();
}

prolog_engine *create_prolog_engine(size_t stack_size, size_t heap_size)
{
	prolog_engine *pe;
	
	pe = malloc(sizeof(prolog_engine));
	
	init_prolog_engine(pe, stack_size, heap_size);
	
	return pe;
}

void delete_prolog_engine(prolog_engine *pe)
{
	shutdown_prolog_engine(pe);
	
	free(pe);
}

void chpt_init(PE);

void init_prolog_engine(prolog_engine *pe, size_t stack_size, size_t heap_size)
{	
	if (sizeof(PCell) != sizeof(PWord)) fatal_error(0,0);
	
	//	init_db(pe->db);
	
	alloc_prolog_memory(pe, stack_size, heap_size);

	pe->reg_stack_base = pe->reg_stack_top = pe->register_stack;


	pe->reg.H.ptr = pe->reg.HB.ptr = pe->heap_base;
	
	pe->reg.SP.ptr = pe->reg.SPB.ptr = pe->stack_base;
	pe->reg.E.ptr = pe->stack_base;
	
	pe->reg.B.ptr = NULL;

	pe->reg.TR.ptr = pe->trail_base;

	pe->reg.rFAIL.code = pe->db->wm_panic;
	
	pe->wm_normal = DEFAULT_SAFETY;
	pe->wm_safety = DEFAULT_SAFETY;
	pe->wm_trigger = -1;
	pe->wm_interrupt_caught = 0;
	pe->wm_in_Prolog = 0;
	pe->wm_spying = 0;

	pe->wm_aborted = 0;

	chpt_init(pe);

	ASSERT(prolog_engine_invariant(pe));
}

void init_db(prolog_database *db)
{
	db->builtins_initialized = 0;
	db->pegvnum = -5;
#if 0
    //no init int  *top_module;
    //no init int *module_stack;
    //no init int  *top_clausegroup;
    //no init int *clausegroup_stack;
    //no init struct mtbl_entry *module_table;
    //no init struct use_entry *use_table;
    //no init int nmods;
    //no init int nuses;
    //no init long *default_uses;
    //no init long default_usemax;
    //no init struct defprocs *default_procs;
    //no init long default_procmax;
#endif
	db->nmods = 0;
	db->nuses = 0;

	db->ts_allocidx = 0;
	db->ts_allocated = 2048;
	db->ts_prime = 2039;
	db->ts_cutoff = 1712;
	db->ts_next = TK_EOF + 1;
	db->toktable = 0;
#if 0
    //no init hashtable;
    //no init UCHAR *strings;
    //no initUCHAR *strings_last;
#endif
	db->strings_next = 0;
#if 0
	// no init long *char_to_tok_map;
#endif
	db->w_freeptr = 0;
	db->w_totspace = 0;
	db->w_timestamp = 0;
	db->w_reconstamp = 0;
	db->w_tofreelist = 0;
	db->w_tofreesize = 0;
	db->db_w_nametable = 0;
	db->codeblock_list = 0;
	db->ane_entbase = 0;
	db->ane_entptr = 0;
	
	db->state = DBRS_WRITABLE;

	db->perrcount = 0;

	db->prs_erc_index = -1;
	
	db->obp_stack_top = 0;
	db->PI_modid = MODULE_GLOBAL;

}

void fix_offset(void **q, prolog_database *db, block_info old_blocks[])
{
	int i;
	char **p = (char **)q;
	
	for (i = 0; i < db->als_mem->nblocks; i++) {
		if ((long *)*p >= old_blocks[i].start
			&& (char *)*p < ((char *)old_blocks[i].start) + old_blocks[i].asize) {
			*p += (char *)db->als_mem->blocks[i].start - (char *)old_blocks[i].start;
			break;
		}
	}
/*//	if (i == db->als_mem->nblocks)*/
/*//		printf("offset error\n");*/
}

extern long *
alloc_big_block(size_t size, int fe_num);

#if 0
static size_t db_store_size(prolog_database *db)
{
	sum size of blocks
}

static void store_db(prolog_database *db, void *store)
{
	for each block, copy to store
}

static prolog_database *load_db(void *store)
{
	prolog_database *db;
	
	db = (prolog_database *)store;
	
	relocate_db(db);
	
	return db;
}
#endif

static void relocate_db(prolog_database *db, block_info old_blocks[]);

#define THROW_ANSI_ERROR(n) \
{ fprintf(stderr, "errno: %d (%s)\n", (n), strerror(n)); exit(EXIT_ERROR); }

static void safe_fseek(FILE *stream, long int offset, int wherefrom)
{
  if (fseek(stream, offset, wherefrom) == -1) THROW_ANSI_ERROR(errno);
}

static FILE *safe_fopen(const char *filename, const char *mode)
{
	FILE *result;

	result = fopen(filename, mode);
	if (result == NULL) THROW_ANSI_ERROR(errno);
	
	return result;
}

static void safe_fwrite(const void *ptr, size_t size, size_t nmemb,
			FILE *stream)
{
  size_t count;
  count = fwrite(ptr, size, nmemb, stream);
  if (count != nmemb) THROW_ANSI_ERROR(errno);
}

static void safe_fread(void *ptr, size_t size, size_t nmemb, FILE
		       *stream)
{
  size_t count;
  count = fread(ptr, size, nmemb, stream);
  if (count != nmemb) THROW_ANSI_ERROR(errno);
}

static void file_load(void *f, void *data, long length, long offset)
{
	safe_fseek(f, offset, SEEK_SET);
	safe_fread(data, length, 1, f);
}

static void file_store(void *f, void *data, long length, long offset)
{
	safe_fseek(f, offset, SEEK_SET);
	safe_fwrite(data, length, 1, f);
}

static void safe_freopen(const char *filename, const char *mode,
			 FILE *stream)
{
	FILE *result;

	result = freopen(filename, mode, stream);
	if (result == NULL) THROW_ANSI_ERROR(errno);
}


void store_db_file(prolog_database *db, const char *file_name, long offset)
{
	FILE *f;
	
	/* Open the file with a+ to create it if necessary, then
           reopen with r+ to allow writing to the interior of the
           file. */
	f = safe_fopen(file_name, "a+b");
	safe_freopen(file_name, "r+b", f);
	store_db(db, f, offset, file_store);
	fclose(f);
}

void load_db_file(prolog_database *db, const char *file_name, long offset)
{
	FILE *f;
	f = safe_fopen(file_name, "rb");
	load_db(db, f, offset, file_load);
	fclose(f);
}

#ifdef Threaded
static void thread_byte_convert_db(prolog_database *db, int direction);
#endif

void store_db(prolog_database *db, void *f, long offset, 
	void (*store)(void *f, void *data, long length, long offset))
{
	int i;

#ifdef Threaded
	thread_byte_convert_db(db, convert_byte);
#endif	

	store(f, db, sizeof(*db), offset);
	offset += sizeof(*db);
	
	for (i = 0; i < db->als_mem->nblocks; i++) {
		store(f, db->als_mem->blocks[i].start, db->als_mem->blocks[i].asize, offset);
		offset += db->als_mem->blocks[i].asize;
	}

#ifdef Threaded
	thread_byte_convert_db(db, convert_thread);
#endif
}

void load_db(prolog_database *db, void *f, long offset,
		void (*load)(void *f, void *data, long length, long offset))
{
	struct am_header header;
	int i;
	
	load(f, db, sizeof(*db), offset);
	offset += sizeof(*db);
	load(f, &header, sizeof(header), offset);
	/* do not inc offset, because we re-read header into first block */
	
	db->als_mem = (struct am_header *)alloc_big_block(header.blocks[0].asize, FE_ALS_MEM_INIT);
	load(f, db->als_mem, header.blocks[0].asize, offset);
	offset += header.blocks[0].asize;
	db->als_mem->blocks[0].start = (long *)db->als_mem;
	db->als_mem->blocks[0].asize = header.blocks[0].asize;
	
	for (i = 1; i < header.nblocks; i++) {
		db->als_mem->blocks[i].start = alloc_big_block(db->als_mem->blocks[i].asize, FE_ALS_MEM_INIT);
		db->als_mem->blocks[i].asize = db->als_mem->blocks[i].asize;
		load(f, db->als_mem->blocks[i].start, db->als_mem->blocks[i].asize, offset);
		offset += db->als_mem->blocks[i].asize;
	}
	
	relocate_db(db, (block_info *)header.blocks);
}

void move_db(prolog_database *db)
{
	prolog_database *new_db;
	int i;
	
	new_db = malloc(sizeof(*new_db));
	*new_db = *db;
	
	/* copy the first big block */
	new_db->als_mem = (struct am_header *)alloc_big_block(db->als_mem->blocks[0].asize, FE_ALS_MEM_INIT);
	memcpy(new_db->als_mem, db->als_mem, db->als_mem->blocks[0].asize);
	new_db->als_mem->blocks[0].start = (long *)new_db->als_mem;
	new_db->als_mem->blocks[0].asize = db->als_mem->blocks[0].asize;
	
	/* Copy any other big blocks */
	for (i = 1; i < db->als_mem->nblocks; i++) {
		new_db->als_mem->blocks[i].start = alloc_big_block(db->als_mem->blocks[i].asize, FE_ALS_MEM_INIT);
		new_db->als_mem->blocks[i].asize = db->als_mem->blocks[i].asize;
		memcpy(new_db->als_mem->blocks[i].start, db->als_mem->blocks[i].start, db->als_mem->blocks[i].asize);		
	}
	
	relocate_db(new_db, (block_info *)db->als_mem->blocks);

	/* Erase old blocks */
	{
		int nblocks = db->als_mem->nblocks;
	for (i = nblocks-1; i >= 0; i--) {
		memset(db->als_mem->blocks[i].start, 0xFF, db->als_mem->blocks[i].asize);
	}
	}
	
	/* Free old blocks someday */

	/* Replace old with new */
	*db = *new_db;
	free(new_db);
}
	
static void relocate_db(prolog_database *db, block_info *old_blocks)
{
	int i;
	/* Do offsets */
	
#define FIX_OFFSET(p) fix_offset(((void **)p), db, old_blocks)
	
	/* Fix pointers in amheader */
	FIX_OFFSET(&db->als_mem->freelist);
	
	{
	long *p;
	for (p = db->als_mem->freelist; p; p = FB_NEXT(p)) {
		if (FB_NEXT(p)) FIX_OFFSET(&FB_NEXT(p));
	}
	}
	
	/* Fix db pointers */
	FIX_OFFSET(&db->top_module);
	FIX_OFFSET(&db->module_stack);
	
	FIX_OFFSET(&db->top_clausegroup);
	FIX_OFFSET(&db->clausegroup_stack);
	
	FIX_OFFSET(&db->module_table);
	FIX_OFFSET(&db->use_table);
	
	FIX_OFFSET(&db->default_uses);
	FIX_OFFSET(&db->default_procs);
	
	FIX_OFFSET(&db->toktable);
	FIX_OFFSET(&db->hashtable);
	
	for (i = 0; i < db->ts_prime; i++) {
		FIX_OFFSET(&db->toktable[i].tkname);
		if (db->hashtable[i]) FIX_OFFSET(&db->hashtable[i]);
	}
	
	FIX_OFFSET(&db->strings);
	FIX_OFFSET(&db->strings_last);
	FIX_OFFSET(&db->strings_next);
	
	FIX_OFFSET(&db->char_to_tok_map);
	
	FIX_OFFSET(&db->w_freeptr);
	
	{
		long *p;
	for (p = db->w_freeptr; 1; ) {
		FIX_OFFSET((long **)p+WCI_BLINK);
		FIX_OFFSET((long **)p+WCI_FLINK);
		p = *((long **) p + WCI_FLINK);
		if (p == db->w_freeptr) break;
	}
	}
	
	FIX_OFFSET(&db->w_tofreelist);
	
	{
		long *p;
	p = db->w_tofreelist;
	while (p) {
		if (*((long **) p + WCI_NEXTCLAUSEADDR))
			FIX_OFFSET((long **) p + WCI_NEXTCLAUSEADDR);
		p = *((long **) p + WCI_NEXTCLAUSEADDR);
	}
	}
	
	FIX_OFFSET(&db->db_w_nametable);
	
	{
		for (i = 0; i < NTBL_SIZE; i++) {
			if (db->db_w_nametable[i]) {
				FIX_OFFSET(&db->db_w_nametable[i]);
				
				if (db->db_w_nametable[i]->first_clause) {
					long *p;
					FIX_OFFSET(&db->db_w_nametable[i]->first_clause);
					p = db->db_w_nametable[i]->first_clause;
					while (p) {
						FIX_OFFSET(&nextClauseAddr(p));
						relocate_code(choiceEntry(p), sizeCode(p), db, old_blocks);
						p = next_clause_db(db, p);
					}
				}
				if (db->db_w_nametable[i]->last_clause) FIX_OFFSET(&db->db_w_nametable[i]->last_clause);
				if (db->db_w_nametable[i]->index_block) {
					long *p;
					FIX_OFFSET(&db->db_w_nametable[i]->index_block);
					p = db->db_w_nametable[i]->index_block;
					relocate_code(choiceEntry(p)+2, sizeCode(p), db, old_blocks);
				}
				relocate_code(db->db_w_nametable[i]->overflow, (int) (NTBL_OVERFLOWSIZE 
	                               + NTBL_CALLENTRYSIZE
				       + NTBL_EXECENTRYSIZE
				       + NTBL_CODESIZE), db, old_blocks);
			}
		}
	}
	
	FIX_OFFSET(&db->codeblock_list);
	
	{
		struct codeblock *p;
		
		for (p = db->codeblock_list; p; p = p->next) {
			FIX_OFFSET(&p->addr);
			FIX_OFFSET(&p->next);
		}
	}
	
	FIX_OFFSET(&db->ane_entbase);
	FIX_OFFSET(&db->ane_entptr);

	FIX_OFFSET(&db->wm_fail);
	relocate_code(db->wm_fail, 64, db, old_blocks);
	FIX_OFFSET(&db->wm_trust_fail);
	FIX_OFFSET(&db->wm_return_success);
	FIX_OFFSET(&db->wm_special);
	FIX_OFFSET(&db->wm_rungoal_code);
	FIX_OFFSET(&db->wm_panic);
	FIX_OFFSET(&db->wm_cutaddr);
	FIX_OFFSET(&db->wm_overcode);
	FIX_OFFSET(&db->rungoal_modpatch);
	FIX_OFFSET(&db->rungoal_goalpatch);

#undef FIX_OFFSET
	
	
}

#ifdef Threaded
static void thread_byte_convert_db(prolog_database *db, int direction)
{
	int i;

	for (i = 0; i < NTBL_SIZE; i++) {
	  if (db->w_nametable[i]) {
	    if (db->w_nametable[i]->first_clause) {
	      long *p;
	      p = db->w_nametable[i]->first_clause;
	      while (p) {
		thread_byte_convert_clause(choiceEntry(p), sizeCode(p), db, direction);
		p = next_clause(p);
	      }
	    }
	    if (db->w_nametable[i]->index_block) {
	      long *p;
	      p = db->w_nametable[i]->index_block;
	      thread_byte_convert_clause(choiceEntry(p)+2, sizeCode(p), db, direction);
	    }
	    thread_byte_convert_clause(db->w_nametable[i]->overflow,
				       (int) (NTBL_OVERFLOWSIZE
					      + NTBL_CALLENTRYSIZE
					      + NTBL_EXECENTRYSIZE
					      + NTBL_CODESIZE), db, direction);
	  }
	}

	
	thread_byte_convert_clause(db->wm_fail, 64, db, direction);
}
#endif

void shutdown_prolog_engine(prolog_engine *pe)
{
	free_prolog_memory(pe);
}

void push_register_stack(prolog_engine *pe)
{
	if (pe->reg_stack_top >= pe->reg_stack_base + REGISTER_STACK_MAX) {
		THROW_GENERIC_ERROR(Register_Stack_Overflow);
	}
	
	*pe->reg_stack_top = pe->reg;
	pe->reg_stack_top++;
}

void pop_register_stack(prolog_engine *pe)
{
	if (pe->reg_stack_top <= pe->reg_stack_base) {
		THROW_GENERIC_ERROR(Register_Stack_Underflow);
	}
	pe->reg_stack_top--;
	pe->reg = *pe->reg_stack_top;
}

static int heap_cell_invariant(const prolog_engine *pe, PCell *p)
{
	int size;
	
	switch (MTP_TAG(p->sint)) {
	case MTP_UNBOUND:
	case MTP_STRUCT:
	case MTP_LIST:
		CHECK(p->ptr > pe->heap_base && p->ptr < pe->reg.H.ptr);
		size = 1;
		break;
	case MTP_CONST:
		switch (MTP_CONSTTAG(p->sint)) {
		case MTP_INT:
		case MTP_SYM:
		case MTP_UIA:
		  /* nothing to do */
			size = 1;
			break;
		case MTP_FENCE:
			size = MFENCE_VAL(p->sint) + 1;
			{
#if 0
			  // Make this correct
			  // PCell *endfence = p+size;
			  //printf("size: %d\n", size);
			  //CHECK(MTP_TAG(endfence->sint) == MTP_UIA);
			  //CHECK(MFENCE_VAL(endfence->sint) == size -1);
#endif
			}
			break;
		default:
		  size = 0;
		  break;
		}
		break;
	default:
	  size = 0;
	  break;
	}
	
	return size;
}

static void global_cell_invariant(const prolog_engine *pe, PCell *p)
{
	switch (MTP_TAG(p->sint)) {
	case MTP_UNBOUND:
	case MTP_STRUCT:
	case MTP_LIST:
		CHECK((p->ptr >= pe->trail_base && p->ptr < pe->globals_base)
			  || (p->ptr > pe->heap_base && p->ptr < pe->reg.H.ptr));
		break;
	}
}

static void register_set_invariant(const prolog_engine *pe, const register_set *rs)
{
	CHECK(rs->B.ptr == NULL || (rs->B.ptr >= rs->TR.ptr && rs->B.ptr < pe->trail_base));
	CHECK(rs->HB.ptr >= pe->heap_base && rs->HB.ptr <= rs->H.ptr);
	CHECK(rs->SPB.ptr >= rs->SP.ptr && rs->SPB.ptr <= pe->heap_base);
	CHECK(rs->E.ptr >= rs->SP.ptr && rs->E.ptr <= pe->heap_base);
	CHECK(rs->TR.ptr > rs->H.ptr
		  && (rs->B.ptr == NULL || rs->TR.ptr <= rs->B.ptr));
	CHECK(rs->H.ptr >= pe->heap_base && rs->H.ptr < rs->TR.ptr);
	CHECK(rs->SP.ptr > pe->stack_max && rs->SP.ptr <= rs->SPB.ptr);
}


int prolog_engine_invariant(const prolog_engine *pe)
{
	PCell *p, *t, *q;
	choice_point *b;
	register_set *rs;
	
	/* Check memory areas */
	
	CHECK(pe->stack_max < pe->heap_base);
	CHECK(pe->heap_base < pe->trail_base);
	
	CHECK(((unsigned long)pe->memory_base & 0x80000000)
		== ((unsigned long)(pe->memory_base + pe->stack_size + pe->heap_size) & 0x80000000));
	
	/* Check registers */

	register_set_invariant(pe, &pe->reg);
	for (rs = pe->reg_stack_base; rs < pe->reg_stack_top; rs++)
		register_set_invariant(pe, rs);	
	

	/* Check heap */

	for (p = pe->heap_base; p < pe->reg.H.ptr; ) {
		p += heap_cell_invariant(pe, p);
	}

	/* Check trail */
	for (b = pe->reg.B.cp, t = pe->reg.TR.ptr; b;
		 t = (PCell *)(b + 1), b = b->B.cp) {
		CHECK((b->B.cp >= pe->reg.B.cp && b->B.ptr <= pe->trail_base && b->B.cp >= b + 1) || b->B.cp == 0);
		CHECK(b->SPB.ptr >= pe->reg.SP.ptr && b->SPB.ptr <= pe->heap_base);
		
		CHECK(b->HB.ptr <= pe->reg.H.ptr && b->HB.ptr >= pe->heap_base);
		for (q = t ; (void *)q < (void *)b; q++)
			CHECK(q->ptr >= pe->reg.SP.ptr && q->ptr <= pe->reg.H.ptr);
	}
	
	/* Check stack */
	
	/* Check globals */
	for (p = pe->trail_base; p < pe->globals_base; p++) {
		/* Skip pointers on the free list */
			global_cell_invariant(pe, p);
	}

	return 1;
}

static void resize_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	PCell *old_memory_base = pe->memory_base;
	long offset;
	
	realloc_prolog_memory(pe, new_stack_size, new_heap_size);
	
	pe->stack_size = new_stack_size;
	pe->heap_size = new_heap_size;
	
	/* memory may have moved, so update pointers */
	
	offset = pe->memory_base - old_memory_base;
	
	pe->stack_max += offset;
	pe->stack_base += offset;
	pe->heap_base += offset;
	pe->trail_base += offset;
	pe->globals_top += offset;
	pe->globals_base += offset;
	if (pe->globals_free_list.uint != MMK_INT(-1)) pe->globals_free_list.ptr += offset;
	
	pe->reg.SP.ptr += offset;
	pe->reg.E.ptr += offset;
	pe->reg.SPB.ptr += offset;
	pe->reg.HB.ptr += offset;
	pe->reg.H.ptr += offset;
	pe->reg.TR.ptr += offset;
	if (pe->reg.B.ptr) pe->reg.B.ptr += offset;
}

static void shift_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	long stack_heap_offset, trail_offset;
	
	stack_heap_offset = (pe->memory_base + new_stack_size) - pe->heap_base;
	trail_offset = (PCell *)(pe->heap_base + stack_heap_offset + new_heap_size - 1) - pe->trail_base;
	
	memmove(pe->reg.SP.ptr + stack_heap_offset, pe->reg.SP.ptr, (pe->reg.H.ptr - pe->reg.SP.ptr)*sizeof(PCell));
	memmove(pe->reg.TR.ptr + trail_offset, pe->reg.TR.ptr, (pe->globals_base - pe->reg.TR.ptr )*sizeof(PCell));

	pe->stack_max += 0;
	pe->stack_base += stack_heap_offset;
	pe->heap_base += stack_heap_offset;
	pe->trail_base += trail_offset;
	pe->globals_top += trail_offset;
	pe->globals_base += trail_offset;
	if (pe->globals_free_list.uint != MMK_INT(-1)) pe->globals_free_list.ptr += trail_offset;

	pe->reg.SP.ptr += stack_heap_offset;
	pe->reg.E.ptr += stack_heap_offset;
	pe->reg.SPB.ptr += stack_heap_offset;
	pe->reg.HB.ptr += stack_heap_offset;
	pe->reg.H.ptr += stack_heap_offset;
	pe->reg.TR.ptr += trail_offset;
	if (pe->reg.B.ptr) pe->reg.B.ptr += trail_offset;
}

static int do_offset(PCell *p, long offset)
{
	int size;
	
	switch (MTP_TAG(p->sint)) {
	case MTP_UNBOUND:
	case MTP_STRUCT:
	case MTP_LIST:
#if 0
		if ((long *)(*p+offset*4) <= wm_stackbot || (long *)(*p+offset*4) >= wm_gvbase)
			printf("problem\n"); 
#endif
		p->ptr += offset;
		size = 1;
		break;
	case MTP_CONST:
		switch (MTP_CONSTTAG(p->sint)) {
		case MTP_INT:
		case MTP_SYM:
		case MTP_UIA:
		  /* nothing to do */
			size = 1;
			break;
		case MTP_FENCE:
			size = MFENCE_VAL(p->sint) + 1;
			break;
		default:
		  size = 0;
		  break;
		}
		break;
	default:
	  size = 0;
	  break;
	}
	
	return size;
}

static void relocate_prolog_memory(prolog_engine *pe, long stack_heap_offset, long trail_offset)
{
	register_set *s;
	PCell *p, *t;
	choice_point *b;
	
	/* fix register stack */

	for (s = pe->reg_stack_base; s < pe->reg_stack_top; s++) {
		s->SP.ptr += stack_heap_offset;
		s->E.ptr += stack_heap_offset;
		s->SPB.ptr += stack_heap_offset;
		s->HB.ptr += stack_heap_offset;
		s->H.ptr += stack_heap_offset;
		s->TR.ptr += trail_offset;
		if (s->B.ptr) s->B.ptr += trail_offset;
	}
	
	/* scan stack */

	/* Brute force approach! */
	{
		for (p = pe->reg.E.ptr; p < pe->stack_base; p++) {
			if (p->ptr > pe->stack_max - stack_heap_offset && p->ptr < pe->trail_base - trail_offset)
			{
				do_offset(p, stack_heap_offset);
			}
		}
	}
	
	/* fix heap */
	
	for (p = pe->heap_base; p < pe->reg.H.ptr; ) {
		p += do_offset(p, stack_heap_offset);
	}
	
	/* scan trail */

	for (b = pe->reg.B.cp, t = pe->reg.TR.ptr; b; t = (PCell *)(b + 1), b = b->B.cp) {
		if (b->B.ptr) b->B.ptr += trail_offset;
		b->SPB.ptr += stack_heap_offset;
		b->HB.ptr += stack_heap_offset;
		for (p = t ; (void *)p < (void *)b; p++) p->ptr += stack_heap_offset;
	}			
		
	/* globals */

	if (pe->globals_free_list.uint != MMK_INT(-1)) {
		/* First adjust the free lists by trail_offset */
		for (p = pe->globals_free_list.ptr; p->uint != MMK_INT(-1); p = p->ptr) {
			p->ptr += trail_offset;
		}
		/* Then adjust the rest, skiping over free list items */
		for (p = pe->trail_base; p < pe->globals_base; p++) {
			if (MTP_TAG(p->sint) != MTP_CONST && !(p->ptr >= pe->trail_base && p->ptr < pe->globals_base)) {
				do_offset(p, stack_heap_offset);
			}
		}
	}
}

int size_prolog_engine(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
TRY {
	long diff = (long)(new_stack_size + new_heap_size) - (long)(pe->stack_size + pe->heap_size);
	PCell *old_heap_base = pe->heap_base, *old_trail_base = pe->trail_base;
	
	prolog_engine_invariant(pe);
	
	if (diff > 0) {
		resize_prolog_memory(pe, new_stack_size, new_heap_size);
		shift_prolog_memory(pe, new_stack_size, new_heap_size);
	} else if (diff == 0) {
		shift_prolog_memory(pe, new_stack_size, new_heap_size);
	} else if (diff < 0) {
		shift_prolog_memory(pe, new_stack_size, new_heap_size);
		resize_prolog_memory(pe, new_stack_size, new_heap_size);
	}
	
	relocate_prolog_memory(pe, pe->heap_base - old_heap_base, pe->trail_base - old_trail_base);

	ASSERT(prolog_engine_invariant(pe));
	
	return 1;
} ONEXCEPTION {
	ClearFailure();
	return 0;
} ENDTRY
}

int pbi_resize_memory(PE)
{
	PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    
    size_prolog_engine(hpe, v1, v2);
	
	SUCCEED;
}

#if 0
#define env_E(e) 	*(PWord **)(e)

static void relocate_heap(long offset)
{
	PWord *p, *e, *b, *t;
	int i;
	
	PWord *old_wm_stackbot = wm_stackbot;
	PWord *old_wm_trailbase = wm_trailbase;
	/* fix pointers! */

	wm_stackbot += offset;
	wm_stackbot_safety += offset;
	wm_heapbase += offset;
	wm_trailbase += offset;
	wm_gvbase += offset;
	wm_gvfreelist += offset;

	/* registers */

	for (i = 0; i <= wm_regidx; i++) {	
		wm_regs[i][wm_B_idx] += offset;
		wm_regs[i][wm_HB_idx] += offset;
		wm_regs[i][wm_SPB_idx] += offset;
		wm_regs[i][wm_E_idx] += offset;
		wm_regs[i][wm_TR_idx] += offset;
		wm_regs[i][wm_H_idx] += offset;
		wm_regs[i][wm_SP_idx] += offset;
	}
	
	/* scan heap */
	
	for (p = wm_heapbase; p < wm_H; ) {
		p += do_offset(p, offset);
	}
	
	/* scan trail*/
	
	for (b = wm_B, t = wm_TR; b; t = b+4 , b = chpt_B(b)) {
		if (chpt_B(b)) chpt_B(b) += offset;
		for (p = t ; p < b; p++) *p += offset*4;
		chpt_SPB(b) += offset;
		chpt_HB(b) += offset;
	}
			
		
	/* scan stack */

#if 0
	// first fix e chain
	for (e = wm_E; e < wm_heapbase; e = env_E(e)) {
		env_E(e) += offset;
	}

	for (b = wm_B, e = wm_E; b; b = chpt_B(b)) {
		long *spb, *next_e;
		spb = (long *) (((long) chpt_SPB(b)) & ~1);
		next_e = env_E(e);
		if (spb <= next_e) {
			for (p = e+2; p < spb; p++) {
				do_offset(p, offset);
			}
		} else {
			for (; e < spb; e = next_e, next_e = env_E(e)) {
				if (spb < next_e) {
					for (p = e+2; p < spb; p++)
						do_offset(p, offset);
				} else {
					for (p = e+2; p < next_e; p++)
						do_offset(p, offset);
				}
			}
		}
	}
#endif

#if 0
	{
	PWord *next_e;
	for (e = wm_E, b = wm_B; e < wm_heapbase ; e = next_e) {
		*e += offset*4;

		if (b && ((PWord)chpt_SPB(b) & ~1) <= *e) {
			next_e = (long *) ((long) chpt_SPB(b) & ~1);
			b = chpt_B(b);
			if (b == 0) break;
		} else
		{
			next_e = (PWord *)*e;
		}
		
		for (p = e+2; p < next_e; p++) do_offset(p, offset);
	}
	}
	
#endif
#if 0
	for (f = wm_E, b = wm_B; chpt_B(b) ; ) {
		if (chpt_SPB(b) <= *f) {
			next_f = 	
	} 
#endif

	/* Brute force approach! */
	{
		for (p = wm_E; p < wm_heapbase; p++) {
			if ((long *)*p > old_wm_stackbot && (long *)*p < old_wm_trailbase) {
				do_offset(p, offset);
			}
		}
	}
	//globals

	for (p = wm_trailbase; p < wm_gvbase; p++) {
		do_offset(p, offset);
	}
}

void *heap_ring[4] = {0,0,0,0};
int heap_ring_index = 0;
void newheap(void);
void newheap(void)
{
	PWord *new_wm_stackbot, *old_wm_stackbot, *p;
	size_t heapsize, stacksize, size;
	long offset;

	prolog_control_invariant();
	heapsize = wm_gvbase - wm_heapbase + 1;
	stacksize = wm_heapbase - wm_stackbot;
	size = heapsize+stacksize;
		
	old_wm_stackbot = wm_stackbot;
		
	/* Allocate the new prolog memory */
	if (heap_ring[heap_ring_index]) new_wm_stackbot = heap_ring[heap_ring_index];
	else {
		new_wm_stackbot = reallocate_prolog_heap_and_stack(size);
		heap_ring[heap_ring_index] = new_wm_stackbot;
	}
	//new parse area
	memcpy(new_wm_stackbot, old_wm_stackbot, size * sizeof(PWord));

	// zero old area and free
	for (p = old_wm_stackbot+size-1; p >= old_wm_stackbot; p--) *p = 0xFFFFFF00;
	//free(old_wm_stackbot);
	
	offset = new_wm_stackbot - old_wm_stackbot;

	relocate_heap(offset);
	
	/* test - recopy to original positions and relocate */

#if 0
	memcpy(old_wm_stackbot, new_wm_stackbot, size * sizeof(PWord));
	
	relocate_heap(-offset);
	free(new_wm_stackbot);
#endif 

	prolog_control_invariant();

	heap_ring_index = (heap_ring_index + 1) % 4;
}
#endif
