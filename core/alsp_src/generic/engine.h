#ifndef ENGINE_H
#define ENGINE_H

#include "wintcode.h"
#include "alsmem.h"

/* Logic + Control = Programming  */

typedef struct {
	struct am_header *als_mem;


    long builtins_initialized;

	long pegvnum;


    int  *top_module;

    int *module_stack;

    int  *top_clausegroup;

    int *clausegroup_stack;

    struct mtbl_entry *module_table;

    struct use_entry *use_table;

    int nmods;

    int nuses;

    long *default_uses;

    long default_usemax;

    struct defprocs *default_procs;

    long default_procmax;

    int ts_allocidx;

    long ts_allocated;

    unsigned long ts_prime;

    unsigned long ts_cutoff;

    unsigned long ts_next;

    tkentry *toktable;

    tkentry **hashtable;

    UCHAR *strings;

    UCHAR *strings_last;

    long *strings_next;

    long *char_to_tok_map;

    long *w_freeptr;

    long w_totspace;

    unsigned long w_timestamp;

    unsigned long w_reconstamp;

    long *w_tofreelist;

    long w_tofreesize;

    ntbl_entry **w_nametable;

    struct codeblock *codeblock_list;

    ntbl_entry *ane_entbase;

    ntbl_entry *ane_entptr;

Code *wm_fail;
Code *wm_trust_fail;
Code *wm_return_success;
Code *wm_special;
Code *wm_rungoal_code;
Code *wm_panic;
Code *wm_cutaddr;
Code *wm_overcode;
PWord *rungoal_modpatch, *rungoal_goalpatch;


} prolog_database;

typedef struct {long *start; long asize; long fsize;} block_info;

#ifdef Threaded
typedef enum {convert_byte, convert_thread} dirtype;
void thread_byte_convert_clause(Code *addr, int n, prolog_database *db, dirtype direction);
#endif

void init_db(prolog_database *db);
void fix_offset(void **q, prolog_database *db, block_info old_block[]);
void move_db(prolog_database *db);
void relocate_code(Code *addr, int n, prolog_database *db, block_info old_blocks[]);
void store_db(prolog_database *db, void *f, long offset, 
	void (*store)(void *f, void *data, long length, long offset));
void load_db(prolog_database *db, void *f, long offset,
		void (*load)(void *f, void *data, long length, long offset));

void os_store_db(prolog_database *db, const char *file_name, long offset);
void os_load_db(prolog_database *db, const char *file_name, long offset);
void store_db_file(prolog_database *db, const char *file_name, long offset);
void load_db_file(prolog_database *db, const char *file_name, long offset);

union PCellUnion;

union PCellUnion {
	long sint;
	unsigned long uint;
	union PCellUnion *ptr;
};

typedef union PCellUnion PCell; 

struct choice_point_struct;

struct stack_frame_struct;

typedef union {
	PCell *ptr;
	struct choice_point_struct *cp;
	struct stack_frame_struct *sf;
	Code *code;
} PReg;

struct choice_point_struct {
	Code *next_clause;
	PReg HB;
	PReg SPB;
	PReg B;
};

typedef struct choice_point_struct choice_point;

struct stack_frame_struct {
	Code *CP;
	PReg E;
};

typedef struct stack_frame_struct stack_frame;

typedef struct {
	PReg SP, E, SPB, HB, H, TR, B, rFAIL;
} register_set;

typedef struct {
/* Prolog logic */
	
	prolog_database db;
	
/* Prolog Control */
	
	/* Registers */
	
	register_set reg;

	/* Memory */
	
	/* Register Stack */
	#define REGISTER_STACK_MAX 100
	register_set register_stack[REGISTER_STACK_MAX];
	register_set *reg_stack_top;
	register_set *reg_stack_base;

	/* Stack, Heap and Mark Area */
	unsigned long stack_size;
	unsigned long heap_size;

	PCell *memory_base;
	void *mark_area;

	int stack_protected;

	PCell *stack_max;
	PCell *stack_base;
	PCell *heap_base;
	PCell *trail_base;
	PCell *globals_top;
	PCell *globals_base;
	PCell globals_free_list;
	int globals_set_count;
	

} prolog_engine;

extern prolog_engine current_engine;

void init_prolog_globals(void);

prolog_engine *create_prolog_engine(size_t stack_size, size_t heap_size);
void delete_prolog_engine(prolog_engine *pe);

void init_prolog_engine(prolog_engine *pe, size_t stack_size, size_t heap_size);
void shutdown_prolog_engine(prolog_engine *pe);

void push_register_stack(prolog_engine *pe);
void pop_register_stack(prolog_engine *pe);

int prolog_engine_invariant(const prolog_engine *pe);

int size_prolog_engine(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size);

int pbi_resize_memory(void);

/* OS memory support */
void protect_stack(prolog_engine *pe);
void unprotect_stack(prolog_engine *pe);

void os_init_prolog_globals(void);
void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size);
void free_prolog_memory(prolog_engine *pe);
void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size);

/* Logic Compatibility macros */
#define als_mem ((long *)current_engine.db.als_mem)

#define builtins_initialized (current_engine.db.builtins_initialized)
#define pegvnum (current_engine.db.pegvnum)
#define top_module (current_engine.db.top_module)
#define module_stack (current_engine.db.module_stack)
#define top_clausegroup (current_engine.db.top_clausegroup)
#define clausegroup_stack (current_engine.db.clausegroup_stack)
#define module_table (current_engine.db.module_table)
#define use_table (current_engine.db.use_table)
#define nmods (current_engine.db.nmods)
#define nuses (current_engine.db.nuses)
#define default_uses (current_engine.db.default_uses)
#define default_usemax (current_engine.db.default_usemax)
#define default_procs (current_engine.db.default_procs)
#define default_procmax (current_engine.db.default_procmax)
#define ts_allocidx (current_engine.db.ts_allocidx)
#define ts_allocated (current_engine.db.ts_allocated)
#define ts_prime (current_engine.db.ts_prime)
#define ts_cutoff (current_engine.db.ts_cutoff)
#define ts_next (current_engine.db.ts_next)
#define toktable (current_engine.db.toktable)
#define hashtable (current_engine.db.hashtable)
#define strings (current_engine.db.strings)
#define strings_last (current_engine.db.strings_last)
#define strings_next (current_engine.db.strings_next)
#define char_to_tok_map (current_engine.db.char_to_tok_map)
#define w_freeptr (current_engine.db.w_freeptr)
#define w_totspace (current_engine.db.w_totspace)
#define w_totspace (current_engine.db.w_totspace)
#define w_timestamp (current_engine.db.w_timestamp)
#define w_reconstamp (current_engine.db.w_reconstamp)
#define w_tofreelist (current_engine.db.w_tofreelist)
#define w_tofreesize (current_engine.db.w_tofreesize)
#define w_nametable (current_engine.db.w_nametable)
#define codeblock_list (current_engine.db.codeblock_list)
#define ane_entbase (current_engine.db.ane_entbase)
#define ane_entptr (current_engine.db.ane_entptr)

/* WAM global compatability macros */

#define wm_fail (current_engine.db.wm_fail)
#define wm_trust_fail (current_engine.db.wm_trust_fail)
#define wm_return_success (current_engine.db.wm_return_success)
#define wm_special (current_engine.db.wm_special)
#define wm_rungoal_code (current_engine.db.wm_rungoal_code)
#define wm_panic (current_engine.db.wm_panic)
#define wm_overcode (current_engine.db.wm_overcode)
#define wm_cutaddr (current_engine.db.wm_cutaddr)
#define rungoal_modpatch (current_engine.db.rungoal_modpatch)
#define rungoal_goalpatch (current_engine.db.rungoal_goalpatch)

/* Control Compatibility macros */

#define wm_heapbase ((PWord *)current_engine.heap_base)
#define wm_stackbot ((PWord *)current_engine.stack_max)
#define wm_trailbase ((PWord *)current_engine.trail_base)

#define wm_gvbase	((PWord *)current_engine.globals_base)
#define wm_gvfreelist ((PWord *)current_engine.globals_free_list.ptr)
#define gv_setcnt (current_engine.globals_set_count)

#define wm_B	((PWord *)current_engine.reg.B.ptr)
#define wm_HB	((PWord *)current_engine.reg.HB.ptr)
#define wm_SPB	((PWord *)current_engine.reg.SPB.ptr)
#define wm_E	((PWord *)current_engine.reg.E.ptr)
#define wm_TR	((PWord *)current_engine.reg.TR.ptr)
#define wm_H	((PWord *)current_engine.reg.H.ptr)
#define wm_SP	((PWord *)current_engine.reg.SP.ptr)
#define wm_FAIL ((PWord *)current_engine.reg.rFAIL.code)

#endif
