#ifndef ENGINE_H
#define ENGINE_H

#include "wintcode.h"
#include "alsmem.h"
#include "compile.h"
#include "varproc.h"
#include "parsstak.h"
#include "icodegen.h"

#include <setjmp.h>

/* Logic + Control = Programming  */

typedef struct prolog_database_ {
	struct am_header *als_mem;


    long builtins_initialized;


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

//	MUTEX symtab_mutex;
	
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

    ntbl_entry **db_w_nametable;

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

/* wintcode.c */
long *aib_clause_addr;		/* used by assertz */

/* index.c */
long *ibuf_ptr;

long inargs;
long emaskagg;		/* the emask aggregate */

struct index_str *free_ptr;
int nodes_left;
jmp_buf alloc_overflow;

/* alloc.c */

pword prs_area;
pword areap;

/* compile.c */
int npv;				/* number of permanent variables */
int ngoals;			/* number of goals               */
int nvars;			/* number of variables           */
int goalnumber;		/* which goal we're working on,  *
				 			 * 0 represents the head         */
int cnargs;		    /* number of arguments in the current goal */

int firstgoalnumber;
			/* usually 1, but not when there are cut(s) preceding
			 * the first real goal
			 */

int nargsmatched;
			/* number of arguments matched in head so far.
			 */

int to_do[TODOSIZE];

		/*-----------------------------------------------*
		 * the "to_do" stack.  We use this stack when
		 * compiling structure to keep track of register
		 * numbers allocated by previous recursive calls
		 * which built other structure or need other
		 * structure built (it differs between the head
		 * and the body).  Head structure is built top-
		 * down and structure occurring in the body is
		 * built bottom-up
		 *-----------------------------------------------*/

int to_do_top;
		/*----------------------------------------------*
		 * points to the next available location on the
		 * "to do" stack
		 *----------------------------------------------*/

pword model[MODELSIZE];

		/*----------------------------------------------*
		 * stack model of argument blocks and environment.
		 * The following variables are indices into this
		 * block.
		 *----------------------------------------------*/

int model_Adst;		/* index of destination arguments */
int model_Adst2;		/* index of destination arguments which might
				 			 * overlap the source arguments */

int model_Eend;		/* end of safe part of E */
int model_SPstart;

		/*----------------------------------------------*
		 * Place where SP starts out at before the compilation
		 *      of a goal allocates some temps and moves it.
		 *      This value is used for the allocation of
		 *      temporaries.  Between model_SP and model_SPstart
		 *      are the potentially free temporaries.
		 *----------------------------------------------*/

	/*==================================*
	 * Compiler directive variables
	 *==================================*/

int cd_cutpt;	/* Variable number to put cutpt in; -1 if not needed */
int cd_cmod;		/* Clause module									 */
int cd_gmod;		/* Goal module; i.e, module to place next goal in	 */

int model_SP;
int model_E;

/* from wintcode */

dbprot_t state;

/* from varproc */
varinf vtbl[VTBLSIZE + 2];	/* variable table               */

int   call_env_sizes[MAXGLS];

/* from parser */

jmp_buf prs_erc[30];	/* buffers for error recovery   */
int   prs_erc_index; /* index into the above         */

/*
 * vtable is an array of UIA's which represent the variable names
 *
 */

pword vtable[240];	/* variable name table          */

int   nxtvar;		/* next free space in var table */


/*
 * The parser uses two stacks: an operator stack represented by
 * pst_rator and ps_rator; and an operand stack represented by
 * pst_rand and ps_rand.  The ps_ arguments are the actual allocated
 * storage.  The pst_ arguments are pointers to the tops.
 *
 * The parser stacks will grow from low to high memory with the tops of stacks
 * pointing one beyond where the top element resides.
 */

struct rator *pst_rator;	/* parser stack top -- rator */
pword *pst_rand;		/* parser stack top -- rand */

struct rator ps_rator[PSTKSZ];
pword ps_rand[PSTKSZ];

/*
 * errcount is the number of errors encountered so far in the parse.  Care
 *      must be taken to preserve this value in recursive consults.
 */

int   perrcount;

/* mapsym */

#define BBSZ 4			/* number of buckets in a bucket block */
#define HASHSZ 512		/* number of entries in hash table */
#define HASHMASK (HASHSZ-1)
			/* mask to use for HASHSZ a power of two */
#define NIBB 4			/* number of initial bucket blocks to
				 * allocate
				 */

struct bucket_block {
    long  key_ptr[BBSZ];
    unsigned short map_tag[BBSZ];
};

struct map_descr {
    struct map_descr *prev;
    long *mem_allocated;
    long  next_mapval;
    struct bucket_block *next_bb;
    struct bucket_block *last_bb;
    long  key_ptr[HASHSZ];
    unsigned short map_tag[HASHSZ];
    struct bucket_block initial_bbs[NIBB];
}    *mapstack;


/* from load_file */

/* from loadfile */

long init_pos;
long init_obp_nrecs;

FILE *obp_fp;
long obp_nrecs;

struct obp_stack_rec {
    int   nrecs;
    int   makeobp;
    FILE *fp;
} obp_stack[100];
int obp_stack_top;

/* from icode1 */

#define MAXCALLS    512
#define ICBUFSIZE 32768

Code *dstart;	/* Where to go for a determinate start of
				 * clause
				 */

int imakeobp;

/*
 * data structure for storing gc call information
 */

struct {
    Code *patchaddr;
    long  argenvsize;
    long  argmask;
} callinfo[MAXCALLS];

int callidx;		/* call index                   */

/* 1 if the first argument has been processed.  0 otherwise. */

int   firstargprocessed;

/*
 * Pointer to the determinate code for the first argument in which
 * the dereference code is skipped because A1 has the dereferenced
 * first argument.
 */

Code *firstargptr;

int   capturemode;		/* not used in Port code */

/*
 * The icode buffer and buffer pointer
 */

Code  icode_buf[ICBUFSIZE];
/*  Now declared in icodegen.h; 
  Code *ic_ptr;  */

ic_uptr_type ic_uptr;

/*
 * Previous instruction pointer for doing minimial peephole optimization
 */

Code *ic_pptr;

/*---------------------------------------------------------------------------
 * The "Portable" version has no temporary registers.  Therefore, the compiler
 * will allocate stack when it needs a temporary.  The problem with this is
 * that we are inundated with instructions which decrement SP.  Rather than
 * changing SP at all, we will simply record the offset to add to SP to get
 * to where it really should be and update SP with this value before a call
 * or execute instruction.
 *--------------------------------------------------------------------------*/

long sp_disp;


/* from expand */

long evtable[4096];
int vtp;

/* from foreign */
PWord PI_modid;

/* from icode1 */
Code *deallocate2patch;
Code *deallocate3patch;
PWord *size2patch;
int my_isdetflag;

PWord proc_id;
int proc_arity;
PWord firstargkey;


/* from wdist */
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

struct prolog_engine_struct {
/* Prolog logic */
	
	MUTEX db_write_mutex;
	prolog_database *db;

	long pegvnum;
	
/* Prolog Control */
	
	/* Registers */
	
	register_set reg;

	PWord *gr_SPB, *gr_HB, *gr_TR;
	
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
	
	/* from arith.c */
	jmp_buf is_error;
	PWord error_functor; int error_arity;

/* from bsio */

long nt_tokstart;
long nt_tokend;

/* from gc */

unsigned long *marks;
long *heap_low;
long *heap_high;
long nmarked;

/* from winter.c */

PWord wm_normal;
PWord wm_safety;
PWord wm_trigger;
PWord wm_interrupt_caught;
PWord wm_in_Prolog;
PWord wm_spying;

/* from wintcode.c */
PWord wm_aborted;

};

typedef struct prolog_engine_struct prolog_engine;

void init_prolog_globals(void);

prolog_engine *create_prolog_engine(size_t stack_size, size_t heap_size);
void delete_prolog_engine(prolog_engine *pe);

void init_prolog_engine(prolog_engine *pe, size_t stack_size, size_t heap_size);
void shutdown_prolog_engine(prolog_engine *pe);

void push_register_stack(prolog_engine *pe);
void pop_register_stack(prolog_engine *pe);

int prolog_engine_invariant(const prolog_engine *pe);

int size_prolog_engine(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size);

int pbi_resize_memory(prolog_engine *hpe);

/* OS memory support */
void protect_stack(prolog_engine *pe);
void unprotect_stack(prolog_engine *pe);

void os_init_prolog_globals(void);
void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size);
void free_prolog_memory(prolog_engine *pe);
void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size);

//#define PE	prolog_engine *hpe
/* Logic Compatibility macros */
#define als_mem ((long *)hpe->db->als_mem)

#define builtins_initialized (hpe->db->builtins_initialized)
#define pegvnum (hpe->pegvnum)
#define top_module (hpe->db->top_module)
#define module_stack (hpe->db->module_stack)
#define top_clausegroup (hpe->db->top_clausegroup)
#define clausegroup_stack (hpe->db->clausegroup_stack)
#define module_table (hpe->db->module_table)
#define use_table (hpe->db->use_table)
#define nmods (hpe->db->nmods)
#define nuses (hpe->db->nuses)
#define default_uses (hpe->db->default_uses)
#define default_usemax (hpe->db->default_usemax)
#define default_procs (hpe->db->default_procs)
#define default_procmax (hpe->db->default_procmax)
#define ts_allocidx (hpe->db->ts_allocidx)
#define ts_allocated (hpe->db->ts_allocated)
#define ts_prime (hpe->db->ts_prime)
#define ts_cutoff (hpe->db->ts_cutoff)
#define ts_next (hpe->db->ts_next)
#define toktable (hpe->db->toktable)
#define hashtable (hpe->db->hashtable)
#define strings (hpe->db->strings)
#define strings_last (hpe->db->strings_last)
#define strings_next (hpe->db->strings_next)
#define char_to_tok_map (hpe->db->char_to_tok_map)
#define w_freeptr (hpe->db->w_freeptr)
#define w_totspace (hpe->db->w_totspace)
#define w_totspace (hpe->db->w_totspace)
#define w_timestamp (hpe->db->w_timestamp)
#define w_reconstamp (hpe->db->w_reconstamp)
#define w_tofreelist (hpe->db->w_tofreelist)
#define w_tofreesize (hpe->db->w_tofreesize)
#define w_nametable (hpe->db->db_w_nametable)
#define codeblock_list (hpe->db->codeblock_list)
#define ane_entbase (hpe->db->ane_entbase)
#define ane_entptr (hpe->db->ane_entptr)

#define aib_clause_addr (hpe->db->aib_clause_addr)

#define ibuf_ptr (hpe->db->ibuf_ptr)

#define inargs (hpe->db->inargs)
#define emaskagg (hpe->db->emaskagg)

#define free_ptr (hpe->db->free_ptr)
#define nodes_left (hpe->db->nodes_left)
#define alloc_overflow (hpe->db->alloc_overflow)

#define prs_area (hpe->db->prs_area)
#define areap (hpe->db->areap)

#define npv (hpe->db->npv)
#define ngoals (hpe->db->ngoals)
#define nvars (hpe->db->nvars)
#define goalnumber (hpe->db->goalnumber)
#define cnargs (hpe->db->cnargs)
#define firstgoalnumber (hpe->db->firstgoalnumber)
#define nargsmatched (hpe->db->nargsmatched)
#define to_do (hpe->db->to_do)
#define to_do_top (hpe->db->to_do_top)
#define model (hpe->db->model)
#define model_Adst (hpe->db->model_Adst)
#define model_Adst2 (hpe->db->model_Adst2)
#define model_Eend (hpe->db->model_Eend)
#define model_SPstart (hpe->db->model_SPstart)
#define cd_cutpt (hpe->db->cd_cutpt)
#define cd_cmod (hpe->db->cd_cmod)
#define cd_gmod (hpe->db->cd_gmod)
#define model_SP (hpe->db->model_SP)
#define model_E (hpe->db->model_E)

#define vtbl (hpe->db->vtbl)
#define call_env_sizes (hpe->db->call_env_sizes)

#define prs_erc (hpe->db->prs_erc)
#define prs_erc_index (hpe->db->prs_erc_index)
#define vtable (hpe->db->vtable)
#define nxtvar (hpe->db->nxtvar)
#define pst_rator (hpe->db->pst_rator)
#define pst_rand (hpe->db->pst_rand)
#define ps_rator (hpe->db->ps_rator)
#define ps_rand (hpe->db->ps_rand)
#define perrcount (hpe->db->perrcount)

#define mapstack (hpe->db->mapstack)


#define init_pos (hpe->db->init_pos)
#define init_obp_nrecs (hpe->db->init_obp_nrecs)
#define obp_fp (hpe->db->obp_fp)
#define obp_nrecs (hpe->db->obp_nrecs)
#define obp_stack (hpe->db->obp_stack)
#define obp_stack_top (hpe->db->obp_stack_top)

#define dstart (hpe->db->dstart)
#define imakeobp (hpe->db->imakeobp)
#define callinfo (hpe->db->callinfo)
#define callidx (hpe->db->callidx)
#define firstargprocessed (hpe->db->firstargprocessed)
#define firstargptr (hpe->db->firstargptr)
#define capturemode (hpe->db->capturemode)
#define icode_buf (hpe->db->icode_buf)
#define ic_uptr (hpe->db->ic_uptr)
#define ic_pptr (hpe->db->ic_pptr)
#define sp_disp (hpe->db->sp_disp)

#define evtable (hpe->db->evtable)
#define vtp (hpe->db->vtp)

#define deallocate2patch (hpe->db->deallocate2patch)
#define deallocate3patch (hpe->db->deallocate3patch)
#define size2patch (hpe->db->size2patch)
#define my_isdetflag (hpe->db->my_isdetflag)

#define proc_id (hpe->db->proc_id)
#define proc_arity (hpe->db->proc_arity)
#define firstargkey (hpe->db->firstargkey)

/* WAM global compatability macros */

#define wm_fail (hpe->db->wm_fail)
#define wm_trust_fail (hpe->db->wm_trust_fail)
#define wm_return_success (hpe->db->wm_return_success)
#define wm_special (hpe->db->wm_special)
#define wm_rungoal_code (hpe->db->wm_rungoal_code)
#define wm_panic (hpe->db->wm_panic)
#define wm_overcode (hpe->db->wm_overcode)
#define wm_cutaddr (hpe->db->wm_cutaddr)
#define rungoal_modpatch (hpe->db->rungoal_modpatch)
#define rungoal_goalpatch (hpe->db->rungoal_goalpatch)

/* Control Compatibility macros */

#define wm_heapbase ((PWord *)hpe->heap_base)
#define wm_stackbot ((PWord *)hpe->stack_max)
#define wm_trailbase ((PWord *)hpe->trail_base)

#define wm_gvbase	((PWord *)hpe->globals_base)
#define wm_gvfreelist ((PWord *)hpe->globals_free_list.ptr)
#define gv_setcnt (hpe->globals_set_count)

#define wm_B	((PWord *)hpe->reg.B.ptr)
#define wm_HB	((PWord *)hpe->reg.HB.ptr)
#define wm_SPB	((PWord *)hpe->reg.SPB.ptr)
#define wm_E	((PWord *)hpe->reg.E.ptr)
#define wm_TR	((PWord *)hpe->reg.TR.ptr)
#define wm_H	((PWord *)hpe->reg.H.ptr)
#define wm_SP	((PWord *)hpe->reg.SP.ptr)
#define wm_FAIL ((PWord *)hpe->reg.rFAIL.code)

#define gr_SPB	(hpe->gr_SPB)
#define gr_HB	(hpe->gr_HB)
#define gr_TR	(hpe->gr_TR)

#define is_error (hpe->is_error)
#define error_functor (hpe->error_functor)
#define error_arity (hpe->error_arity)

#define nt_tokstart (hpe->nt_tokstart)
#define nt_tokend (hpe->nt_tokend)

#define marks (hpe->marks)
#define heap_low (hpe->heap_low)
#define heap_high (hpe->heap_high)
#define nmarked (hpe->nmarked)

#define wm_normal (hpe->wm_normal)
#define wm_safety (hpe->wm_safety)
#define wm_trigger (hpe->wm_trigger)
#define wm_interrupt_caught (hpe->wm_interrupt_caught)
#define wm_in_Prolog (hpe->wm_in_Prolog)
#define wm_spying (hpe->wm_spying)

#define wm_aborted (hpe->wm_aborted)

#endif
