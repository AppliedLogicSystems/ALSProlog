#include "defs.h"

#include "dump_wam.h"

void get_wam_task_state(wam_task_state *s)
{
	s->P = 0; /* Currently not saved outside of run_wam!?!?!? */
	s->B = (PWord) wm_B;
	s->HB = (PWord) wm_HB;
	s->SPB = (PWord) wm_SPB;
	s->E = (PWord) wm_E;
	s->TR = (PWord) wm_TR;
	s->H = (PWord) wm_H;
	s->SP = (PWord) wm_SP;
	s->F = (PWord) wm_FAIL;
	
	s->heap_and_stack_base = wm_heapbase;
	s->choice_trail_base = wm_trailbase;
	s->stack_limit = wm_stackbot;
	s->global_end = wm_gvbase;
}

#if 0
static void set_wam_task_state(const wam_task_state *s)
{

}

static void duplicate_wam_task_state(const wam_task_state *cs, wam_task_state *s)
{

}
#endif

static void dump_fail(const char *s)
{
	fprintf(stderr, "Dump WAM Error: %s\n", s);
	exit(1);
}

size_t name_space_size;
long *name_space;
long next_name;

static void name_heap_loc(const wam_task_state *s, PWord *c)
{
	size_t i;
	i = ((size_t)(c) - (size_t)(s->stack_limit))/4;
	/*//printf("c: %p i: %u stacklimit: %p\n", c, i, s->stack_limit);*/
	if (i < 0 || i > name_space_size) dump_fail("Heap range error");
	if (name_space[i] == -1) name_space[i] = next_name++;
}

static long get_heap_loc_name(const wam_task_state *s, PWord *c)
{
	size_t i;
	i = ((size_t)(c) - (size_t)(s->stack_limit))/4;
	if (i < 0 || i > name_space_size) dump_fail("Heap range error");
	if (name_space[i] == -1) dump_fail("unamed object");
	return name_space[i];
}

static int named_heap_loc(const wam_task_state *s, PWord *c)
{
	size_t i;
	i = ((size_t)(c) - (size_t)(s->stack_limit))/4;
	if (i < 0 || i > name_space_size) dump_fail("Heap range error");
	return name_space[i] != -1;	
}

#define MTP_REF MTP_UNBOUND
#define PWMEM(x)	(*(PWord *)(x))

static void name(const wam_task_state *s, PWord *c)
{
	PWord *addr;
	int i, arity;
	
	switch (MTP_TAG(*c)) {
	case MTP_REF:
		if (!named_heap_loc(s, c)) {
			name_heap_loc(s, c);
			if ((PWord)c != *c) name(s, (PWord *)*c);
		}
		break;
	case MTP_STRUCT:
		addr = MSTRUCTADDR(*c);
		if (!named_heap_loc(s, addr)) {
			name_heap_loc(s, addr);
			arity = MFUNCTOR_ARITY(MFUNCTOR(addr));
			for (i = 0; i < arity; i++) {
				name(s, addr+i);
			}
		}
		break;
	case MTP_LIST:
		addr = MLISTADDR(*c);
		if (!named_heap_loc(s, addr)) {
			name_heap_loc(s, addr);
			name(s, addr);
			name(s, addr+1);
		}
		break;
	case MTP_CONST:
		switch (MTP_CONSTTAG(*c)) {
		case MTP_INT:
			break;
		case MTP_SYM:
			break;
		case MTP_FENCE:
			break;
		case MTP_UIA:
			name_heap_loc(s, (PWord *)(MUIA(*c) + (size_t)s->heap_and_stack_base));
			break;
		};
		break;
	}
}


static void print_obj(const wam_task_state *s, PWord *c)
{
	switch (MTP_TAG(*c)) {
	case MTP_REF:
		if ((PWord)c == *c) printf("unbound var named %ld", get_heap_loc_name(s, c));
		else {
			printf("var named %ld, points to ", get_heap_loc_name(s, c));
			print_obj(s, (PWord *)PWMEM(c));
		}
		break;
	case MTP_STRUCT:
		printf("struct named %ld at %p", get_heap_loc_name(s, MSTRUCTADDR(*c)), MSTRUCTADDR(*c));
		break;
	case MTP_LIST:
		printf("list named %ld at %p", get_heap_loc_name(s, MLISTADDR(*c)), MLISTADDR(*c));
		break;
	case MTP_CONST:
		switch (MTP_CONSTTAG(*c)) {
		case MTP_INT:
			printf("integer %ld", MINTEGER(*c));
			break;
		case MTP_SYM:
			printf("symbol");
			break;
		case MTP_FENCE:
			printf("fence");
			break;
		case MTP_UIA:
			printf("UIA named %ld at %p",
					get_heap_loc_name(s, (PWord *)(MUIA(*c) + (size_t)s->heap_and_stack_base)),
					(PWord *)(MUIA(*c) + (size_t)s->heap_and_stack_base));
			break;
		}
	}
}

void dump_wam_task_state(const wam_task_state *s)
{
	PWord e, b, *g, sp;
	int i;
	int count;
	Code *code;
	
	/* Allocate name space for heap objects */
	name_space_size = ((size_t)(s->global_end) - (size_t)(s->stack_limit))/4;
	name_space = (long *)malloc(name_space_size * sizeof(long));
	if (name_space == NULL) dump_fail("out of memory");
	for (i = 0; i < name_space_size; i++) name_space[i] = -1;
	next_name = 0;
	
	printf("ALS-WAM State\n");
	
	printf("Registers:\n");
	printf("P   %8lX\n", s->P);
	printf("TR  %8lX\n", s->TR);
	printf("H   %8lX\n", s->H);
	printf("F   %8lX\n", s->F);
	printf("SPB %8lX\n", s->SPB);
	printf("HB  %8lX\n", s->HB);
	printf("B   %8lX\n", s->B);
	printf("E   %8lX\n", s->E);
	printf("SP  %8lX\n", s->SP);

	/* dump stack */
	printf("\nStack\n");
	for (sp = s->SP, count = 0; sp < (PWord)s->heap_and_stack_base && count < 100; sp += 4, count++) {
		printf("%8lX: %8lX\n", sp, PWMEM(sp));
	}

	/* dump choice points */
	printf("\nChoice Point Stack\n");
	for (count = 0, b = s->B; count < 15 && b; count++, b = PWMEM(b+12)) {
		printf("%8lX: %8lX %8lX %8lX %8lX\n", b, PWMEM(b), PWMEM(b+4), PWMEM(b+8), PWMEM(b+12));
	}

	/* dump environment stack */
	printf("\nEnvironment Stack\n");
	
	{
	PWord start_e, end_e;
	count = 0;
	for (b = s->B, start_e = s->E,end_e = PWMEM(b+8) & ~1; b;
		 start_e = end_e, b =  PWMEM(b+12), end_e = PWMEM(b+8) & ~1) { 
		for (e = start_e, code = 0; e < end_e; e = PWMEM(e), code = (Code *)PWMEM(e+4)) {
			PWord *arg_end, *arg;
			count++;
			if (count < 7) {
				Code *r;
				printf ("%8lX: ENV %8lX %8lX\n", e, PWMEM(e), PWMEM(e+4));
				r = (Code *)PWMEM(e+4);
				printf ("Code Mask %lX nargs: %ld\n",
					     *((long *) (r + GCMAGICVal(r)) + 1),
					     (*(long *) (r + GCMAGICVal(r))) & 0xffff);
			}
			arg_end = (PWord *)(PWMEM(e) < end_e ? PWMEM(e) : end_e);
			if (code) {
				long mask;
				int nargs, cutoff;
				if (*code != GCMAGIC) dump_fail("no gc magic");
				mask = *((long *) (code + GCMAGICVal(code)) + 1);
				nargs = (*(long *) (code + GCMAGICVal(code))) & 0xffff;
				cutoff = nargs - 32;
				for (arg = (PWord *)(e+8); nargs--; arg++, mask >>= 1) {
					if ((mask & 1) || nargs < cutoff) {
						if (count < 7) printf("%8p: ARG %8lX", arg, *arg);
						if (count < 7) name(s, arg);
						if (count < 7) {
							print_obj(s, arg);
							printf("\n");
						}
					}
				}
				
				for (; arg < arg_end; arg++) {
					if (count < 7) printf("%8p: ARG %8lX", arg, *arg);
					if (count < 7) name(s, arg);
					if (count < 7) {
						print_obj(s, arg);
						printf("\n");
					}				
				}
			} else {
				for (arg = (PWord *)(e+8); arg < arg_end; arg++) {
					if (count < 7) printf("%8p: ARG %8lX", arg, *arg);
					if (count < 7) name(s, arg);
					if (count < 7) {
						print_obj(s, arg);
						printf("\n");
					}
				}
			}
		}
	}
	}


	/* dump global variables */
	//printf("\nGlobal Variables\n");
	for (g = s->choice_trail_base; g < s->global_end; g++) {
		//printf("%8X: %8X ", g, *g);
		name(s, g);
		//print_obj(s, g);
		//printf("\n");
	}
	
	free(name_space);
}
