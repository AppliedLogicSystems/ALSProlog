#include "defs.h"
#include "engine.h"

#include <stdlib.h>
#include <errno.h>

#define THROW_ANSI_ERROR(n) \
{ fprintf(stderr, "error: %d\n", (n)); exit(EXIT_ERROR); }

static void *safe_malloc(size_t size)
{
	void *p;
	
	p = malloc(size);
	if (!p)	THROW_ANSI_ERROR(errno);
	
	return p;
}

static void *safe_realloc(void *ptr, size_t size)
{
	void *p;
	
	p = realloc(ptr, size);
	if (!p)	THROW_ANSI_ERROR(errno);
	
	return p;
}

static void safe_free(void *ptr)
{
	free(ptr);
	if (errno) THROW_ANSI_ERROR(errno);
}

void os_init_prolog_globals(void)
{
}

void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size)
{

	pe->stack_size = stack_size;
	pe->heap_size = heap_size;
	pe->memory_base = (PCell *)safe_malloc((heap_size + stack_size)*sizeof(PCell));
	pe->mark_area = safe_malloc(heap_size/8);

	pe->stack_max = pe->memory_base;
	pe->stack_base = pe->heap_base = pe->stack_max + stack_size;
	pe->trail_base = pe->globals_top = pe->globals_base = pe->heap_base + heap_size - 1;
	pe->globals_free_list.uint = MMK_INT(-1);
}

void protect_stack(prolog_engine *pe)
{
#pragma unused(pe)
}

void free_prolog_memory(prolog_engine *pe)
{
	safe_free((void *)pe->memory_base);
	safe_free((void *)pe->mark_area);
}

void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	pe->memory_base = safe_realloc(pe->memory_base,
		(new_heap_size + new_stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell));

	pe->mark_area = safe_realloc(pe->mark_area, new_heap_size/8);
}
