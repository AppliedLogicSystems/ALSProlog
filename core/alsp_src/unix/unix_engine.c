#include "defs.h"
#include "engine.h"

/* for open() */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* for mmap() */
#include <unistd.h>
#include <sys/mman.h>

/* for errno */
#include <errno.h>

/* for sigaction */
#include <signal.h>

#ifndef _SC_PAGE_SIZE
#define _SC_PAGE_SIZE _SC_PAGESIZE
#endif

#ifdef UNIX_SUNOS
#define PAGE_SIZE (getpagesize())
#else
#define PAGE_SIZE (sysconf(_SC_PAGE_SIZE))
#endif

#ifndef MAP_ANONYMOUS

static int dev_zero_fd = -1; /* Cached file descriptor for /dev/zero. */

#define MMAP(start, length, flags) ((dev_zero_fd < 0) ? \
			  (dev_zero_fd = open("/dev/zero", O_RDWR), \
			   mmap((start), (length), PROT_READ|PROT_WRITE, MAP_PRIVATE|(flags), dev_zero_fd, 0)) : \
			  mmap((start), (length), PROT_READ|PROT_WRITE, MAP_PRIVATE|(flags), dev_zero_fd, 0))

#else

#define MMAP(start, length, flags) \
     (mmap((start), (length), PROT_READ|PROT_WRITE, MAP_ANONYMOUS|(flags), -1, 0))

#endif

#define THROW_UNIX_ERROR(n) \
{ fprintf(stderr, "errno: %d (%s)\n", (n), strerror(n)); exit(EXIT_ERROR); }

static void *safe_mmap(void *start, size_t length, int flags)
{
	void *result;
	
	result = MMAP(start, length, flags);
	if (result == (void *)-1) THROW_UNIX_ERROR(errno);
	
	return result;
}

#ifdef UNIX_LINUX
static void *safe_mremap(void *addr, size_t old_size, size_t new_size,
			 unsigned long flags)
{
  void *result;
  
  result = mremap(addr, old_size, new_size, flags);
  if (result == (void *)-1) THROW_UNIX_ERROR(errno);

  return result;
}
#endif

static void safe_munmap(void *start, size_t length)
{
	int result;
	
	result = munmap(start, length);
	if (result == -1) THROW_UNIX_ERROR(errno);
}

static void safe_mprotect(void *addr, size_t len, int prot)
{
	int result;
	
	result = mprotect(addr, len, prot);
	if (result == -1) THROW_UNIX_ERROR(errno);
}

static void safe_sigaction(int sig, const struct sigaction *act,
							struct sigaction *oact)
{
	int result;
	
	result = sigaction(sig, act, oact);
	if (result == -1) THROW_UNIX_ERROR(errno);
}

void protect_stack(prolog_engine *pe)
{
	safe_mprotect(pe->memory_base, PAGE_SIZE, PROT_NONE);
	pe->stack_protected = 1;
}

void unprotect_stack(prolog_engine *pe)
{
	safe_mprotect(pe->memory_base, PAGE_SIZE, PROT_READ|PROT_WRITE);
	pe->stack_protected = 0;
}

/* Can I use safe functions in a interupt handler? */
static void sigsegv_handler(int signum)
{
	if (current_engine.stack_protected) {
		unprotect_stack(&current_engine);
		signal_handler(ALSSIG_STACK_OVERFLOW);
	} else {
		signal(signum, SIG_DFL);
	}
}

struct sigaction original_sigsegv_action;

void os_init_prolog_globals(void)
{
    struct sigaction action;

    action.sa_handler = sigsegv_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;
    safe_sigaction(SIGSEGV, &action, &original_sigsegv_action);
}

void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size)
{

	pe->stack_size = stack_size;
	pe->heap_size = heap_size;
	pe->memory_base = (PCell *)safe_mmap(0, (heap_size + stack_size)*sizeof(PCell) + heap_size/8, 0);
	pe->mark_area = pe->memory_base + heap_size + stack_size;
	pe->stack_max = pe->memory_base;
	pe->stack_base = pe->heap_base = pe->stack_max + stack_size;
	pe->trail_base = pe->globals_top = pe->globals_base = pe->heap_base + heap_size - 1;
	pe->globals_free_list.uint = MMK_INT(-1);

	protect_stack(pe);
}


void free_prolog_memory(prolog_engine *pe)
{
	safe_munmap((void *)pe->memory_base, (pe->heap_size + pe->stack_size)*sizeof(PCell) + pe->heap_size/8);
}

#define PAGE_ROUND(n) (n + PAGE_SIZE - n % PAGE_SIZE)

static void *resize_mmap(void *addr, size_t old_size, size_t new_size)
#ifdef UNIX_LINUX
{
  return safe_mremap(addr, old_size, new_size, MREMAP_MAYMOVE);
}
#else
{
	void *new_addr;
	
	if (new_size > old_size) {
		new_addr = safe_mmap(0, new_size, 0);
		memcpy(new_addr, addr, old_size);
		safe_munmap(addr, old_size);
		addr = new_addr;
	} else if (new_size < old_size) {
		if (PAGE_ROUND(old_size) - PAGE_ROUND(new_size) > 0) {
			safe_munmap((char *)addr + PAGE_ROUND(new_size),
				    PAGE_ROUND(old_size)-PAGE_ROUND(new_size));
		}
	}
	
	return addr;
}
#endif

void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	unprotect_stack(pe);

	pe->memory_base = resize_mmap(pe->memory_base,
		(pe->heap_size + pe->stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell) + pe->heap_size/8,
		(new_heap_size + new_stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell) + new_heap_size/8);
	pe->mark_area = pe->memory_base + new_heap_size + new_stack_size + (pe->globals_base - pe->trail_base);

	protect_stack(pe);
}

void os_store_db(prolog_database *db, const char *file_name, long offset)
{
	store_db_file(db, file_name, offset);
}

void os_load_db(prolog_database *db, const char *file_name, long offset)
{
	load_db_file(db, file_name, offset);
}
