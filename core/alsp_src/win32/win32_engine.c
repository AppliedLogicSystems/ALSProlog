#include "defs.h"
#include "engine.h"


#define THROW_WIN32_ERROR(n) \
{ fprintf(stderr, "error: %d\n", (n)); exit(EXIT_ERROR); }

#define PAGE_SIZE page_size()
#define PAGE_ROUND(n) (n + PAGE_SIZE - n % PAGE_SIZE)

#ifdef __MWERKS__
/* Metrowerks does not compile the Microsoft definition of
   SYSTEM_INFO.  The first element is an anonymous union. */
typedef struct _FIXED_SYSTEM_INFO {
    DWORD dwOemId;
    DWORD dwPageSize;
    LPVOID lpMinimumApplicationAddress;
    LPVOID lpMaximumApplicationAddress;
    DWORD dwActiveProcessorMask;
    DWORD dwNumberOfProcessors;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD wProcessorLevel;
    WORD wProcessorRevision;
} FIXED_SYSTEM_INFO;
#endif

static DWORD page_size(void)
{
	static DWORD size = 0;
	
	if (!size) {
		FIXED_SYSTEM_INFO info;
		GetSystemInfo((LPSYSTEM_INFO)&info);
		size = info.dwPageSize;
	}
	return size;
}

static LPVOID safe_VirtualAlloc(LPVOID lpAddress,	DWORD dwSize,
    				DWORD flAllocationType, DWORD flProtect)
{
	LPVOID result;
	
	result = VirtualAlloc(lpAddress, dwSize, flAllocationType, flProtect);
	if (result == NULL) THROW_WIN32_ERROR(GetLastError());
	
	return result;
}

static void safe_VirtualFree(LPVOID lpAddress, DWORD dwSize,
							 DWORD dwFreeType)
{
	BOOL success;
	
	success = VirtualFree(lpAddress, dwSize, dwFreeType);
	if (!success) THROW_WIN32_ERROR(GetLastError());
}

static void safe_VirtualProtect(LPVOID lpAddress, DWORD dwSize,
						  DWORD flNewProtect, PDWORD lpflOldProtect)
{
	BOOL success;
	
	success = VirtualProtect(lpAddress, dwSize, flNewProtect, lpflOldProtect);
	if (!success) THROW_WIN32_ERROR(GetLastError());
}

void protect_stack(prolog_engine *pe)
{
	DWORD old;
	safe_VirtualProtect(pe->memory_base, 1, PAGE_NOACCESS, &old);
	pe->stack_protected = 1;
}

void unprotect_stack(prolog_engine *pe)
{
	DWORD old;
	safe_VirtualProtect(pe->memory_base, 1, PAGE_READWRITE, &old);
	pe->stack_protected = 0;
}

static LONG WINAPI exception_filter(struct _EXCEPTION_POINTERS *lpexpExceptionInfo)
{
    if (lpexpExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION
        && current_engine.stack_protected) {
	unprotect_stack(&current_engine);
	signal_handler(ALSSIG_STACK_OVERFLOW);
	return EXCEPTION_CONTINUE_EXECUTION;
    } else {
    	return EXCEPTION_CONTINUE_SEARCH;
    }	    
}

void os_init_prolog_globals(void)
{
    SetUnhandledExceptionFilter(exception_filter);
}

void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size)
{

	pe->stack_size = stack_size;
	pe->heap_size = heap_size;
	pe->memory_base = (PCell *)safe_VirtualAlloc(NULL, (heap_size + stack_size)*sizeof(PCell), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
	pe->mark_area = safe_VirtualAlloc(NULL, heap_size/8, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

	pe->stack_max = pe->memory_base;
	pe->stack_base = pe->heap_base = pe->stack_max + stack_size;
	pe->trail_base = pe->globals_top = pe->globals_base = pe->heap_base + heap_size - 1;
	pe->globals_free_list.uint = MMK_INT(-1);

	protect_stack(pe);
}


void free_prolog_memory(prolog_engine *pe)
{
	safe_VirtualFree(pe->memory_base, 0, MEM_RELEASE);
	safe_VirtualFree(pe->mark_area, 0, MEM_RELEASE);
}



static void *resize_virtual(void *addr, size_t old_size, size_t new_size)
{
	void *new_addr;
	
	if (new_size > old_size) {
		new_addr = safe_VirtualAlloc(NULL, new_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
		memcpy(new_addr, addr, old_size);
		safe_VirtualFree(addr, old_size, MEM_DECOMMIT);
		addr = new_addr;
	} else if (new_size < old_size) {
		if (PAGE_ROUND(old_size) > PAGE_ROUND(new_size)) {
			safe_VirtualFree((char *)addr + PAGE_ROUND(new_size),
				PAGE_ROUND(old_size)-PAGE_ROUND(new_size), MEM_DECOMMIT);
		}
	}
	
	return addr;
}

void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	unprotect_stack(pe);

	pe->memory_base = resize_virtual(pe->memory_base,
		(pe->heap_size + pe->stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell),
		(new_heap_size + new_stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell));

	protect_stack(pe);

	pe->mark_area = resize_virtual(pe->mark_area, pe->heap_size/8, new_heap_size/8);
}

void os_store_db(prolog_database *db, const char *file_name, long offset)
{
	store_db_file(db, file_name, offset);
}

void os_load_db(prolog_database *db, const char *file_name, long offset)
{
	load_db_file(db, file_name, offset);
}
