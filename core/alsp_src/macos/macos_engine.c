#include "defs.h"
#include "engine.h"
#include "cexception.h"

#include <MacMemory.h>

#include <FullPath.h>
#include <FileCopy.h>

#define THROW_MACOS_ERROR(n) Failure((n), 0)

static Ptr safe_NewPtr(Size byteCount)
{
	Ptr p;
	
	p = NewPtr(byteCount);
	if (MemError() != noErr)
		THROW_MACOS_ERROR(MemError());
	
	return p;
}

static void safe_DisposePtr(Ptr p)
{
	DisposePtr(p);
	if (MemError() != noErr)
		THROW_MACOS_ERROR(MemError());
}

void os_init_prolog_globals(void)
{
}

static void resource_store(void *r, void *data, long length, long offset)
{
	SetResourceSize(r, offset+length);
	printf("reserror: %d\n", ResError());	
	WritePartialResource(r, offset, data, length);
	printf("reserror: %d\n", ResError());
}

static void resource_load(void *r, void *data, long length, long offset)
{
	ReadPartialResource(r, offset, data, length);
}

static void store_db_resource(prolog_database *db, const FSSpec *spec)
{
	short ref;
	Handle r;
	
	ref = FSpOpenResFile(spec, fsRdWrPerm);
	printf("reserror: %d\n", ResError());
	
	SetResLoad(false);
	r = Get1Resource('ALSS', 128);
	printf("reserror: %d\n", ResError());
	SetResLoad(true);
	
	if (!r) {
		r = NewHandleClear(10000);
		AddResource(r, 'ALSS', 128, "\pALS Prolog State");
	printf("reserror: %d\n", ResError());
		WriteResource(r);
	printf("reserror: %d\n", ResError());
		ReleaseResource(r);
	printf("reserror: %d\n", ResError());
	SetResLoad(false);
	r = Get1Resource('ALSS', 128);
	printf("reserror: %d\n", ResError());
	SetResLoad(true);
	}
	
	store_db(db, r, 0, resource_store);
	CloseResFile(ref);
	printf("reserror: %d\n", ResError());
}

static void load_db_resource(prolog_database *db, const FSSpec *spec)
{
	short ref;
	Handle r;
	
	ref = FSpOpenResFile(spec, fsRdPerm);
	
	SetResLoad(false);
	r = Get1Resource('ALSS', 128);
	SetResLoad(true);
	
	load_db(db, r, 0, resource_load);
	
	CloseResFile(ref);
}

void os_store_db(prolog_database *db, const char *file_name, long offset)
{
	FSSpec spec;
	Str255 pname;
	c2pstrcpy(pname, file_name);
	FSMakeFSSpec(0, 0, pname, &spec);
	store_db_resource(db, &spec);
}

void os_load_db(prolog_database *db, const char *file_name, long offset)
{
	FSSpec spec;
	Str255 pname;
	c2pstrcpy(pname, file_name);
	FSMakeFSSpec(0, 0, pname, &spec);
	load_db_resource(db, &spec);
}

long ss_image_offset(const char *imagepath)
{
	Handle r;
	Str255 pname;
	FSSpec spec;
	short f;
	c2pstrcpy(pname, imagepath);
	FSMakeFSSpec(0, 0, pname, &spec);
	f = FSpOpenResFile(&spec, fsRdPerm);
	if (f) {
	SetResLoad(false);
	r = Get1Resource('ALSS', 128);
	SetResLoad(true);
	} else r = NULL;
	CloseResFile(f);
	return (r != NULL);
}

static OSErr DuplicateThisApplication(ConstStr255Param newAppName)
{
    OSErr err;
    Str255 AppName;
    FSSpec AppSpec, NewAppSpec, DirSpec;
    
    if (MPW_Tool) {
    	c2pstrcpy(AppName, executable_path);
    	
    	err = FSMakeFSSpec(0, 0, AppName, &AppSpec);
    	if (err != noErr) return err;
    } else {
	ProcessSerialNumber PSN;
	ProcessInfoRec info;
	
	/* Get the FSSpec for this application. */    
	PSN.highLongOfPSN = 0;
	PSN.lowLongOfPSN = kCurrentProcess;
	
	info.processInfoLength = sizeof(ProcessInfoRec);
	info.processName = AppName;
	info.processAppSpec = &AppSpec;
	
	err = GetProcessInformation(&PSN, &info);
	if (err != noErr) return err;
    }

    /* Create a FSSpec for the new app and destination directory. */
    err = FSMakeFSSpec(0, 0, newAppName, &NewAppSpec);
    if (err != noErr && err != fnfErr) return err;
    
    if (err == noErr) {
    	err = FSpDelete(&NewAppSpec);
    	if (err != noErr) return err;
    }
    
    err = FSMakeFSSpec(NewAppSpec.vRefNum, NewAppSpec.parID, "\p", &DirSpec);
    if (err != noErr && err != fnfErr) return err;

    return FSpFileCopy(&AppSpec, &DirSpec, (StringPtr) newAppName, NULL, 0, 1);
}

int ss_save_image_with_state(const char * new_image_name)
{
	Str255 pnew_image_name;
	
	c2pstrcpy(pnew_image_name, new_image_name);
	
	DuplicateThisApplication(pnew_image_name);
   
	return ss_attach_state_to_file(new_image_name);
}

int ss_attach_state_to_file(const char *image_name)
{
	os_store_db(&current_engine.db, image_name, 0);
	return 1;
}

void alloc_prolog_memory(prolog_engine *pe, size_t stack_size, size_t heap_size)
{

	pe->stack_size = stack_size;
	pe->heap_size = heap_size;
	pe->memory_base = (PCell *)safe_NewPtr((heap_size + stack_size)*sizeof(PCell) + heap_size/8);
	pe->mark_area = pe->memory_base + heap_size+stack_size;

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
	safe_DisposePtr((Ptr)pe->memory_base);
}

static void *resize_ptr(Ptr ptr, size_t old_size, size_t new_size)
{
	Ptr new_ptr;

	SetPtrSize(ptr, new_size);
	if (MemError() != noErr) {
		new_ptr = safe_NewPtr(new_size);
		BlockMove(ptr, new_ptr, old_size);
		DisposePtr(ptr);
		ptr = new_ptr;
	}

	return ptr;
}

void realloc_prolog_memory(prolog_engine *pe, size_t new_stack_size, size_t new_heap_size)
{
	pe->memory_base = resize_ptr((Ptr)pe->memory_base,
		(pe->heap_size + pe->stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell) + pe->heap_size/8,
		(new_heap_size + new_stack_size + pe->globals_base - pe->trail_base)*sizeof(PCell) + new_heap_size/8);

	pe->mark_area = pe->memory_base + new_stack_size+new_heap_size+(pe->globals_base - pe->trail_base);
}
