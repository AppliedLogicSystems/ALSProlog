/*=================================================================*
 |			mem.c                
 |      Copyright (c) 1992-95 Applied Logic Systems, Inc.
 |
 |			-- memory allocation
 |
 | Author: Kevin A. Buettner
 | Creation: 12/17/92
 | 12/11/94 - C. Houpt -- Put sys/types.h and fcntl.h under ifdef control.
 | 				 -- Removed SIGBUS, SIGSEGV signal handling for Mac
 |				 -- Added HAVE_BRK for ifdef control of brk(), sbrk().
 |				 -- Ifdefed around 3 param open() call.
 |				 -- Misc char casts.
 |??/??/94 - C. Houpt -- NEED non-brk() implementation of ss_restore_state.
 |					    Maybe from the Windows version.
 *=================================================================*/
#include "defs.h"
#include "version.h"
#include "sig.h"

#ifdef UNIX_HPUX
#undef __harg
#define __harg int
#elif UNIX_SUNOS
#undef SIG_DFL
#define SIG_DFL (void (*)(int))0
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef MacOS
#include <Processes.h>
#include <Resources.h>
#include <Errors.h>
#include <FileCopy.h> /* From MoreFiles - from Macintosh Sample Code library. */
#endif

#ifdef MSWin32
#include <windows.h>
#include "fswin32.h"
#endif

#include <errno.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

/* MAX_PAGE_SIZE is the largest possible page size of the OS */
#define MAX_PAGE_SIZE 0x10000


/*
 * ALS Memory allocation	-- saved code states
 *
 * Defined below are functions which we use for memory allocation.  These
 * routines should be called only by functions whose state needs to be
 * saved. ALL pointers to space allocated by these routines should either
 * be self contained or contained in space obtained through other calls
 * to these functions.   There are times when the use of a global variable
 * is unavoidable.  
 *
 * I originally considered putting all global variables into a large
 * structure.  This is a fairly pleasing idea but for the fact that
 * it totally ruins modularity.  There are static variables in files like
 * symtab.c which manage the structure of symbol tables.  These files
 * are self contained.  The thought of opening these files up and making
 * the internal type information available to the rest of the program
 * is quite repulsive.
 *
 
 BZZZZZZ!  Wrong! Just because C doesn't allow proper data hidding doesn't mean
 you should make everything static globals.  Static globals just make threads,
 position independent code, etc harder to implement.
 
 
 * What we do instead is register our global variables with these routines.
 * This means that globals should be either longs or pointers (I'm assuming
 * that the space required by these two types are equivalent).  Then prior
 * to saving state, we can get the values needed to properly restore the
 * state at a later time.
 *
 * Space can only be allocated -- never freed.  This is not a problem as
 * all of our code which uses these facilities is designed to reuse the
 * space already allocated to it if a larger space is needed.  See symtab.c
 * for an excellent example.
 *
 * als_mem is the pointer to all of the memory necessary to create a saved
 * code state.  These means that all procedure table entries, clauses,
 * module information, symbol table information, and any variables which
 * reference these entities is stored here or is registered so that it
 * may be stored here when a save is performed.
 *
 */

#include "alsmem.h"

/*//static long *als_mem;*/

long *	alloc_big_block		( size_t, int );
static	long *	ss_malloc0		( size_t, int, int, long * );
#ifndef PURE_ANSI
static	void	ss_restore_state	( const char *, long );
#endif /* PURE_ANSI */
static	int	ss_saved_state_present	( void );

#define amheader (* (struct am_header *) als_mem)


#undef round
#define round(x,s) ((((long) (x) - 1) & ~(long)((s)-1)) + (s))

#define round_up(x,s) (((size_t)(x)) + (((size_t)(s)) - ((size_t)(x))%((size_t)(s)))%((size_t)(s)))
#define round_down(x,s) (((size_t)(x)) - (((size_t)(x))%((size_t)(s))))

#ifdef UNIX
#include <sys/mman.h>
#endif

long *
alloc_big_block(size, fe_num)
    size_t size;
    int fe_num;
{
    long *np;

    size = round(size, MAX_PAGE_SIZE);

	np = (long *)malloc(size);

	if (np == NULL)
	    fatal_error(fe_num, 0);    
    return np;
}

int
als_mem_init(file,offset)
    const char *file;
    long offset;
{

    if (ss_saved_state_present())
	return 1;	/* saved state loaded */

    else if (!file) {	/* no file specified */
	char *mp = malloc(AM_MALLOCSPACE);
	if (mp)
	    free(mp);
	else
	    fatal_error(FE_ALS_MEM_INIT,0);

	als_mem = (void *)alloc_big_block(AM_BLOCKSIZE, FE_ALS_MEM_INIT);

	amheader.nblocks = 1;
	amheader.totsize = AM_BLOCKSIZE;
	amheader.nglobals = 0;
	amheader.blocks[0].start = (void *)als_mem;
	amheader.blocks[0].asize = AM_BLOCKSIZE;

	amheader.freelist = (long *) 
		((char *) als_mem + sizeof (struct am_header));
	FB_SIZE(amheader.freelist) = 
		amheader.blocks[0].asize - sizeof (struct am_header);
	FB_NEXT(amheader.freelist) = (long *) 0;

	/* Set integrity information in case saved state is created */
	/*//amheader.integ_als_mem = &als_mem;*/
	amheader.integ_als_mem_init = als_mem_init;
	amheader.integ_w_unify = w_unify;
	(void)strncpy(amheader.integ_version_num, VERSION_STRING, sizeof(amheader.integ_version_num)-1);
	amheader.integ_version_num[sizeof(amheader.integ_version_num)-1] = '\0';
	(void)strncpy(amheader.integ_processor, ProcStr, sizeof(amheader.integ_processor)-1);
	amheader.integ_processor[sizeof(amheader.integ_processor)-1] = '\0';
	(void)strncpy(amheader.integ_minor_os, MinorOSStr, sizeof(amheader.integ_minor_os)-1);
	amheader.integ_minor_os[sizeof(amheader.integ_minor_os)-1] = '\0';

	return 0;	/* no saved state */
    }
    else {		/* need to open specified file and load it */
#ifndef PURE_ANSI
	ss_restore_state(file,offset);
#endif /* PURE_ANSI */
	return 1;	/* saved state loaded */
    }
}

#define SMALLEST_BLOCK_SIZE 16

static long *
ss_malloc0(size, align, fe_num, actual_sizep)
    size_t size;
    int align;
    int fe_num;
    long *actual_sizep;
{
    long **bestp = (long **) 0;
    long best_size = 0x7fffffff;
    long best_diff = 0;
    long **pp;
    long p_size;
    long p_diff;
    long *retblock;

    if (align)
	size = round(size, MAX_PAGE_SIZE);
    else
	size = round(size,8);
    
    p_diff = 0;
    for (pp = &amheader.freelist; *pp; pp = &FB_NEXT(*pp)) {
	if (align)
	    p_diff = (char *) round(*pp, MAX_PAGE_SIZE) - (char *) *pp;
	p_size = FB_SIZE(*pp) - p_diff;
	if (p_size >= 0 && size <= p_size && p_size <= best_size) {
	    bestp = pp;
	    best_size = p_size;
	    best_diff = p_diff;
	    if (best_size < size+SMALLEST_BLOCK_SIZE)
		break;		/* have best size with no more work */
	}
    }

    if (bestp == (long **) 0) {
	/* could not find block of ample size; must allocate new big block  */
	long *newblock;
	int newsize = AM_BLOCKSIZE;
	if (size > newsize)
	    newsize = round(size, MAX_PAGE_SIZE);

	if (amheader.nblocks >= AM_MAXBLOCKS)
	    fatal_error(fe_num, 0);

	newblock = alloc_big_block((size_t)newsize, fe_num);

	FB_NEXT(newblock) = amheader.freelist;
	FB_SIZE(newblock) = newsize;
	amheader.freelist = newblock;
	bestp = &amheader.freelist;
	amheader.blocks[amheader.nblocks].start = newblock;
	amheader.blocks[amheader.nblocks].asize = newsize;
	amheader.nblocks++;
	best_size = newsize;
	best_diff = 0;		/* newblock is aligned properly */
    }

    retblock = (long *) ((char *) *bestp + best_diff);
    if (best_diff >= SMALLEST_BLOCK_SIZE)
	/* adjust size of unaligned space */
	FB_SIZE(*bestp) = best_diff;
    else
	/* remove block from free list */
	*bestp = FB_NEXT(*bestp);
    
    if (best_size - size >= SMALLEST_BLOCK_SIZE) {
	long *fb = (long *) ((char *) retblock + size);
	FB_SIZE(fb) = best_size - size;
	FB_NEXT(fb) = amheader.freelist;
	amheader.freelist = fb;
    }

    if (actual_sizep)
	*actual_sizep = size;

    return retblock;
}

long *
ss_pmalloc(size,fe_num,actual_sizep)
    size_t size;
    int fe_num;
    long *actual_sizep;
{
    return ss_malloc0(size,1,fe_num,actual_sizep);
}

long *
ss_malloc(size,fe_num)
    size_t size;
    int fe_num;
{
    return ss_malloc0(size,0,fe_num,(long *) 0);
}

#if 0
/*
 * The following two functions are for use with the dynamic foreign
 * interface.
 *
 * ss_fmalloc_start returns the address where alloc_big_block will allocate
 * its next block of memory.
 *
 * ss_fmalloc actually allocates this memory and returns a pointer to
 * it.
 */

long *
ss_fmalloc_start()
{
    long *retval;
/* #if defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
#if	(defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))) || \
	defined(MACH_SUBSTRATE) || defined(MSWin32)
    retval = (long *) next_big_block_addr;
#elif defined(HAVE_BRK)
    retval = (long *) sbrk(0);
    retval = (long *) round((long) retval,MAX_PAGE_SIZE);
#else
	retval = (long *) -1;
#endif	/* defined(HAVE_MMAP) || defined(MACH_SUBSTRATE) */
    if (retval == (long *) -1)
	fatal_error(FE_SS_FMALLOC,0);
    return retval;
}

long *
ss_fmalloc(size)
    size_t size;
{
    long * newblock ;

    size = round(size,MAX_PAGE_SIZE);
    newblock = alloc_big_block(size,FE_SS_FMALLOC);

    amheader.blocks[amheader.nblocks].start = newblock;
    amheader.blocks[amheader.nblocks].asize = size;
    amheader.nblocks++;
    return newblock;
}
#endif

void
ss_register_global(addr)
    long *addr;
{
    if (amheader.nglobals > AM_MAXGLOBALS)
	fatal_error(FE_SS_MAXGLOBALS,0);
    amheader.globals[amheader.nglobals].addr = addr;
    amheader.nglobals++;
}

#ifdef MSWin32
/* ms_pecoff_end calculated the offset to the end of a
   Microsoft Portable-Executable/Common-Object-File-Format file.
   
   The result is the offset to the first byte AFTER the pe/coff data.
 */
static DWORD ms_pecoff_end(HANDLE image_file)
{
    DWORD r;
    WORD head_offset;
    IMAGE_FILE_HEADER head;
    IMAGE_SECTION_HEADER section;
    long end;
    int s;
    
    r = SetFilePointer(image_file, 0x3c, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    if (!ReadFile(image_file, &head_offset, sizeof(head_offset), &r, NULL)
    	|| r != sizeof(head_offset)) return 0;
    
    r = SetFilePointer(image_file, head_offset+4, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    if (!ReadFile(image_file, &head, sizeof(head), &r, NULL)
    	|| r != sizeof(head)) return 0;
    	
    end = head_offset + 4 + sizeof(head) + head.SizeOfOptionalHeader;

    r = SetFilePointer(image_file, end, NULL, FILE_BEGIN);
    if (r == 0xFFFFFFFF) return 0;
    
    
    for (s = 0; s < head.NumberOfSections; s++) {
	if (!ReadFile(image_file, &section, sizeof(section), &r, NULL)
	    || r != sizeof(section)) return 0;
	end = max(end, section.PointerToRawData + section.SizeOfRawData);
    }

    return end;
}

#ifdef EXTERNAL_STATE
long ss_image_offset(const char *imagepath)
{
  char statepath[PATH_MAX];

  strcpy(statepath, imagepath);
  strcat(statepath, ".pst");
  if (access(statepath, R_OK) != -1) return -1;
  else return 0;
}
#else
long ss_image_offset(const char *image_name)
{
    DWORD image_size, pecoff_size;
    HANDLE image_file;
    
    
    image_file = CreateFile(image_name, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (image_file == INVALID_HANDLE_VALUE) return 0;

    pecoff_size = ms_pecoff_end(image_file);
    image_size = GetFileSize(image_file, NULL);
    
    CloseHandle(image_file);

    if (image_size == 0xFFFFFFFF) return 0;
    
    if (image_size > pecoff_size) return pecoff_size;
    else return 0;
}
#endif

int ss_save_image_with_state(const char * new_image_name)
{
    char image_name[MAX_PATH];
    DWORD l;
    
    l = GetModuleFileName(NULL, image_name, MAX_PATH);
    if (l <= 0 || l >= MAX_PATH) {
    	printf("ss_save_image: Couldn't get module name.\n");
    	return 0;
    }
    
    if (!CopyFile(image_name, new_image_name, FALSE)) {
    	printf("ss_save_image: Couldn't copy %s to %s\n", image_name, new_image_name);
    	return 0;
    }
    
    return ss_attach_state_to_file(new_image_name);
}

int ss_attach_state_to_file(const char *image_name)
{   
    HANDLE image_file;
    DWORD state_offset;
    
    image_file = CreateFile(image_name, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (image_file == INVALID_HANDLE_VALUE) {
    	printf("ss_save_image: Couldn't open %s\n", image_name);
    	return 0;
    }
   
    state_offset = ms_pecoff_end(image_file);

    CloseHandle(image_file);

    if (state_offset == 0) {
    	printf("ss_save_image: Couldn't find end of PE/COFF file %s\n", image_name);
    	return 0;
    }
        
    return ss_save_state(image_name, state_offset);
}

#endif /* MSWin32 */

#ifdef UNIX
#if defined(HP_AOUT_800)

#include <filehdr.h>

static unsigned long image_end(int image_file)
{
    size_t r;
    struct header head;
    
    r = read(image_file, &head, sizeof(head));
    if (r != sizeof(head)) return 0;
    
    return head.som_length;
}

#endif /* HP_AOUT_800 */

#if defined(SUNOS_AOUT)

#include <a.out.h>
#include <stab.h>

static unsigned long image_end(int image_file)
{
    size_t r;
    off_t sr, string_off;
    unsigned long str_size;
    struct exec head;
    struct stat info;
    
    r = fstat(image_file, &info);
    if (r == -1) return 0;

    r = read(image_file, &head, sizeof(head));
    if (r != sizeof(head)) return 0;
    
    if (head.a_syms == 0) {
        return N_SYMOFF(head);
    } else {
        string_off = N_STROFF(head);

	sr = lseek(image_file, string_off, SEEK_SET);
	if (sr == -1) return 0;

	r = read(image_file, &str_size, sizeof(str_size));
	if (r != sizeof(str_size)) return 0;

	return round_up(string_off + str_size, PAGSIZ);
    }
}

#endif /* SUNOS_AOUT */


#if defined(AIX_XCOFF)

/* These two macros are defined in xcoff.h */
#undef TP_INT
#undef TP_DOUBLE

#include <xcoff.h>
#include <filehdr.h>
#include <scnhdr.h>

static unsigned long image_end(int image_file)
{
    size_t r;
    off_t sr, string_off, end;
    int i;
    unsigned long str_size;
    FILHDR head;
    
    r = read(image_file, &head, FILHSZ);
    if (r != sizeof(head)) return 0;

    if (head.f_nsyms == 0) {
        SCNHDR *scnhdrs;
        scnhdrs = (SCNHDR *)malloc(head.f_nscns*SCNHSZ);
	if (!scnhdrs) return 0;
	
	sr = lseek(image_file, FILHSZ + head.f_opthdr, SEEK_SET);
	r = read(image_file, scnhdrs, head.f_nscns*SCNHSZ);

        for (end = 0, i = 0; i < head.f_nscns; i++) {
	  if (scnhdrs[i].s_scnptr)
	      end = max(end, scnhdrs[i].s_scnptr + scnhdrs[i].s_size);
	}

	free(scnhdrs);
	return end;
    } else {
        string_off = head.f_symptr + head.f_nsyms * SYMESZ; 

	sr = lseek(image_file, string_off, SEEK_SET);
	if (sr == -1) return 0;

	r = read(image_file, &str_size, sizeof(str_size));
	if (r != sizeof(str_size)) return 0;

	return round_up(string_off + str_size, 1);
    }
}

#endif /* AIX_XCOFF */

#ifdef HAVE_LIBELF
#include <libelf.h>

#ifdef USE_ELF_SECTION_FOR_IMAGE

static Elf_Scn *find_named_section(Elf *elf, const char *name, int create)
{
    Elf32_Ehdr *header;
    Elf32_Half secstr_index;
    Elf_Scn *section, *str_section;
    Elf32_Shdr *section_header;
    char *section_name;
    Elf_Data *str_data;
    Elf_Void *new_d_buf;
    Elf32_Word name_offset;
    size_t name_size, new_d_size;
    
    header = elf32_getehdr(elf);
    if (!header) return NULL;
    
    secstr_index = header->e_shstrndx;
    section = NULL;

    while ((section = elf_nextscn(elf, section))) {
    	section_header = elf32_getshdr(section);
    	if (!section_header) return NULL;
    	section_name = elf_strptr(elf, secstr_index, section_header->sh_name);
    	if (!section_name) return NULL;
    	if (strcmp(name, section_name) == 0) return section;
    }

    if (create) {
    	name_size = strlen(name)+1;
    	
    	str_section = elf_getscn(elf, secstr_index);
    	str_data = elf_getdata(str_section, NULL);
    	new_d_size = str_data->d_size + name_size;
    	new_d_buf = malloc(new_d_size);
    	if (!new_d_buf) return NULL;
    	memcpy(new_d_buf, str_data->d_buf, str_data->d_size);
    	memcpy(((char *)new_d_buf)+str_data->d_size, name, name_size);
    	name_offset = str_data->d_size;
    	
    	str_data->d_buf = new_d_buf;
    	str_data->d_size = new_d_size;
    	
    	elf_flagdata(str_data, ELF_C_SET, ELF_F_DIRTY);
    	
    	section = elf_newscn(elf);
    	if (!section) return NULL;
    	section_header = elf32_getshdr(section);
    	if (!section_header) return NULL;
    	
    	section_header->sh_name = name_offset;
    	
    	return section;
    } else return NULL;
}

long ss_image_offset(const char *imagepath)
{
  int image_file, r;
  Elf *elf;
  Elf_Scn *scn;
  Elf32_Shdr *shdr;
  long image_offset;

  if (imagepath == NULL)
    return 0;

  if (elf_version(EV_CURRENT) == EV_NONE) return 0;
    
  image_file = open(imagepath, O_RDONLY);
  if (image_file == -1)
    fatal_error(FE_SS_OPENERR,(long)imagepath);
    
  elf = elf_begin(image_file, ELF_C_READ, NULL);
  if (elf == NULL)
    fatal_error(FE_SS_OPENERR,(long)imagepath);

  scn = find_named_section(elf, "ALS Prolog State", 0);
  if (scn == NULL) image_offset = 0;
  else {
    shdr = elf32_getshdr(scn);
    if (shdr == NULL) image_offset =  0;
    else image_offset = shdr->sh_offset;
  }

  elf_end(elf);

  r = close(image_file);
  if (r == -1)
    fatal_error(FE_SS_OPENERR,(long)imagepath);

  return image_offset;
}

#else

static unsigned long image_end(int image_file)
{
    Elf *elf;
    Elf_Scn *scn;
    Elf32_Shdr *shdr;
    size_t end;
    
    if (elf_version(EV_CURRENT) == EV_NONE) return 0;

    elf = elf_begin(image_file, ELF_C_READ, NULL);
    if (elf_kind(elf) != ELF_K_ELF) return 0;

    scn = NULL;
    end = 0;
    while ((scn = elf_nextscn(elf, scn))) {
        shdr = elf32_getshdr(scn);
	if (shdr == NULL) return 0;

        if (shdr->sh_type != SHT_NOBITS) {
/*
	    printf("sh_type: %u sh_offset: %u sh_size: %u\n",
		   shdr->sh_type, shdr->sh_offset, shdr->sh_size);
*/
	    end = max(end, shdr->sh_offset + shdr->sh_size);
	}
    }

    if (elf_end(elf) != 0) return 0;

    /* printf("image_end: %u\n", end); */
    return end;
}

#endif /* USE_ELF_SECTION_FOR_IMAGE */
#endif /* HAVE_LIBELF */

#ifdef EXTERNAL_STATE
static unsigned long image_end(int image_file)
{
  return -1;
}
#endif

#ifndef USE_ELF_SECTION_FOR_IMAGE
#ifdef EXTERNAL_STATE
long ss_image_offset(const char *imagepath)
{
  char statepath[PATH_MAX];

  strcpy(statepath, imagepath);
  strcat(statepath, ".pst");
  if (access(statepath, R_OK) != -1) return -1;
  else return 0;
}
#else
long ss_image_offset(const char *imagepath)
{
    unsigned long file_size, image_size;
    int file, fstat_result;
    struct stat file_status;

    if (imagepath == NULL)
	return 0;
    
    file = open(imagepath, O_RDONLY);
    
    if (file == -1)
	fatal_error(FE_SS_OPENERR,(long)imagepath);

    image_size = image_end(file);
    fstat_result = fstat(file, &file_status);
        
    close(file);

    if (fstat_result != 0)
	fatal_error(FE_SS_OPENERR,(long)imagepath);

    file_size = file_status.st_size;
 
    if (file_size > image_size)
	return image_size;
    else return 0;
}
#endif
#endif

static int copy(const char *filename, const char *copyname)
{
    unsigned char *buf;
    int f, c, r;
    struct stat s; 
    
    f = open(filename, O_RDONLY);
    if (f == -1) return -1;
    
    r = fstat(f, &s);
    if (r != 0) {
    	close(f);
        return -1;
    }
    
    buf = malloc((size_t)s.st_size);
    if (buf == NULL) {
    	close(f);
        return -1;
    }
    
    r = read(f, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
    	close(f);
        return -1;
    }

    close(f);
    
    c = open(copyname, O_WRONLY | O_CREAT | O_TRUNC, 0777);
    if (c == -1) {
    	free(buf);
    	return -1;
    }
    
    r = write(c, buf, (size_t)s.st_size);
    if (r != s.st_size) {
    	free(buf);
        close(f);
        return -1;
    }
    
    close(c);
    
    free(buf);
    
    return 0;
}

#ifdef USE_ELF_SECTION_FOR_IMAGE
int ss_save_image_with_state(const char * new_image_name)
{
    
    if (copy(executable_path, new_image_name) != 0) {
	printf("ss_save_image: Couldn't copy %s to %s\n", executable_path, new_image_name);
    	return 0;
    }

    return ss_attach_state_to_file(new_image_name);
}

int ss_attach_state_to_file(const char * image_name)
{
    char tmp_name[L_tmpnam];
    mem_file_info tmp_mmap;
    int fd, r;
    Elf *elf;
    Elf_Scn *scn;
    Elf32_Shdr *shdr;
    Elf_Data *data;
    
    tmpnam(tmp_name);

    ss_save_state(tmp_name, 0);

    if (!open_memory_file(tmp_name, &tmp_mmap)) return 0;

    if (elf_version(EV_CURRENT) == EV_NONE) return 0;

    fd = open(image_name, O_RDWR);
    if (fd == -1) return 0;

    elf = elf_begin(fd, ELF_C_RDWR, NULL);
    if (elf == NULL) return 0;

    scn = find_named_section(elf, "ALS Prolog State", 1);
    if (scn == NULL) return 0;

    shdr = elf32_getshdr(scn);
    if (shdr == NULL) return 0;

    shdr->sh_type = SHT_PROGBITS;
  
    data = elf_getdata(scn, NULL);
    if (!data) {
      data = elf_newdata(scn);
      if (data == NULL) return 0;
    }
  
    data->d_buf = tmp_mmap.start;
    data->d_size = tmp_mmap.length;
    data->d_align = MAX_PAGE_SIZE;
    data->d_type = ELF_T_WORD;

    elf_flagdata(data, ELF_C_SET, ELF_F_DIRTY);
  
    r = elf_update(elf, ELF_C_WRITE);
    if (r == -1) return 0;
  
    elf_end(elf);

    r = close(fd);
    if (r == -1) return 0;

    close_memory_file(&tmp_mmap);

    remove(tmp_name);

    return 1;
}

#else
int ss_save_image_with_state(const char * new_image_name)
{
    
    if (copy(executable_path, new_image_name) != 0) {
	printf("ss_save_image: Couldn't copy %s to %s\n", executable_path, new_image_name);
    	return 0;
    }

    return ss_attach_state_to_file(new_image_name);
}

int ss_attach_state_to_file(const char *image_name)
{
    int image_file;
    long state_offset;

    image_file = open(image_name, O_RDONLY);
    if (image_file == -1) {
    	printf("ss_save_image: Couldn't open %s\n", image_name);
    	return 0;
    }
   
    state_offset = image_end(image_file);

    close(image_file);

    if (state_offset == 0) {
    	printf("ss_save_image: Couldn't find end of image file %s\n", image_name);
    	return 0;
    }
        
    return ss_save_state(image_name, state_offset);
}
#endif

#endif /* UNIX */


#ifndef PURE_ANSI
#ifdef EXTERNAL_STATE
int
ss_save_state(const char *filename, long offset)
{
  char statename[PATH_MAX];
  strcpy(statename, filename);
  strcat(statename, ".pst");
  os_store_db(&current_engine.db, statename, 0);
  return 1;

}
#else
int
ss_save_state(const char *filename, long offset)
{
	os_store_db(&current_engine.db, filename, offset);
	return 1;
}
#endif

#if 0
{
    int   ssfd;
    int   bnum = 0, gnum;
    long delta_offset;

    /*
     * Open the saved state file.
     */
#ifdef UNIX
    ssfd = open(filename, O_WRONLY);
    if (ssfd == -1)
      ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0777);
#else
    ssfd = open(filename, O_WRONLY | O_BINARY);
    if (ssfd == -1) ssfd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY);
#endif
    if (ssfd < 0)
	return 0;

    if (lseek(ssfd, offset, 0) != offset) goto ss_err;
    
    delta_offset = round_up(offset+sizeof(offset), MAX_PAGE_SIZE) - offset;
    if (write(ssfd, (char *)&delta_offset, sizeof(delta_offset)) < 0) goto ss_err;
    offset += delta_offset;

    if (lseek(ssfd, offset, 0) != offset) goto ss_err;
    
    /*
     * Get the global values and save in amheader
     */

    for (gnum=0; gnum < amheader.nglobals; gnum++)
	amheader.globals[gnum].value = *amheader.globals[gnum].addr;

    
    /*
     * Compute the fsize values for each block.  The fsize value is the
     * amount of space that the block should actually occupy in the file.
     * The reason that this is not necessarily equal to asize is because
     * it is often the case that there is a large free block sitting at
     * the end of an allocated block.  Note that we need to preserve the
     * FB_SIZE and FB_NEXT information associated with this last free block.
     * This is why we add 2*sizeof(long) to the computed size in the case
     * where we've found the appropriate free block.  If we're using mmap,
     * we will need to round this size up as well. 
     */

    for (bnum=0; bnum < amheader.nblocks; bnum++) {
	char *fb = (char *) amheader.freelist;
	char *blockstart = (char *) amheader.blocks[bnum].start;
	char *blockend = blockstart + amheader.blocks[bnum].asize;

	while (fb && (fb + FB_SIZE(fb) != blockend))
	    fb = (char *) FB_NEXT(fb);
	
	amheader.blocks[bnum].fsize = (fb) ? fb - blockstart +  2*sizeof(long)
					: amheader.blocks[bnum].asize;
/* #ifdef HAVE_MMAP */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
	amheader.blocks[bnum].fsize = round(amheader.blocks[bnum].fsize, MAX_PAGE_SIZE);
#endif /* HAVE_MMAP */

    }


    /*
     * Save the blocks to the save file.
     */
    
    for (bnum=0; bnum < amheader.nblocks; bnum++)
	if (write(ssfd,
		  (char *)amheader.blocks[bnum].start,
		  (size_t)amheader.blocks[bnum].fsize) < 0)
	    goto ss_err;
    

    close(ssfd);
    return 1;

ss_err:
    printf("!!Save_state error writing file: bnum=%d errno=%d\n",bnum,errno);
    close(ssfd);
    unlink((char *)filename);
    return 0;
}
#endif

#define SS_MALLOCQUANT	4096
#define SS_MALLOCDIST	16384

#ifdef EXTERNAL_STATE
static void
ss_restore_state(const char *filename,long offset)
{
  char statename[PATH_MAX];
  strcpy(statename, filename);
  strcat(statename, ".pst");
  os_load_db(&current_engine.db, statename, 0);
}
#else
static void
ss_restore_state(const char *filename,long offset)
{
	os_load_db(&current_engine.db, filename, offset);
}
#endif

#if 0
{
    int ssfd, gnum;
    long delta_offset;
#if defined(HAVE_MMAP) || defined(MSWin32) || defined(MACH_SUBSTRATE)
    int  bnum;
#endif
    struct am_header hdr;
#if (defined(__GO32__) || defined(__DJGPP__))
	unsigned char dos_exe[6];
#endif

    /* Open the file */
    ssfd = open(filename, O_RDONLY|O_BINARY);

    if (ssfd < 0)
	fatal_error(FE_SS_OPENERR,(long)filename);

#if (defined(__GO32__) || defined(__DJGPP__))
	read(ssfd, dos_exe, sizeof(dos_exe));
	if (dos_exe[0] == 'M' && dos_exe[1] == 'Z') /* skip stub */
	{
		int blocks = (unsigned int)dos_exe[4] + (unsigned int)dos_exe[5] * 256;
		int partial = (unsigned int)dos_exe[2] + (unsigned int)dos_exe[3] * 256;
		offset += blocks * 512;
		if (partial)
			offset += partial - 512;
	}
#endif

    if (lseek(ssfd, offset, 0) < 0)
	goto ss_err;

    if (read(ssfd, (char *)&delta_offset, sizeof(delta_offset)) < 0) goto ss_err;
    offset += delta_offset;

    /* Seek to amheader, get amheader, and seek back to amheader */
    if (lseek(ssfd, offset, 0) < 0)
	goto ss_err;
    
    if (read(ssfd, (char *)&hdr, sizeof (struct am_header)) < 0)
	goto ss_err;
    
    if (lseek(ssfd, offset, 0) < 0)
        goto ss_err;
    
    /* Check integrity information */

    if (hdr.integ_als_mem != &als_mem ||
		hdr.integ_als_mem_init != als_mem_init ||
		hdr.integ_w_unify != w_unify ||
		strncmp(hdr.integ_version_num, VERSION_STRING, 12) != 0 ||
		strncmp(hdr.integ_processor, ProcStr, 12) != 0 ||
		strncmp(hdr.integ_minor_os, MinorOSStr, 12) != 0)
	fatal_error(FE_SS_INTEGRITY,0);

/* #if	defined(HAVE_MMAP) && defined(HAVE_DEV_ZERO) */
#if	defined(HAVE_MMAP) && (defined(HAVE_DEV_ZERO) || defined(HAVE_MMAP_ZERO))
    /* Get the blocks */
    for (bnum = 0; bnum < hdr.nblocks; bnum++) {
#ifdef HAVE_MMAP_ZERO
	if (mmap((caddr_t) hdr.blocks[bnum].start,
		 hdr.blocks[bnum].fsize,
		 PROT_READ | PROT_WRITE,
		 MAP_FILE | MAP_PRIVATE | MAP_FIXED,
		 ssfd,
		 offset) != (caddr_t) hdr.blocks[bnum].start)
	    goto ss_err;
#else
	if (mmap((caddr_t) hdr.blocks[bnum].start,
		 hdr.blocks[bnum].fsize,
#if defined(arch_sparc)
		 PROT_EXEC |
#endif  /* arch_sparc */
		 PROT_READ | PROT_WRITE,
		 MAP_PRIVATE | MAP_FIXED,
		 ssfd,
		 offset) != (caddr_t) hdr.blocks[bnum].start)
	    goto ss_err;
#endif
	offset += hdr.blocks[bnum].fsize;

	/*
	 * If the size of the block in the file is not equal to the size
	 * which needs to be allocated, we need to allocate the remaining
	 * portion by mapping /dev/zero...
	 */

	if (hdr.blocks[bnum].asize != hdr.blocks[bnum].fsize) {
	    caddr_t np;
#ifdef HAVE_MMAP_ZERO
	    np = mmap((caddr_t)hdr.blocks[bnum].start + hdr.blocks[bnum].fsize,
		      hdr.blocks[bnum].asize - hdr.blocks[bnum].fsize,
		      PROT_READ | PROT_WRITE,
		      MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED,
		      -1,
		      0);
#else
	    int zfd;
	    if ((zfd = open("/dev/zero", O_RDWR)) == -1)
	      goto ss_err;

	    np = mmap((caddr_t)hdr.blocks[bnum].start + hdr.blocks[bnum].fsize,
		      hdr.blocks[bnum].asize - hdr.blocks[bnum].fsize,
#if defined(arch_sparc)
		      PROT_EXEC |
#endif  /* arch_sparc */
		      PROT_READ | PROT_WRITE,
		      MAP_PRIVATE | MAP_FIXED,
		      zfd,
		      0);

	    close(zfd);			/* close /dev/zero */
#endif
	    if (np != (caddr_t)hdr.blocks[bnum].start + hdr.blocks[bnum].fsize)
	      goto ss_err;
	  }
      }

#elif	defined(MACH_SUBSTRATE)
    /* Get the blocks */
    for (bnum = 0; bnum < hdr.nblocks; bnum++) {
	long *bs = hdr.blocks[bnum].start;
        if (KERN_SUCCESS != vm_allocate(task_self(), (vm_address_t *) &bs,
				    hdr.blocks[bnum].asize, FALSE)
	|| bs != hdr.blocks[bnum].start)
	    goto ss_err;
	if (KERN_SUCCESS !=
		map_fd(	ssfd,
			(vm_offset_t) offset,
			(vm_offset_t *) &bs,
			FALSE,
			(vm_size_t) (hdr.blocks[bnum].fsize)))
	    goto ss_err;
	offset += hdr.blocks[bnum].fsize;
    }
#elif	defined(MSWin32)
    {
        HANDLE file;
        DWORD r;
        
	file = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	if (file == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
	
	r = SetFilePointer(file, offset, NULL, FILE_BEGIN);
    	if (r == 0xFFFFFFFF) goto ss_err;

	for (bnum=0; bnum < hdr.nblocks; bnum++) {

	    if (VirtualAlloc(hdr.blocks[bnum].start, hdr.blocks[bnum].asize,
		MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE) == NULL) goto ss_err;

	    if (!ReadFile(file, hdr.blocks[bnum].start, hdr.blocks[bnum].fsize, &r, NULL)
    	    	|| r != hdr.blocks[bnum].fsize) goto ss_err;
    	}
    		
	CloseHandle(file);
    }
#if 0
/* currently broken, make it work someday. */
{
    HANDLE file, file_map;
    
    file = CreateFile(filename, GENERIC_READ | GENERIC_WRITE, 0 /*FILE_SHARE_READ*/, NULL,
                            OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, NULL);
    if (file == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
printf("created file\n");

    file_map = CreateFileMapping(file, NULL, PAGE_WRITECOPY, 0, hdr.blocks[bnum].asize, NULL);
    if (file_map == INVALID_HANDLE_VALUE) fatal_error(FE_SS_OPENERR,(long)filename);
printf("created file mapping\n");    
    for (bnum=0; bnum < hdr.nblocks; bnum++) {

	if (MapViewOfFileEx(file_map, FILE_MAP_COPY, 0, offset,
		hdr.blocks[bnum].fsize, hdr.blocks[bnum].start)
		 == NULL) goto ss_err;
printf("mapped view\n");
    }
}
#endif
#elif	defined(HAVE_BRK)
    {
	long *mchain = (long *) 0;
	long **mptr = (long **) 0;
	char *bstart, *bend;
	/* Get the blocks */
	for (bnum=0; bnum < hdr.nblocks; bnum++) {
	    bstart = (char *) hdr.blocks[bnum].start;
	    bend = bstart + hdr.blocks[bnum].asize;
	    while ((char *) sbrk(0) < bstart-SS_MALLOCDIST) {
		mptr = (long **) malloc(SS_MALLOCQUANT);
		if ((long) mptr < 0)
		    goto ss_err;
		*mptr = mchain;
		mchain = (long *) mptr;
	    }
	    if ((char *) sbrk(0) > bstart || brk(bend) < 0)
		goto ss_err;
	    if (read(ssfd,bstart,hdr.blocks[bnum].fsize) < 0)
		goto ss_err;
	}
    
	/* Free up memory malloc'd in previous step */
	while (mchain) {
	    mptr = (long **) mchain;
	    mchain = *mptr;
	    free((char *) mptr);
	}
    }
#else

    /* ceh - We need a non-brk implementation of the above code! */
    goto ss_err;

#endif	/* HAVE_MMAP */

    /* Initialize the globals */
    for (gnum=0; gnum < hdr.nglobals; gnum++)
	*hdr.globals[gnum].addr = hdr.globals[gnum].value;
    
    als_mem = hdr.blocks[0].start;
    
    close(ssfd);
    return;


ss_err:
    close(ssfd);
    fatal_error(FE_ALS_MEM_INIT,0);
}
#endif /* PURE_ANSI */
#endif

/*
 * ss_saved_state_present will test to see if the saved state is already
 * magically present in the image.  If it is, then all we have to do
 * is set als_mem and initialize the globals.
 */

static int
ss_saved_state_present()
{
#ifdef MACH_SUBSTRATE
    kern_return_t stat;
    pointer_t dummy_buf;
    unsigned int dummy_size;

    stat = vm_read( task_self(),
		    (vm_address_t) CODESTART,
		    current_page_size,
		    &dummy_buf,
		    &dummy_size );


    if (stat != KERN_SUCCESS)
	return 0;
    else {
	int gnum;

	(void) vm_deallocate( task_self(),
			      (vm_address_t) dummy_buf,
			      (vm_size_t) dummy_size );

	als_mem = (long *) CODESTART;

	/* Initialize the globals */
	for (gnum=0; gnum < amheader.nglobals; gnum++)
	    *amheader.globals[gnum].addr = amheader.globals[gnum].value;

	return 1;
    }
#else	/* MACH_SUBSTRATE */
    return 0;
#endif	/* MACH_SUBSTRATE */
}

#if 0

int pbi_save_app_with_obp(void)
{
    PWord v1, v2, v3, v4, v5;
    int t1, t2, t3, t4, t5;
    UCHAR *name;
    Str255 newAppName, OBPName;
    FSSpec newAppSpec, OBPSpec;
    OSErr err;
    short resRef, obpRef;
    
    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if (!getstring(&name, v1, t1)) PI_FAIL;
    
    if (t2 != PI_LIST) PI_FAIL;
    
    //if (t3 != PI_LIST) PI_FAIL;
    
    c2pstrcpy(newAppName, name);
    
    err = DuplicateThisApplication(newAppName);
    if (err != noErr) PI_FAIL;
    
    err = FSMakeFSSpec(0, 0, newAppName, &newAppSpec);
    if (err != noErr && err != fnfErr) PI_FAIL;
    
    resRef = FSpOpenResFile(&newAppSpec, fsRdWrPerm);
    if (resRef == -1) PI_FAIL;

    while (t2 == PI_LIST) {
	PWord h; int th;
    	Handle obpHandle;
    	int newResource;
	long length, readLength;
    	
	PI_gethead(&h, &th, v2);
	if (!getstring(&name, h, th)) PI_FAIL;
	c2pstrcpy(OBPName, name);
	
	err = FSMakeFSSpec(0, 0, OBPName, &OBPSpec);
	if (err != noErr && err != fnfErr) PI_FAIL;
	
	err = FSpOpenDF(&OBPSpec, fsRdPerm, &obpRef);
	if (err != noErr) PI_FAIL;

	err = GetEOF(obpRef, &length);
	if (err != noErr) PI_FAIL;

	obpHandle = Get1NamedResource('OBPT', OBPSpec.name);
	if (obpHandle) { 
	    newResource = 0;
	    SetHandleSize(obpHandle, length);
	    if (MemError() != noErr) PI_FAIL;
	} else {
	    newResource = 1;
	    obpHandle = NewHandle(length);
	    if (obpHandle == NULL) PI_FAIL;
	}
		
	HLock(obpHandle);
	if (MemError() != noErr) PI_FAIL;
		
	readLength = length;
	err = FSRead(obpRef, &readLength, *obpHandle);
	if (err != noErr || readLength != length) PI_FAIL;
		
	FSClose(obpRef);
		
	HUnlock(obpHandle); 
	if (MemError() != noErr) PI_FAIL;
	
	if (newResource) {
	    AddResource(obpHandle, 'OBPT', Unique1ID('OBPT'), OBPSpec.name);
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(obpHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(obpHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(obpHandle);
	if (ResError() != noErr) PI_FAIL;
			
	PI_gettail(&v2, &t2, v2);
    }
    
    {
    Handle loadHandle;
    int newResource;
    PWord h; int th;
    long length, hlen;
    Str255 pname;
    
    loadHandle = Get1Resource('STR#', 128);
    if (loadHandle) {
    	newResource = 0;
    } else {
    	newResource = 1;
    	loadHandle = NewHandle(sizeof(short));
    	if (loadHandle == NULL) PI_FAIL;
    }
    **((short **)loadHandle) = 0;
    
    while (t3 == PI_LIST) {
    	PI_gethead(&h, &th, v3);
    	if (!getstring(&name, h, th)) PI_FAIL;
    	length = strlen(name);
    	hlen = GetHandleSize(loadHandle);
	(**((short **)loadHandle))++;
	SetHandleSize(loadHandle, hlen + length + 1);
	if (MemError() != noErr) PI_FAIL;
	c2pstrcpy(pname, name);
	BlockMove(pname, *loadHandle + hlen, length+1);
    	PI_gettail(&v3, &t3, v3);
    }
    
    if (newResource) {
	AddResource(loadHandle, 'STR#', 128, "\pAuto Load Files");
	if (ResError() != noErr) PI_FAIL;
    } else {
	ChangedResource(loadHandle);
	if (ResError() != noErr) PI_FAIL;
    }
    
    WriteResource(loadHandle);
    if (ResError() != noErr) PI_FAIL;
	
    ReleaseResource(loadHandle);
    if (ResError() != noErr) PI_FAIL;
    }
    
    {
	Handle initHandle;
	int newResource;
	long len;
	Str255 pname;
	
        if (!getstring(&name, v4, t4)) PI_FAIL;
	len = strlen(name);
	c2pstrcpy(pname, name);
	
	initHandle = Get1Resource('STR ', 128);
	if (initHandle) {
    	    newResource = 0;
    	    SetHandleSize(initHandle, len+1);
    	    if (MemError() != noErr) PI_FAIL;
	} else {
    	    newResource = 1;
    	    initHandle = NewHandle(len+1);
    	    if (initHandle == NULL) PI_FAIL;
	}
	
	BlockMove(pname, *initHandle, len+1);

	if (newResource) {
	    AddResource(initHandle, 'STR ', 128, "\pInit Predicate");
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(initHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(initHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(initHandle);
	if (ResError() != noErr) PI_FAIL;
    }
 
     {
	Handle startHandle;
	int newResource;
	long len;
	Str255 pname;
	
        if (!getstring(&name, v5, t5)) PI_FAIL;
	len = strlen(name);
	c2pstrcpy(pname, name);
	
	startHandle = Get1Resource('STR ', 129);
	if (startHandle) {
    	    newResource = 0;
    	    SetHandleSize(startHandle, len+1);
    	    if (MemError() != noErr) PI_FAIL;
	} else {
    	    newResource = 1;
    	    startHandle = NewHandle(len+1);
    	    if (startHandle == NULL) PI_FAIL;
	}
	
	BlockMove(pname, *startHandle, len+1);

	if (newResource) {
	    AddResource(startHandle, 'STR ', 129, "\pStart Predicate");
	    if (ResError() != noErr) PI_FAIL;
	} else {
	    ChangedResource(startHandle);
	    if (ResError() != noErr) PI_FAIL;
	}
	WriteResource(startHandle);
	if (ResError() != noErr) PI_FAIL;
	
	ReleaseResource(startHandle);
	if (ResError() != noErr) PI_FAIL;
    }
   
    
    CloseResFile(resRef);
    
    PI_SUCCEED;
}
#endif

void heap_overflow(void)
{
    if (wm_normal <= DEFAULT_SAFETY/8) fatal_error(FE_OVER_HEAP, 0);
    wm_normal = wm_normal/2;

    /* raise a prolog interupt - there must be a cleaner way! */

#if 0
    //if (wm_regidx != 0)
#endif
    if (current_engine.reg_stack_top != current_engine.reg_stack_base)
    {
	wm_safety = -1;
    }
    wm_interrupt_caught = ALSSIG_HEAP_OVERFLOW;
}
