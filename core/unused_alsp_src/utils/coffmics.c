/*
 * coffmics.c	-- Merge Image and saved Code State (COFF version)
 *
 * Author: Kevin A. Buettner
 *
 */
#ifndef __GO32__
#include <a.out.h>
#else
/* #include <djcoff.h> */
#include <coff.h>

#define n_name   e.e_name
#define n_numaux e_numaux
#define n_offset e.e.e_offset
#define n_sclass e_sclass
#define n_scnum  e_scnum
#define n_value  e_value
#define n_zeroes e.e.e_zeroes
#define ISCOFF(x) ((x)==0x14c)
#endif
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef SYMESZ
#define SYMESZ sizeof(struct syment)
#endif

#undef round
#define round(x,s) ((((long) (x) -1) & ~(long)((s)-1)) + (s))

/* 
 * MISSBUFSIZE is the size of the buffer used for copying the input image to
 * the output image.
 */

#define MISSBUFSIZE 16384


/* 
 * fe reports a fatal error and then exits.
 */

static void
fe(char *mess, char *arg)
{
    if (arg)
	fprintf(stderr,mess,arg);
    else
	fprintf(stderr,mess);
    fprintf(stderr,"\n");
    exit(1);
}


/*
 * als_data_name is a character array containing the name of the segment
 * where we put the saved state in the elf file.
 */

static char als_data_name[] = "als_data";


/*
 * ss_symname contains the name of the symbol which we search for and fix up
 * to indicate the offset to the saved state data.
 */

static char ss_symname[] = "_saved_state_image_offset";


/*
 * Guess at the page size.  If sysconf is available, we will determine
 * it for real at run time.
 */

static long pagesize = 4096;


main(int argc,char **argv)
{
    int iifd, ssfd, oifd;
    long nbytes;
    struct stat ss_statbuf;
    long ss_offset;
    long ss_size;

#ifndef __GO32__
    struct filehdr fhdr;
    struct aouthdr ehdr;
    struct scnhdr  shdr;

    struct syment *symtab, *sym;
#else
    FILHDR fhdr;
    AOUTHDR ehdr;
    SCNHDR  shdr;

    SYMENT *symtab, *sym;
#endif
    char *strtab, *name;
    long nsyms;

    char buf[MISSBUFSIZE];

#   define iiname argv[1]
#   define ssname argv[2]
#   define oiname argv[3]


    if (argc != 4) 
	fe("Usage: als-mics image-name saved-state-name output-image-name", 0);
    
    /* Determine the page size if possible */
#ifdef _SC_PAGESIZE
    pagesize = sysconf(_SC_PAGESIZE);
#endif

    /*
     * Step 1: Copy the input image to the output image.
     */
    
    iifd = open(iiname, O_RDONLY);
    if (iifd < 0)
	fe("Error opening %s for input", iiname);
    
    oifd = open(oiname, O_RDWR|O_CREAT|O_TRUNC, 0777);
    if (oifd < 0)
	fe("Error opening %s for output",oiname);
    
    while ( (nbytes = read(iifd, buf, MISSBUFSIZE)) ) {
	if (nbytes < 0)
	    fe("Error encountered while reading %s", iiname);

	if (write(oifd, buf, nbytes) != nbytes)
	    fe("Error encountered while writing to %s", oiname);
    }

    close(iifd);


    /*
     * Step 2:  Open the save state file and stat it for future processing
     */
    
    ssfd = open(ssname, O_RDONLY);
    if (ssfd < 0)
	fe("Error opening saved state file %s for read access", ssname);
    if (fstat(ssfd, &ss_statbuf) < 0)
	fe("Cannot fstat file descriptor associated with %s", ssname);
    
    /*
     * Step 3:  Read the file header and a.out header
     */
    
    if (   lseek(oifd, 0, 0) < 0
#ifndef __GO32__
	|| read(oifd, &fhdr, sizeof (struct filehdr)) < 0
	|| read(oifd, &ehdr, sizeof (struct aouthdr)) < 0 )
#else
	|| read(oifd, &fhdr, sizeof (FILHSZ)) < 0
	|| read(oifd, &ehdr, sizeof (AOUTSZ)) < 0 )
#endif
	fe("Cannot read header information",0);
    
#ifdef ISCOFF
    if (!ISCOFF(fhdr.f_magic))
	fe("Bad magic value",0);
#elif defined(U802TOCMAGIC)
    /* Assume AIX */
    if (fhdr.f_magic != U802TOCMAGIC)
	fe("Bad magic value",0);
#else
#error No magic value to compare against
#endif
    
    /*
     * Step 4: Read symbol and string tables
     */

    nbytes = fhdr.f_nsyms * SYMESZ;
    symtab = (struct syment *) malloc(nbytes);

    if (!symtab)
	fe("Unable to allocate sufficient space for symbol table", 0);
    
    if (lseek(oifd, fhdr.f_symptr, 0) < 0)
	fe("Unable to seek to symbol table",0);
    
    if (read(oifd, (char *) symtab, nbytes) != nbytes)
	fe("Unable to read symbol table",0);
    
    if (read(oifd, (char *) &nbytes, 4) != 4)
	fe("Unable to read size of string table",0);
    
    strtab = malloc(nbytes);
    if (!strtab)
	fe("Unable to allocate sufficient space for string table",0);
    
    if (read(oifd, strtab+4, nbytes-4) != nbytes-4)
	fe("Unable to read string table",0);

#define advsym(sym,n) ((sym) = (struct syment *) (((char *) (sym)) + SYMESZ*(n)))
    
    for (sym = symtab, nsyms= fhdr.f_nsyms; nsyms>0; nsyms--,advsym(sym,1)) {
	if (sym->n_scnum != N_DEBUG && sym->n_sclass == C_EXT) {
	    if (sym->n_zeroes)
		name = sym->n_name;
	    else
		name = strtab + sym->n_offset;
	    
	    if (strcmp(name, ss_symname) == 0 || strcmp(name, ss_symname+1) == 0)
		break;
	}

	if (sym->n_numaux) {
	    nsyms -= sym->n_numaux;
	    advsym(sym, sym->n_numaux);
	}
    }

    if (!nsyms)
	fe("Symbol %s not found in image.", ss_symname);
    
    if (sym->n_scnum <= 0)
	fe("Bad section number for symbol %s", ss_symname);

    if (lseek(oifd,
              sizeof fhdr + fhdr.f_opthdr 
			  + sizeof shdr * (sym->n_scnum - 1),
	      0) < 0)
	fe("Can not seek to section for symbol %s", ss_symname);
    
    if (read(oifd, &shdr, sizeof shdr) != sizeof shdr)
	fe("Unable to read section associated with symbol %s", ss_symname);
    
    if (!(   shdr.s_vaddr <= sym->n_value 
	  && sym->n_value < shdr.s_vaddr + shdr.s_size))
	fe("Integrity check between symbol value and section failed", 0);

    if (lseek(oifd, sym->n_value - shdr.s_vaddr + shdr.s_scnptr, 0) < 0)
	fe("Unable to seek to value associated with symbol %s",ss_symname);
    
    if (read(oifd, &ss_offset, 4) != 4)
	fe("Unable to read value associated with symbol %s", ss_symname);
    if (ss_offset == 0) {
	long epos = lseek(oifd, 0, 2);	/* seek to end of file */
	long padsize;
	ss_offset = round(epos+4, pagesize);
	if (ss_offset < 0)
	    fe("Unable to seek to end of file %s", oiname);
	padsize = (ss_offset-4) - epos;
	if (padsize > 4) {
	    if (write(oifd, &padsize, 4) != 4)
		fe("Unable to write padding to end of file %s", oiname);
	    memset(buf, 0, padsize-4);
	    if (write(oifd, buf, padsize-4) != padsize-4)
		fe("Unable to write padding to end of file %s", oiname);
	}

	if (lseek(oifd, sym->n_value - shdr.s_vaddr + shdr.s_scnptr, 0) < 0)
	    fe("Unable to seek to value associated with symbol %s",ss_symname);
	
	if (write(oifd, &ss_offset, 4) != 4)
	    fe("Unable to write new value for symbol %s", ss_symname);
    }

    /*
     * For sections at the ends of files, the convention seems to be
     * to store the size of the section in the first four bytes followed
     * by the information that we wish to store
     */
    if (lseek(oifd, ss_offset - 4, 0) < 0)
	fe("Unable to seek to start of saved state area in %s", oiname);
    
    ss_size = ss_statbuf.st_size + 4;
    if (write(oifd, &ss_size, 4) < 0)
	fe("Error writing to %s", oiname);

    /*
     * copy the saved state file on to the end of the new image
     * file.
     */

    while ( (nbytes = read(ssfd, buf, MISSBUFSIZE)) ) {
	if (nbytes < 0)
	    fe("Error encountered while reading %s", ssname);

	if (write(oifd, buf, nbytes) != nbytes)
	    fe("Error encountered while writing to %s", oiname);
    }

    close(ssfd);
    
    /* end stuff */
#if 0
    if (ftruncate(oifd, lseek(oifd, 0, 1)) < 0)
	fe("Error truncating %s.", oiname);
#endif
    close(oifd);

    /*
     * Change the mode on the output image file to be the same as the
     * input image file.
     */

    if (stat(iiname, &ss_statbuf) == 0)
	(void) chmod(oiname, ss_statbuf.st_mode);

    exit(0);
}
