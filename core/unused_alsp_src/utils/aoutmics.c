/*
 * aoutmics.c	-- Merge Image and saved Code State (a.out version)
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 10-14-93
 */

#include <a.out.h>
/* #include <filehdr.h> */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>


/* 
 * MISSBUFSIZE is the size of the buffer used for copying the input image to
 * the output image.
 */

#define MISSBUFSIZE 16384

#undef round
#define round(x,s) ((((long) (x) -1) & ~(long)((s)-1)) + (s))

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


main(int argc,char **argv)
{
    int iifd, ssfd, oifd;
    long nbytes;
    struct stat ss_statbuf;
    long ss_offset;
    long ss_size;
    struct exec ehdr;
    struct nlist *symtab, *sym;
    char *strtab, *name;
    long nsyms;

    char buf[MISSBUFSIZE];

#   define iiname argv[1]
#   define ssname argv[2]
#   define oiname argv[3]


    if (argc != 4) 
	fe("Usage: als-mics image-name saved-state-name output-image-name", 0);
    
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
     * Step 3:  Read the exec header.
     */
    
    if (   lseek(oifd, 0, 0) < 0
	|| read(oifd, &ehdr, sizeof ehdr) < 0)
	fe("Cannot read header information", 0);
    
    if (N_BADMAG(ehdr))
	fe("Bad magic value in header of %s", oiname);
    
    /*
     * Step 4: Read symbol and string tables
     */
    
    nbytes = ehdr.a_syms;
    symtab = (struct nlist *) malloc(nbytes);
fprintf(stderr,"ehdr.a_syms=%d  nbytes=%d  symtab=%d\n",
    			ehdr.a_syms, nbytes,symtab);


    if (!symtab)
	fe("Unable to allocate sufficient space for symbol table", 0);
    
    if (lseek(oifd, N_SYMOFF(ehdr), 0) < 0)
	fe("Unable to seek to symbol table", 0);
    
    if (read(oifd, (char *) symtab, nbytes) != nbytes)
	fe("Unable to read symbol table", 0);
    
    if (read(oifd, (char *) &nbytes, 4) != 4)
	fe("Unable to read size of string table", 0);
    
    strtab = malloc(nbytes);
    if (!strtab)
	fe("Unable to allocate sufficient space for string table", 0);
    
    if (read(oifd, strtab+4, nbytes-4) != nbytes-4)
	fe("Unable to read string table",0);
    
    for (sym = symtab, nsyms = ehdr.a_syms / sizeof (struct nlist);
	 nsyms; nsyms--, sym++)
	if (   sym->n_un.n_strx
	    && strcmp(ss_symname, strtab + sym->n_un.n_strx) == 0)
	    break;

    if (!nsyms)
	fe("Symbol %s not found in image.", ss_symname);

    if (!(N_DATADDR(ehdr) <= sym->n_value && sym->n_value < N_BSSADDR(ehdr)))
	fe("Integrity check between symbol value and section failed", 0);
    
    if (lseek(oifd, sym->n_value - N_DATADDR(ehdr) + N_DATOFF(ehdr), 0) < 0)
	fe("Unable to seek to value associated with symbol %s", ss_symname);
    
    if (read(oifd, &ss_offset, 4) != 4)
	fe("Unable to read value associated with symbol %s", ss_symname);
    
    if (ss_offset == 0) {
	long endoff = lseek(oifd, 0, 2);	/* seek to end of file */
	long pgsize = getpagesize();

	if (endoff < 0)
	    fe("Unable to seek to end of file %s", oiname);
	/*
	 * We want to be able to use this code on systems with mmap.  This
	 * means that we must align the data on page boundaries.  The following
	 * line of code sets ss_offset to the next page boundary.
	 */

	ss_offset = round(endoff+4, pgsize);

	/*
	 * For sections at the ends of files, the convention seems to be
	 * to store the size of the section in the first four bytes followed
	 * by the information that we wish to store.  In the following
	 * lines, we write this size information out along with a zero
	 * filled area which will take us to where the saved state information
	 * will live.
	 */

	ss_size = ss_statbuf.st_size + (ss_offset - endoff);
	bcopy(&ss_size, buf, 4);
	bzero(buf+4, ss_offset - endoff - 4);

	if (write(oifd, buf, ss_offset-endoff) < 0)
	    fe("Error writing to %s", oiname);
	
	/*
	 * Update the offset variable in the new image to be the value
	 * for ss_offset that we just computed.  This value being non-
	 * zero will inform the image (when run) that there is a saved
	 * state attached to the image.  It also tells the image where
	 * to find the saved state.
	 */

	if (lseek(oifd, sym->n_value - N_DATADDR(ehdr) + N_DATOFF(ehdr), 0) < 0)
	    fe("Unable to seek to value associated with symbol %s", ss_symname);
	
	if (write(oifd, &ss_offset, 4) != 4)
	    fe("Unable to write new value for symbol %s", ss_symname);
    }


    /*
     * Seek to start of saved state area
     */

    if (lseek(oifd, ss_offset, 0) < 0)
	fe("Unable to seek to start of saved state area in %s", oiname);
    
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
    close (oifd);

    /*
     * Change the mode of the output image file to be the same as the input
     * image file.
     */

    if (stat(iiname, &ss_statbuf) == 0)
	(void) chmod(oiname, ss_statbuf.st_mode);

    exit(0);
}
