/*
 * mach-mics.c	-- Merge Image and saved Code State (mach version)
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 11-30-93
 *
 * This file contains code which will merge mach images and saved (ALS) code
 * states.  A mach image consists of a header and a series of load commands.
 * Following the load commands are data associated with the load commands.
 * These data areas are page aligned for efficient demand paging.
 *
 * The strategy for merging an image and saved code state for this kind of
 * arrangement is to first remove any old load commands previously added
 * by a prolog environment.  Once this is done, new load commands for the
 * saved code state are added.  These load commands for the saved code state
 * will cause the loader to demand load the prolog data from the file at
 * the required virtual addresses.
 *	
 * This is a good arrangment in that the image data need not be modified
 * in order for this to work.  In other implementations for other platforms,
 * we look for a certain symbol in the image and modify the value associated
 * with this symbol so that the image will (at runtime) know where to load
 * the saved code state from.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <mach/mach.h>
#include <mach-o/loader.h>
#include <nlist.h>
#include "alspi.h"
#include "alsmem.h"

/* 
 * MICSBUFSIZE is the size of the buffer used for copying the input image to
 * the output image.
 */

#define MICSBUFSIZE 16384

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
 * copy will copy bytes from ifd, the input file descriptor to ofd, the
 * output file descriptor.  It will first lseek to start in ifd and copy
 * size number of bytes.  Prior to copying, it will page align the output
 * file descriptor's position and zero fill the region left vacant by page
 * alignment.  The return value will be the file postion after the zero
 * fill, but prior to the copy.
 */

long copy(int ifd, int ofd, long start, long size)
{
    long pos, rpos;
    char buf[MICSBUFSIZE];

    pos = lseek(ofd, 0, L_INCR);
    if (pos < 0)
	fe("Error obtaining current position in output file",0);
    rpos = round(pos, vm_page_size);
    if (rpos > pos) {
	bzero(buf, rpos-pos);
	if (write(ofd, buf, rpos-pos) != rpos-pos)
	    fe("Error padding output file",0);
    }

    if (lseek(ifd, start, L_SET) < 0)
	fe("Error seeking to start of input file",0);

    while (size > MICSBUFSIZE) {
	if (read(ifd, buf, MICSBUFSIZE) != MICSBUFSIZE)
	    fe("Error reading from input file",0);
	if (write(ofd, buf, MICSBUFSIZE) != MICSBUFSIZE)
	    fe("Error writing to output file",0);
	size -= MICSBUFSIZE;
    }
    if (size > 0) {
	if (read(ifd, buf, size) != size)
	    fe("Error reading from input file",0);
	if (write(ofd, buf, size) != size)
	    fe("Error writing to output file",0);
    }
    return rpos;
}


/*
 * als_data_name is a character array containing the name of the segment
 * where we put the saved state in the mach object file.
 */

static char als_data_name[] = "als_data";

main(int argc,char **argv)
{
    int iifd, ssfd, oifd;
    long nbytes;
    struct mach_header mh;
    struct am_header ah;
    char *raw_cmds, *rcp;
    union cmdptr_union {
	struct load_command *load;
	struct segment_command *segment;
	struct symtab_command *symtab;
    } *cmds;
    int cmdidx, cmdidx0;
    int ascnt;		/* als segment count */
    struct stat statbuf;


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
    
    /*
     * Step 2:  Open the save state file.
     */
    
    ssfd = open(ssname, O_RDONLY);
    if (ssfd < 0)
	fe("Error opening saved state file %s for read access", ssname);
    nbytes = read(ssfd, &ah, sizeof ah);
    if (nbytes != sizeof ah)
	fe("Error encountered while reading %s", ssname);
    
    /*
     * Step 3:  Read the input image file header and check validity
     */
    
    nbytes = read(iifd, &mh, sizeof mh);
    if (nbytes != sizeof mh)
	fe("Error encountered while reading %s", iiname);
    if (mh.magic != MH_MAGIC || mh.filetype != MH_EXECUTE)
	fe("%s is not executable", iiname);
    
    /*
     * Step 4:  Allocate the raw command block and an array for accessing
     * the various load commands within the block.  We allocate some additional
     * space in the event that we need to attach our own commands to the
     * file.  Also read the commands in.
     */
    
    raw_cmds = malloc(mh.sizeofcmds + 
		      ah.nblocks * sizeof(struct segment_command));
    if (raw_cmds == NULL)
	fe("Error allocating segment area",0);
    
    cmds = malloc((mh.ncmds+ah.nblocks) * sizeof (union segptr_union *));
    if (cmds == NULL)
	fe("Error allocating segment area",0);
    
    nbytes = read(iifd, raw_cmds, mh.sizeofcmds);
    if (nbytes != mh.sizeofcmds)
	fe("Error reading load commands from %s", iiname);
    
    /*
     * Step 5: Set up the commands vector with the original non-ALS load
     * commands.
     */
    
    cmdidx = 0;
    cmdidx0 = 0;
    rcp = raw_cmds;
    while (cmdidx < mh.ncmds) {
	cmds[cmdidx0].load = (struct load_command *) rcp;
	rcp += cmds[cmdidx0].load->cmdsize;
	if (cmds[cmdidx0].load->cmd == LC_SEGMENT &&
	    strcmp(cmds[cmdidx].segment->segname, als_data_name) == 0) {
	    /* skip this segment */
	}
	else
	    cmdidx0++;
	cmdidx++;
    }

    cmdidx = cmdidx0;

    /*
     * Step 6: Add in new commands for loading the saved state.  Note that
     * the fileoff field is set to the place to find the data in the saved
     * state file.  It will be set to the offset in the output image file
     * in step 9.
     */
    
    nbytes = 0;
    for (ascnt=0; ascnt < ah.nblocks; ascnt++) {
	cmds[cmdidx0].load = (struct load_command *) rcp;
	bzero(rcp, sizeof (struct segment_command));
	rcp += sizeof (struct segment_command);
	cmds[cmdidx0].segment->cmd = LC_SEGMENT;
	cmds[cmdidx0].segment->cmdsize = sizeof (struct segment_command);
	strcpy(cmds[cmdidx0].segment->segname, als_data_name);
	cmds[cmdidx0].segment->vmaddr = (unsigned long) ah.blocks[ascnt].start;
	cmds[cmdidx0].segment->vmsize = (unsigned long) ah.blocks[ascnt].asize;
	cmds[cmdidx0].segment->fileoff = nbytes;
	nbytes += ah.blocks[ascnt].fsize;
	cmds[cmdidx0].segment->filesize = ah.blocks[ascnt].fsize;
	cmds[cmdidx0].segment->maxprot = VM_PROT_ALL;
	cmds[cmdidx0].segment->initprot = VM_PROT_ALL;
	cmds[cmdidx0].segment->nsects = 0;
	cmds[cmdidx0].segment->flags = 0;
	cmdidx0++;
    }

    /*
     * Step 7: Compute header information for new image and write the
     * header out.
     */
    
    mh.ncmds = cmdidx0;
    mh.sizeofcmds = 0;

    for (cmdidx0=0; cmdidx0 < mh.ncmds; cmdidx0++)
	mh.sizeofcmds += cmds[cmdidx0].load->cmdsize;
    
    if (write(oifd, &mh, sizeof mh) != sizeof mh)
	fe("Error writing header to %s.",oiname);
    
    /*
     * Step 8: Write out the data associated with the original image load
     * commands.
     */

    if (lseek(oifd, mh.sizeofcmds + sizeof mh, L_SET) < 0)
	fe("Error seeking to start of segment data in %s.",oiname);
    
    for (cmdidx0=0; cmdidx0 < cmdidx; cmdidx0++) {
	switch (cmds[cmdidx0].load->cmd) {
	    case LC_SEGMENT :
		cmds[cmdidx0].segment->fileoff = 
		    copy(iifd, oifd, cmds[cmdidx0].segment->fileoff,
				     cmds[cmdidx0].segment->filesize);
		break;
	    case LC_SYMTAB :
		cmds[cmdidx0].symtab->symoff =
		    copy(iifd, oifd, cmds[cmdidx0].symtab->symoff,
			 cmds[cmdidx0].symtab->nsyms * sizeof (struct nlist));
		cmds[cmdidx0].symtab->stroff =
		    copy(iifd, oifd, cmds[cmdidx0].symtab->stroff,
				     cmds[cmdidx0].symtab->strsize);
		break;
	    default :
		/* Other sections are self contained */
		break;
	}
    }

    /*
     * Step 9: Write out data associated with new ALS load commands.
     */
    
    for ( ; cmdidx0 < mh.ncmds; cmdidx0++)
	cmds[cmdidx0].segment->fileoff = 
	    copy(ssfd, oifd, cmds[cmdidx0].segment->fileoff,
			     cmds[cmdidx0].segment->filesize);

    /*
     * Step 10: Now go back and write out the actual commands since
     * the commands were not complete until the data was written out.
     */

    if (lseek(oifd, sizeof mh, L_SET) < 0)
	fe("Error seeking to end of header in %s",ssname);
    
    for (cmdidx0=0; cmdidx0 < mh.ncmds; cmdidx0++) {
	if (write(oifd, cmds[cmdidx0].load, cmds[cmdidx0].load->cmdsize)
		!= cmds[cmdidx0].load->cmdsize)
	    fe("Error writing load command to %s",ssname);
    }

    /*
     * Step 11: Close the files and exit.  Also change the mode of the output
     * image so that it is the same as the input image.
     */
    
    close(ssfd);
    close(oifd);
    close(iifd);

    if (stat(iiname, &statbuf) == 0)
	(void) chmod(oiname, statbuf.st_mode);

    exit(0);
}
