/*
 * elf-mics.c	-- Merge Image and saved Code State (ELF version)
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 10-12-93
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libelf.h>


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

static char ss_symname[] = "saved_state_image_offset";


main(int argc,char **argv)
{
    int iifd, ssfd, oifd;
    int nbytes;
    Elf *elf;
    Elf32_Ehdr *ehdr;
    Elf_Scn *scn, *als_scn;
    Elf32_Shdr *shdr, *als_shdr;
    Elf_Data *als_data, *sym_data, *dat_data;
    Elf32_Sym *sym;
    struct stat statbuf;
    char buf[MISSBUFSIZE];

#   define iiname argv[1]
#   define ssname argv[2]
#   define oiname argv[3]


    if (argc != 4) 
	fe("Usage: miss image-name saved-state-name output-image-name", 0);
    
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
    if (fstat(ssfd, &statbuf) < 0)
	fe("Cannot fstat file descriptor associated with %s", ssname);
    
    
    /*
     * Step 3:  Initialize elf library and get the elf header.  Of interest
     * in the elf header is the e_shstrndx field.  This is the section header
     * index of the entry associated with the section name string table. This
     * value is required in calls to elf_strptr to get the names of the
     * sections.
     */
    
    if (elf_version(EV_CURRENT) == EV_NONE)
	fe("Out of date elf library",0);
    
    elf = elf_begin(oifd, ELF_C_RDWR, 0);
    if (elf_kind(elf) != ELF_K_ELF)
	fe("Image file not of right type",0);
    
    ehdr = elf32_getehdr(elf);
    if (ehdr == 0)
	fe("Unable to read elf header",0);
    
    /*
     * Step 4:  Search for the "als_data" section.  This exist only when
     * the input image is itself a saved state.  In this case, we want
     * to replace the old saved state with the new one.
     */
    
    scn = 0;
    while ((scn = elf_nextscn(elf,scn)) != 0) {
        char *name = 0;
        if ((shdr = elf32_getshdr(scn)) != 0) {
	    name = elf_strptr(elf,ehdr->e_shstrndx,shdr->sh_name);
	}
	if (name && strcmp(name,als_data_name) == 0)
	    break;
    }

    /*
     * Step 5:  Create a new section if the "als_data" section was not
     * found.
     */
    
    if (scn) {
	als_scn = scn;
	als_shdr = elf32_getshdr(als_scn);
    }
    else {
	Elf_Data *newname;
	scn = elf_getscn(elf, (size_t)ehdr->e_shstrndx);
	newname = elf_newdata(scn);
	newname->d_buf = als_data_name;
	newname->d_type = ELF_T_BYTE;
	newname->d_size = sizeof als_data_name;
	elf_update(elf, ELF_C_NULL);
	als_scn = elf_newscn(elf);
	als_shdr = elf32_getshdr(als_scn);
	als_shdr->sh_name = newname->d_off;
	als_shdr->sh_type = SHT_LOUSER;
	als_shdr->sh_flags = 0;
	elf_update(elf, ELF_C_NULL);
    }

    /*
     * Step 6:  Install the saved state data in the "als_data" section.
     */
    
    if ((als_data = elf_getdata(als_scn, 0)) == 0)
	als_data = elf_newdata(als_scn);
    als_data->d_buf = malloc(statbuf.st_size);
    als_data->d_type = ELF_T_BYTE;
    als_data->d_size = statbuf.st_size;
    als_data->d_align = sysconf(_SC_PAGESIZE);
    if (statbuf.st_size != read(ssfd,als_data->d_buf,statbuf.st_size))
	fe("Error reading saved state file",0);
    close(ssfd);
    

    /*
     * Step 7:  Search for symbol table section.  What we want to do here
     * is search for the symbol "saved_state_image_offset".  Once we have
     * located this symbol, we use the symbol table information to locate
     * the object the symbol refers to in the image.  This value gets updated
     * with the offset in the output image file to the saved state information.
     */
    
    scn = 0;
    while ((scn = elf_nextscn(elf,scn)) != 0) {
        if ((shdr = elf32_getshdr(scn)) != 0 && shdr->sh_type == SHT_SYMTAB)
	    break;
    }

    if (scn == 0)
	fe("Image file %s contains no symbol table\n",iiname);
    
    sym_data = 0;
    while ( (sym_data = elf_getdata(scn, sym_data)) ) {
	int i,nsyms;
	sym = sym_data->d_buf;
	nsyms = sym_data->d_size / sizeof (Elf32_Sym);
	for (i = 0; i < nsyms; i++, sym++)
	    if (   ELF32_ST_TYPE(sym->st_info) == STT_OBJECT
		&& sym->st_name
		&& !strcmp(elf_strptr(elf,shdr->sh_link,sym->st_name),
			   ss_symname) )
		goto found_sym;
    }

    fe("Could not locate symbol %s", ss_symname);

found_sym:
    
    scn = elf_getscn(elf, sym->st_shndx);
    shdr = elf32_getshdr(scn);
    elf_update(elf, ELF_C_NULL);
    if (!(   shdr->sh_addr <= sym->st_value
	  && sym->st_value < shdr->sh_addr + shdr->sh_size))
	fe("Symbol value failed integrity check\n",0);
    
    dat_data = elf_getdata(scn, 0);
    memcpy((char *)dat_data->d_buf + sym->st_value - shdr->sh_addr,
	   (char *)&als_shdr->sh_offset,
	   sizeof(long));

    elf_flagdata(dat_data, ELF_C_SET, ELF_F_DIRTY);

    /*
     * Step 8:  Truncate the output file and call elf_update to write the
     *		memory image back out to disk.  Close down the elf library
     *		and close the output image file.
     */

    nbytes = elf_update(elf, ELF_C_WRITE);
    if (nbytes < 0)
	fe("Error updating %s.", oiname);
    elf_end(elf);
    if (ftruncate(oifd, nbytes) < 0)
	fe("Error truncating %s.", oiname);
    close(oifd);

    if (stat(iiname, &statbuf) == 0)
	(void) chmod(oiname, statbuf.st_mode);

    exit(0);
}
