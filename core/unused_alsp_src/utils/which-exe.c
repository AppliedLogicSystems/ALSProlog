/*
 * which-exe.c		-- determine type of executable format
 *
 * Author: Kevin A. Buettner
 * Created: 6/22/94
 */

#include <stdio.h>

#define BSIZE 256

struct aout_hdr {
    unsigned char dontcare1;
    unsigned char dontcare2;
    unsigned short a_magic;
    unsigned long dontcare3;
    unsigned long dontcare4;
    unsigned long dontcare5;
    unsigned long dontcare6;
    unsigned long dontcare7;
    unsigned long dontcare8;
    unsigned long dontcare9;
};

main(argc, argv)
    int argc;
    char **argv;
{
    FILE *f;
    static unsigned char buf[BSIZE];
    
    f = fopen(argv[argc-1], "rb");
    if (f == NULL) {
    	fprintf(stderr, "which-exe: Couldn't open file: %s\n", argv[argc-1]);
	goto unknown;
    }
    if (fread(buf, 1, BSIZE, f) <= 0) {
    	fprintf(stderr, "which-exe: Couldn't read file: %s\n", argv[argc-1]);
	goto unknown;
    }
    fclose(f);
    
    if (buf[0] == 0x7f && buf[1] == 'E' && buf[2] == 'L' && buf[3] == 'F')
	printf("elf\n");
    else if (((unsigned long *) buf)[0] == 0xfeedface)
	printf("mach\n");
    else if (((struct aout_hdr *)buf)->a_magic == 0407	/* OMAGIC */
          || ((struct aout_hdr *)buf)->a_magic == 0410 /* NMAGIC */
	  || ((struct aout_hdr *)buf)->a_magic == 0413 /* ZMAGIC */)
	printf("aout\n");
    else if (((unsigned short *) buf)[0] == 0504
          || ((unsigned short *) buf)[0] == 0505
          || ((unsigned short *) buf)[0] == 0506		
          || ((unsigned short *) buf)[0] == 0507
          || ((unsigned short *) buf)[0] == 0502
          || ((unsigned short *) buf)[0] == 0503
          || ((unsigned short *) buf)[0] == 0510
          || ((unsigned short *) buf)[0] == 0511
          || ((unsigned short *) buf)[0] == 0512
          || ((unsigned short *) buf)[0] == 0522
          || ((unsigned short *) buf)[0] == 0550
          || ((unsigned short *) buf)[0] == 0551
          || ((unsigned short *) buf)[0] == 0560
          || ((unsigned short *) buf)[0] == 0562
          || ((unsigned short *) buf)[0] == 0561
          || ((unsigned short *) buf)[0] == 0570
          || ((unsigned short *) buf)[0] == 0575
          || ((unsigned short *) buf)[0] == 0520
          || ((unsigned short *) buf)[0] == 0521
          || ((unsigned short *) buf)[0] == 0522
          || ((unsigned short *) buf)[0] == 0554
          || ((unsigned short *) buf)[0] == 0210
          || ((unsigned short *) buf)[0] == 0211
          || ((unsigned short *) buf)[0] == 0555
          || ((unsigned short *) buf)[0] == 0530
          || ((unsigned short *) buf)[0] == 0535
          || ((unsigned short *) buf)[0] == 0531
          || ((unsigned short *) buf)[0] == 0x0160	/* mips... */
          || ((unsigned short *) buf)[0] == 0x0162
          || ((unsigned short *) buf)[0] == 0x6001
          || ((unsigned short *) buf)[0] == 0x6201
          || ((unsigned short *) buf)[0] == 0x0183	/* dec alpha */
	  || ((unsigned short *) buf)[0] == 0x01DF	/* AIX xcoff */
#ifdef MSWIN
	  || ((unsigned short *) buf)[0] == IMAGE_DOS_SIGNATURE
	  || ((unsigned short *) buf)[0] == IMAGE_NT_SIGNATURE
#endif
	  						)
	printf("coff\n");
    else
	printf("unknown\n");

    exit(0);
unknown:
    printf("unknown\n");
    exit(1);
}
