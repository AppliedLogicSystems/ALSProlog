/*
 * doscalls.c       -- MS-DOS system calls which not are not supported by
 *					-- HighC compiler.
 *
 * Copyright (c) Applied Logic Systemes, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 1/24/89
 *
 *		Revised: 1/25/90,  Ilyas Cicekli 	-- to use int86 routine
 *		Revised: 6/13/90,  Ilyas Cicekli 	-- to use int386 routine
 */

#include "config.h"

#ifdef DOS

#include "version.h"
#include <stdio.h> 
#include "dos386.h"


/*
 * get_file_attr
 *
 *    -- get the file attribute of the given file.
 *       If this function can get the attribute of the given file,
 *       it returns the attribute of the file.
 *       Otherwise it returns -1. 
 *
 *    Attribute of the file
 *       bit 0 : read only
 *       bit 1 : hidden
 *       bit 2 : system
 *       bit 5 : archive
 */
get_file_attr(fname)
   	char *fname;
{
	union REGS386 in;

   	/*
     * AH  = 43h   ( function number )
	 * AL  = 00h   ( to get file attribute )
	 * EDX = pointer to zero-terminated file name
	 */
	in.x.ax = (short) 0x4300;
	in.l.edx = (long) fname;

    /*
     * make DOS system call
 	 */
	int386(0x21,&in,&in);

    /*
     * If carry flag is clear, the DOS system call is succeeded.
 	 * Return the file attribute in CX.
	 * Otherwise return -1.
	 */
    if (!(in.l.cflag)) 
        return(in.x.cx);
    else  
        return(-1);
}




long get_file_date_and_time(fname)
    char *fname; 
{
	union REGS386 in;
    FILE  *fp;

	/*
	 * AH  = 57h  (function number)
	 * AL  = 00h  (to get file date and time)
	 * BX  = file handle
	 */
	in.x.ax = (short) 0x5700;
    if ((fp = fopen(fname,"r")) == NULL)
		return(-1);
	in.x.bx = (short) fp->_file;  	/* file handle */

    /*
     * Make DOS system call
     */
	int386(0x21,&in,&in);
    fclose(fp);

    /*
     * Ifthe carry flag is clear, return the date in DX and time in CX.
	 * Otherwise return -1.
	 */
    if (!(in.l.cflag))
        return((long)(((in.x.dx) << 16) | (in.x.cx)));
    else
        return(-1);
}




chdir(path)
    char *path;
{
	union REGS386 in;

    /*
	 * AH = 3b  (function number to set current directory)
     * DX = path name
	 */
	in.h.ah = (char) 0x3b;
	in.l.edx = (long) path;

    /*
     * Make DOS system call
	 */
	int386(0x21,&in,&in);

    /*
     * Ifthe carry flag is clear, return success(0) 
	 * Otherwise return -1.
	 */
    if (!(in.l.cflag))
        return(0);
    else
        return(-1);
}



char *getcwd(buf,drive)
	char *buf;
	int  drive;
{
	union REGS386 in;
	struct SREGS386 seg;

	/*
	 * Read original segment registers
	 */
	segread386(&seg);

	/*
	 * AH = 047h  to get current directory
	 * DL = drive number (0=defualt 1=A:, etc)
	 * DS:ESI = pointer to 64-byte buffer in which to store directory name
	 */
	in.h.ah = (char) 0x047;
	in.h.dl = (char) drive;
	in.l.esi = (long) buf;

    /*
     * Make DOS system call
	 */
	int386x(0x21,&in,&in,&seg);
	
	/*
	 * If the carry flag is clear, return success (buffer pointer)
	 * Otherwise return (char *) 0 for failure
	 */
	if (!(in.l.cflag))
		return(buf);
	else
		return((char *) 0);
}



lock_or_unlock_page(lockflag,addr,size,segment)
	int 	lockflag;		/* Lock or Unlock 0: Unlock otherwise lock */
	char 	*addr;			/* Starting address of memory area 	*/
	int 	size;			/* Size of memory area 				*/
	int 	segment;		/* Segment of memory area 0:DS otherwise CS */ 
{
	union REGS386 in;
	struct SREGS386 seg;
	char 	*endaddr;
	int 	numofpages;

	/*
	 * How many page will be locked (unlocked) ?
	 */
	endaddr = addr + size;
	addr = (char *) ((long)addr & 0xFFFFF000);
	endaddr = (char *) ((long)endaddr & 0xFFFFF000);
	numofpages = (((long)(endaddr - addr)) >> 12) + 1;

	/*
	 * Read original segment registers
	 */
	segread386(&seg);

#ifdef PharLap
	/*
	 * AX = 251Ah  to lock pages in memory 
	 * AX = 251Bh  to unlock pages in memory 
	 * BL = 1  ==> ES:ECX pointer to first page to lock(unlock)
	 * EDX = number of pages to be locked (unlocked)
	 */
	if (lockflag)
		in.x.ax = (short) 0x251A;
	else
		in.x.ax = (short) 0x251B;
	in.l.edx = (long) numofpages;
	in.h.bl = (char) 1;
	in.l.ecx = (long) addr;
	if (segment) 
		seg.es = seg.cs;
	else 
		seg.es = seg.ds;

    /*
     * Make DOS system call
	 */
	int386x(0x21,&in,&in,&seg);
	
	/*
	 * Return carry flag (clear if success; set if failure)
	 */
	return((int)(in.l.cflag));

#endif 	/* PharLap */

#ifdef 	Ergo
	/*
	 * AX = EB06h  to lock pages in memory 
	 * AX = EB07h  to unlock pages in memory 
	 * DS:EDX pointer to first page to lock(unlock)
	 * CX = number of pages to be locked (unlocked)
	 */
	if (lockflag)
		in.x.ax = (short) 0xEB06;
	else
		in.x.ax = (short) 0xEB07;
	in.x.cx = (long) numofpages;
	in.l.edx = (long) addr;
	if (segment) 
		seg.ds = seg.cs;

    /*
     * Make DOS system call
	 */
	int386x(0x21,&in,&in,&seg);
	
	/*
	 * Return carry flag (clear if success; set if failure)
	 */
	return((int)(in.l.cflag));

#endif 	/* Ergo */

}


/*
 * If the virtual memory subsystem is present returns 1;
 * otherwise (not present) returns 0.
 */
vm_subsystem_present()
{
#ifdef PharLap
	char mem_stats[128];
	get_memory_statistics(mem_stats,0);
	if (*(long *)&mem_stats[0] == 1)
		return(1); 		/* present */
	else
		return(0); 		/* not present */
#endif /* PharLap */

#ifdef Ergo
	/* Assume that, Virtual Memory subsystem is always present */
	return(1);
#endif /* Ergo */
}



#ifdef PharLap

get_memory_statistics(buf,reset_flag)
	char 	*buf;			/* pointer to buffer at least 100 bytes in size */
	int 	reset_flag; 	/* 0: don't reset VM statistics  1: reset them 	*/
{
	union REGS386 in;

	/*
	 * AX = 2520h  to lock pages in memory 
	 * BL = 0  ==> Don't reset VM statistics
	 * 	  = 1  ==> Reset VM statistics
	 * DS:EDX = pointer to buffer at least 100 bytes in size
	 */
	in.x.ax = (short) 0x2520;
	in.l.edx = (long) buf;
	if (reset_flag == 0)
		in.h.bl = (char) 0;
	else
		in.h.bl = (char) 1;

    /*
     * Make DOS system call
	 */
	int386(0x21,&in,&in);
	
#ifdef STAT_DEBUG
{
	long *p;
	printf("\nMemory Statistics: ");
	for(p=(long *)buf; p < (long *)(buf+100); ) {
		printf("\n%02x: %08x %08x %08x %08x %08x %08x %08x %08x",
				(int)(((char *)p)-buf),
				*p,*(p+1),*(p+2),*(p+3),*(p+4),*(p+5),*(p+6),*(p+7));
		p += 8;
	}
	printf("\n");
}
#endif /* STAT_DEBUG */

	return(0);
}

#endif 	/* PharLap */



/*
 * Check the dos extender
 */
check_dos_extender()
{
	union REGS386 in;

	/*
	 * Get MS-DOS and DOS-Extender version
	 */
	in.l.eax = (long) 0x00003000;
	int386((short)0x21,&in,&in);
	/* 
	 * EAX<16-31> == "DX" implies that we are running PharLap Dos extender 
	 */
#ifdef PharLap
	if (((in.l.eax) & (long) 0xFFFF0000) != (long) 0x44580000) {
		fprintf(stderr,
				"\nThis image (.EXP) file is intended to be used ");
		fprintf(stderr,
				"with PharLap DOS extender.");
		return(0);
	}
#else 	/* PharLap */
	if (((in.l.eax) & (long) 0xFFFF0000) == (long) 0x44580000) {
		fprintf(stderr,
				"\nThis image (.EXP) file is intended to be used ");
		fprintf(stderr,
				"with Ergo DOS extender.");
		return(0);
	}
#endif 	/* PharLap */

	return(1);
}



/*
 * Create a code window in the data segment (only in Ergo Environment)
 */
create_code_window()
{
#ifdef Ergo

	extern unsigned short xseg_call_selector;
	union REGS386 in;
	struct SREGS386 seg;

	/*
	 * Read original segment registers
	 */
	segread386(&seg);

	/*
	 * AH = ED  to get segment information
	 * BX = Selector of the segment 
	 *
	 * Outputs:
	 * 		AL = Type
	 * 		CX:DX = length
	 * 		SI:BX = base
	 * 		DI = parent selector
	 */
	in.h.ah = (unsigned char) 0xED;
	in.x.bx = (unsigned short) seg.ds;
	int386(0x21,&in,&in);

	if (in.l.cflag) {
		fprintf(stderr,
				"\nError: Unable to get segment information of segment %04x ",
				seg.ds);
		return(0);
	}

	/*
	 * AX = E801 to create data window
	 * CX:DX = length
	 * SI:BX = base
	 * DS = parent segment
	 *
	 * Outputs:
	 * 		AX = Selector of data window
	 */
	in.x.ax = (unsigned short) 0xE801;
	in.x.si = (unsigned short) 0; 	/* Base address of data window is zero */
	in.x.bx = (unsigned short) 0;
	/* CX:DX contains size of data window which is same as its parent */
	int386x(0x21,&in,&in,&seg);

	if (in.l.cflag) {
		fprintf(stderr, "\nError: Unable to create data window");
		return(0);
	}

	xseg_call_selector = (unsigned short) in.x.ax;

	/*
 	 * AX = E901  to make it code window
	 * BX = Selector
	 */
	in.x.ax = (unsigned short) 0xE901;
	in.x.bx = xseg_call_selector;
	int386(0x21,&in,&in);
	
	if (in.l.cflag) {
		fprintf(stderr, 
				"\nError: Unable to convert data window into code window");
		return(0);
	}

#endif 	/* Ergo */

	return(1);
}


/*
#define SEG_DEBUG 1
*/


/*
 * Refresh the code window (only in Ergo Environment)
 * (make its size same as its parent)
 */
refresh_code_window()
{
#ifdef Ergo

	extern unsigned short xseg_call_selector;
	union REGS386 in;
	struct SREGS386 seg;

	/*
	 * Read original segment registers
	 */
	segread386(&seg);

	/*
	 * AH = ED  to get segment information
	 * BX = Selector of the segment 
	 *
	 * Outputs:
	 * 		AL = Type
	 * 		CX:DX = length
	 * 		SI:BX = base
	 * 		DI = parent selector
	 */
	in.h.ah = (unsigned char) 0xED;
	in.x.bx = (unsigned short) seg.ds;
	int386(0x21,&in,&in);

	if (in.l.cflag) {
		fprintf(stderr,
				"\nError: Unable to get segment information of segment %04x ",
				seg.ds);
		return(0);
	}

#ifdef SEG_DEBUG
	{
		unsigned char  type;
		unsigned long  length;
		unsigned long  base;
		unsigned short parent;

		_ergo_get_segment_info(0xF,&type,&length,&base,&parent);
		_ergo_get_segment_info(0x17,&type,&length,&base,&parent);
		_ergo_get_segment_info(xseg_call_selector,&type,&length,&base,&parent);
	}
#endif /* SEG_DEBUG */

	/*
	 * AX = E905 to change size of code window
	 * CX:DX = length
	 * BX = selector
	 */
	in.x.ax = (unsigned short) 0xE905;
	in.x.bx = xseg_call_selector;
	/* CX:DX contains size of data window which is same as its parent */
	int386(0x21,&in,&in);

	if (in.l.cflag) {
		fprintf(stderr, "\nError: Unable to adjust code window");
		return(0);
	}

#ifdef SEG_DEBUG
	{
		unsigned char  type;
		unsigned long  length;
		unsigned long  base;
		unsigned short parent;

		_ergo_get_segment_info(xseg_call_selector,&type,&length,&base,&parent);
	}
#endif /* SEG_DEBUG */

#endif /* Ergo */

	return(1);

}


#ifdef Ergo
#ifdef SEG_DEBUG

_ergo_get_segment_info(Selector,Type,Length,BaseAddr,Parent)
	unsigned short Selector;
	unsigned char  *Type;
	unsigned long  *Length;
	unsigned long  *BaseAddr;
	unsigned short *Parent;
{
	union REGS386 in;

	/*
	 * AH = ED  to get segment information
	 * BX = Selector of the segment 
	 *
	 * Outputs:
	 * 		AL = Type
	 * 		CX:DX = length
	 * 		SI:BX = base
	 * 		DI = parent selector
	 */
	in.h.ah = (unsigned char) 0xED;
	in.x.bx = (unsigned short) Selector;
	int386(0x21,&in,&in);

	if (in.l.cflag) {
		fprintf(stderr,
				"\nError: Unable to get segment information of segment %04x ",
				Selector);
		return(0);
	}

	*Type = (unsigned char) in.h.al;
	*Length = (((unsigned long)in.x.cx)<<16) | ((unsigned long)in.x.dx);
	*BaseAddr = (((unsigned long)in.x.si)<<16) | ((unsigned long)in.x.bx);
	*Parent = (unsigned short)in.x.di;

	printf("\nSelector:%04x  Type:%02x  Length:%08x  Base:%08x  Parent:%04x",
		Selector,*Type,*Length,*BaseAddr,*Parent);
	fflush(stdout);

	return(1);
}

#endif /* SEG_DEBUG */
#endif /* Ergo */

#endif 	/* DOS */

