/*
 * dos386.h  -- Register macros for system calls
 *
 * Copyright (c) Applied Logic Systems
 *
 * Author : Ilyas Cicekli
 * Date   : 4/18/89
 */

/* Long Word Registers */

struct LONGWORDREGS386 {
   unsigned long eax;
   unsigned long ebx;
   unsigned long ecx;
   unsigned long edx;
   unsigned long ebp;
   unsigned long esi;
   unsigned long edi;
   unsigned long cflag;
   unsigned long eflags;
};

/* Word Registers */

struct WORDREGS386 {
   unsigned short ax,upword1;
   unsigned short bx,upword2;
   unsigned short cx,upword3;
   unsigned short dx,upword4;
   unsigned short bp,upword5;
   unsigned short si,upword6;
   unsigned short di,upword7;
};

/* Byte Registers */

struct BYTEREGS386 {
   unsigned char  al,ah;
   unsigned short upword1;
   unsigned char  bl,bh;
   unsigned short upword2;
   unsigned char  cl,ch;
   unsigned short upword3;
   unsigned char  dl,dh;
   unsigned short upword4;
};


/*
 * General purpose registers union. 
 * Overlays the corresponding long word, word and byte registers.
 */

union REGS386 {
	struct LONGWORDREGS386 l;
	struct WORDREGS386 x;
	struct BYTEREGS386 h;
	};


struct SREGS386 {
	unsigned short es;
	unsigned short cs;
	unsigned short ss;
	unsigned short ds;
};

