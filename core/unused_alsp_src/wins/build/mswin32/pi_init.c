/*=====================================================================*
 |		pi_init.c
 | Copyright (c) 1988-1996, Applied Logic Systems, Inc.
 |
 |	- Foreign interface initialization
 |		- a segment of the old pimain.c
 *=====================================================================*/

#include "pi_init.h"

extern void kernal32_init(void);
extern void user32_init(void);
extern void gdi32_init(void);
extern void mswin32aux_init(void);

void pi_init(void)
{
	kernal32_init();
	user32_init();
	gdi32_init();
	winspool32_init();
	//shell32_init();
	//ole32_init();
	comdlg32_init();
	mswin32aux_init();
}
