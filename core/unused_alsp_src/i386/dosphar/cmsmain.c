#include <stdio.h>
#include "alspi.h"
#include "td.h"

typedef struct vpgdescs {unsigned short o; unsigned char f,t;} vpgdesc;

extern int cms_vptab10();
extern int cms_vptab();
extern int cms_vsnlock();
extern int cms_vmlockr();
extern int cms_vmunlockr();

PI_BEGIN
PI_PDEFINE("$vptab",2,cms_vptab,"_cms_vptab")
PI_PDEFINE("$vptab",10,cms_vptab10,"_cms_vptab10")
PI_PDEFINE("$vsnlock",2,cms_vsnlock,"_cms_vsnlock")
PI_PDEFINE("$vmlockr",2,cms_vmlockr,"_cms_vmlockr")
PI_PDEFINE("$vmunlockr",2,cms_vmunlockr,"_cms_vmunlockr")
PI_END


extern int tfor_shutdown();

PI_main_init()
{
	PI_vminit();
	PI_shutdown = tfor_shutdown;
	PI_INIT
	printf("\nCMS Predicates Initialized \n"); fflush(stdout);
}

PI_main()
{
	PI_toplevel();
}


tfor_shutdown() 
{
	system("del cmsvm.pwf");
}

extern unsigned vsnlock[];
extern void vmlockr();
extern void vmunlockr();


extern unsigned LOWDBASE;
extern unsigned pmbase;

cms_vptab()
{
	PWord a1,a2;
	int   t1,t2;
	int i;
	unsigned long lastpage;

	PI_getan(&a1,&t1,1);
	PI_getan(&a2,&t2,2);

	if ( (t1 != PI_INT || a1 < 0) || 
		 (t1 != PI_INT || a2 < 0 ) )
		PI_FAIL

	/*
	printf("LOWDBASE:%x (%x)  pmbase=%x (%x) vmtop=%x vptab=%x \n", 
		&LOWDBASE,LOWDBASE,&pmbase,pmbase,vmtop,vptab); 
	*/

	i = 0;
	lastpage = vmtop >> 12;
	while (a2-- && a1 < lastpage) {
		if (i%16 == 0) {printf("\n%4x:  ",a1);}
		/*
		printf("(%d,%d,%d) ",vptab[a1].o, vptab[a1].t, vptab[a1].f);
		*/
		printf("%2x ",vptab[a1].t);
		a1++;
		i++;
	}

	PI_SUCCEED
}

cms_vptab10()
{
	PWord a1,a2,a3,a4,a5,a6,a7,a8,a9,a10;
	int   t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
	int i;
	PWord v3,v4,v5,v6,v7,v8,v9,v10;
	unsigned long lastpage;

	PI_getan(&a1,&t1,1);
	PI_getan(&a2,&t2,2);
	PI_getan(&a3,&t3,3);
	PI_getan(&a4,&t4,4);
	PI_getan(&a5,&t5,5);
	PI_getan(&a6,&t6,6);
	PI_getan(&a7,&t7,7);
	PI_getan(&a8,&t8,8);
	PI_getan(&a9,&t9,9);
	PI_getan(&a10,&t10,10);

	if ( (t1 != PI_INT || a1 < 0) || 
		 (t1 != PI_INT || a2 < 0 ) )
		PI_FAIL

	lastpage = vmtop >> 12;
	v3=0; v4=0; v5=0; v6=0; v7=0; v8=0; v9=0; v10=0;
	while (a2-- && a1 < lastpage) {
		switch(vptab[a1].t) {
			case 0x00: v3++; break;
			case 0x40: v4++; break;
			case 0x80: v5++; break;
			case 0x83: v6++; break;
			case 0x84: v7++; break;
			case 0xa0: v8++; break;
			case 0xff: v9++; break;
			default: printf("\nvptab9: Illegal type"); break;
		}
		a1++;
		v10++;
	}

	if (PI_unify(a3,t3,v3,PI_INT) &&
	    PI_unify(a4,t4,v4,PI_INT) &&
	    PI_unify(a5,t5,v5,PI_INT) &&
	    PI_unify(a6,t6,v6,PI_INT) &&
	    PI_unify(a7,t7,v7,PI_INT) &&
	    PI_unify(a8,t8,v8,PI_INT) &&
	    PI_unify(a9,t9,v9,PI_INT) &&
	    PI_unify(a10,t10,v10,PI_INT) )
		PI_SUCCEED
	else
		PI_FAIL

}



cms_vsnlock()
{
	PWord a1,a2;
	int   t1,t2;
	int i;

	PI_getan(&a1,&t1,1);
	PI_getan(&a2,&t2,2);

	if ( (t1 != PI_INT || a1 < 0 || a1 > 0xfff) ||
		 (t1 != PI_INT || a2 < 0 || a2 > 0xfff) ||
		 (a1+a2 > 0x1000) )
		PI_FAIL

	i = 0;
	while (a2--) {
		if (i%20 == 0) {printf("\n%4d:  ",a1);}
		printf("%d ",vsnlock[a1]);
		a1++;
		i++;
	}

	PI_SUCCEED
}

cms_vmlockr()
{
	PWord a1,a2;
	int   t1,t2;

	PI_getan(&a1,&t1,1);
	PI_getan(&a2,&t2,2);

	if ( (t1 != PI_INT || a1 < 0) ||
		 (t2 != PI_INT || a2 < 0) )
		PI_FAIL

	vmlockr((unsigned)a1,(unsigned)a2);

	PI_SUCCEED
}

cms_vmunlockr()
{
	PWord a1,a2;
	int   t1,t2;

	PI_getan(&a1,&t1,1);
	PI_getan(&a2,&t2,2);

	if ( (t1 != PI_INT || a1 < 0) ||
		 (t2 != PI_INT || a2 < 0) )
		PI_FAIL

	vmunlockr((unsigned)a1,(unsigned)a2);

	PI_SUCCEED
}
