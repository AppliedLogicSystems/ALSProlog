/*
 * setexp.c  	-- Setting expiration date on the memory key
 *
 * Copyright (c) Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : August 1992
 *
 * Usage: 	setexp
 *			setexp days
 * 			setexp days numofusage
 */


/*******************************************************************************
*                                                                              *
*  MICROPHAR 1989  :   PHAR_LAP  V 2.2 b   / High C Metaware   V 1.5           *
*                                    ----                                      *
*******************************************************************************/

/*******************************************************************************
* these three lines must appear in your source.                                *
*******************************************************************************/

struct pmptr{ int off;short sel;} ;
#define FP_OF(ptr) ((struct pmptr *)&ptr)->off          
#define FP_SL(ptr) ((struct pmptr *)&ptr)->sel          


#define  code_c     9427 
#define  code_ch   (code_c / 256)
#define  code_cl   (code_c % 256)
#define  depl1     0
#define  depl2     0

void decryp();

#include <stdio.h>
#include <time.h>

#define EXPDATE_W1 		0
#define EXPDATE_W2 		1

#define LAST_USAGE_W1 	2
#define LAST_USAGE_W2	3

#define NUMOF_USAGE 	4


void main(argc,argv)
	int argc;
	char *argv[];
{
 	extern void printf();
 	extern unsigned char lecture();
 	extern unsigned char ecriture();
 	unsigned char retour;
 	unsigned char ret1,ret2,ret3,ret4,ret5;
 	unsigned short val,i;
 	unsigned long days;
 	time_t starttime;
 	time_t exptime;
 	unsigned long defaultdays = 30;
 	unsigned short defaultusage = 10000;
	unsigned short usage;


	printf("\nargc=%d ", argc);

	if (argc > 3) {
		fprintf(stderr,"\nUsage: setexp [days] [numofusage] \n");
		exit(1);
	}


	if (argc == 3) {
	printf("\nargv1: %s ", argv[1]);
	printf("\nargv2: %s ", argv[2]);
		if (sscanf(argv[1],"%d",&days) == 0) {
			fprintf(stderr,"\nsetexp: '%s'is incorrect days format \n",argv[1]);
		}
		if (sscanf(argv[2],"%d",&usage) == 0) {
			fprintf(stderr,
				"\nsetexp: '%s'is incorrect numofusage format \n",argv[2]);
		}
	printf("\ndays=%d usage=%d\n", days,usage);
	}
	else if (argc == 2) {
	printf("\nargv1: %s ", argv[1]);
		if (sscanf(argv[1],"%d",&days) == 0) {
			fprintf(stderr,"\nsetexp: '%s'is incorrect days format \n",argv[1]);
		}
		usage = defaultusage;
	}
	else {
		days = defaultdays;
		usage = defaultusage;
	printf("\ndays=%d usage=%d\n", days,usage);
	printf("\ndefaultdays=%d defaultusage=%d\n", defaultdays,defaultusage);
	}

	starttime = time(0);
	exptime = starttime + 86400 * days;
	//exptime = starttime + days;

	printf("\nStart Time: %lx \n", starttime);
	printf("Start Time String: %s \n", asctime(localtime(&starttime)));
	printf("Expiration Time: %lx \n", exptime);
	printf("Expiration Time String: %s \n", asctime(localtime(&exptime)));
	printf("\ndays=%d usage=%d\n", days,usage);

   	ret1 = ecriture(EXPDATE_W1, *((unsigned short *)&exptime));
   	ret2 = ecriture(EXPDATE_W2, *(((unsigned short *)&exptime)+1));

   	ret3 = ecriture(LAST_USAGE_W1, *((unsigned short *)&starttime));
   	ret4 = ecriture(LAST_USAGE_W2, *(((unsigned short *)&starttime)+1));

   	ret5 = ecriture(NUMOF_USAGE, usage);

   	printf("Writing error : ret1=%d ret2=%d ret3=%d ret4=%d ret5=%d \n",
			ret1,ret2,ret3,ret4,ret5);

	exit(0);
}



/******************************************************************************

* Function  to write a word at a location.                                    *
*        - "Adresse" has the value of the location where to write.            *
*        - The range of "Adresse" is beetwen 0 and 30.                        *
*        - "Valeur" is the value of the word to write.                        *
*        - In return you get:                                                 *
*            -0 If all is done well.                                          *
*            -1 If the writing hasn't been done.                              *
*            -2 If the location is out of range.                              *
*            -3 If the key is not present.                                    *
******************************************************************************/

unsigned char ecriture(adresse,valeur)
unsigned char adresse;
unsigned short valeur;
{                                                           
extern void ecrit();
unsigned char cb;
_far char *p   ;


/******************************************************************************
*  The following block must appear often in your source.                      *
*  Its function is to destruct the interrupt vectors used by the debugger.    *
******************************************************************************/                        
				 
 
   FP_SL(p)=0x34;		 
   FP_OF(p)=0x46C    ; cb=*p;			
   FP_OF(p)=7+depl1  ; *p=cb;
   FP_OF(p)=0xD+depl2; *p=cb;

/************************* End Block *****************************************/                        

   decryp();

   FP_OF(p)= 0x46c  ; cb=*p;
   FP_OF(p)= 4+depl1; *p=cb;
   FP_OF(p)= 5+depl1; *p=1 ;

   FP_OF(p)=12+depl2; *p=cb^adresse;         
   FP_OF(p)=0x46C   ; cb=*p        ;

   FP_OF(p)= 6+depl1 ; *p=cb ;
   FP_OF(p)= 13+depl2; *p=cb^(valeur %256);
 
   FP_OF(p)= 0x46C   ; cb=*p ;
   FP_OF(p)= 7+depl1 ; *p=cb ;
   FP_OF(p)= 14+depl2; *p=cb^(valeur /256);


   ecrit();

   FP_OF(p)=4+depl1 ; cb=*p;

   return(cb);
}

/******************************************************************************
* Function  to read a word at a location.                                     *
*        - "Adresse" has the value of the location where to read.             *
*        - The range of "Adresse" is beetwen 0 and 30.                        *
*        - The word is stored in "Valeur".                                    *
*        - In return you get:                                                 *
*            -0 If all is done well.                                          *
*            -2 If the location is out of range.                              *
*            -3 If the key is not present.                                    *
******************************************************************************/

unsigned char lecture(adresse,valeur)
unsigned char adresse;
unsigned short *valeur;
{
extern void ecrit();
unsigned char cb,e6,e7,e13,e14;
_far char *p   ;

/******************************************************************************
*  The following block must appear often in your source.                      *
*  Its function is to destruct the interrupt vectors used by the debugger.    *
******************************************************************************/                        

FP_SL(p) = 0x34;

FP_OF(p) = 0x46C    ; cb=*p;
FP_OF(p) = 7+depl1  ; *p=cb;
FP_OF(p) = 0xD+depl2; *p=cb;

/************************* End Block *****************************************/                        

decryp();
            
FP_OF(p)= 0x46c   ; cb=*p;
FP_OF(p)= 4+depl1 ; *p=cb;		  
FP_OF(p)= 5+depl1 ; *p=2;

FP_OF(p)= 12+depl2; *p=(char)(cb^adresse);
            
FP_OF(p)= 0x46c   ; cb = *p;
FP_OF(p)= 6+depl1 ;	*p = cb;

FP_OF(p)= 13+depl2; *p=cb  ;

FP_OF(p)= 0x46c   ; cb=*p;
FP_OF(p)= 7+depl1 ; cb=*p;

FP_OF(p)= 14+depl2; *p=cb;
            
ecrit();

FP_OF(p)= 6+depl1 ; e6=*p ;
FP_OF(p)= 13+depl2; e13=*p;
FP_OF(p)= 7+depl1 ; e7=*p ;
FP_OF(p)= 14+depl2; e14=*p;

*valeur =  (e6-e13) + (e7-e14)*256;

FP_OF(p)= 4+depl1; cb=*p;

return(cb);
}

void decryp()
{
extern void setup();
unsigned I,fin,ofs;
unsigned selecteur,segment,longueur;            
unsigned char q;			  
 _far unsigned *pu;
 _far short *ps;
 _far char  *pc;

selecteur = 0x34;

setup();					 
			  			 
/*************** offset ecrit***************/

	FP_SL(pu)=selecteur; FP_OF(pu)=4+depl1 ; ofs=*pu ;

/*************** longueur ecrit***************/

	FP_SL(ps)=selecteur; FP_OF(ps)=12+depl1; longueur=*ps;
     fin=ofs+longueur;

/*************** cs selecteur ***************/
     FP_SL(ps)=selecteur; FP_OF(ps)=14+depl2; segment=*ps;
	 

for(FP_SL(pc)=segment,I=ofs;I<=fin;I+=2)
  {
    FP_OF(pc)=I;
    q=*pc;
    q ^=code_ch;
    *pc=q;
    FP_OF(pc)=I+1;q=*pc;	   
    q  ^=code_cl;
    *pc=q;
  };					
}

