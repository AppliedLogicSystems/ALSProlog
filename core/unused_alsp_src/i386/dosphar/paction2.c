/*
 * paction2.c 	-- Protech Memory key stuff
 *
 * Copyright (c) Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : August 1992
 */

#ifdef EXP_DATE
#ifdef als_i386
#ifdef PharLap

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

//#define CHECK_NUMOF_USAGE 	1
//#define CHECK_LAST_USAGE		1


#define EXPDATE_W1 		0
#define EXPDATE_W2 		1

#define LAST_USAGE_W1 	2
#define LAST_USAGE_W2 	3

#define NUMOF_USAGE 	4


paction2()
{
 	extern void printf();
 	extern unsigned char lecture();
 	extern unsigned char ecriture();
 	unsigned char ret1,ret2,ret3,ret4,ret5;
 	unsigned short val,i;
	unsigned short usage;
 	time_t starttime;
 	time_t exptime;
 	time_t t;

	/*
	 * Read expiration date from the memory key
	 */
   	printf("\nReading expiration date at the memory key\n");
   	ret1 = lecture(EXPDATE_W1, ((unsigned short *)&exptime));
   	ret2 = lecture(EXPDATE_W2, (((unsigned short *)&exptime)+1));
	printf("Expiration Time: %lx  ret1=%d  ret2=%d \n", exptime,ret1,ret2);
	printf("Expiration Time String: %s \n", asctime(localtime(&exptime)));

#ifdef CHECK_LAST_USAGE
	/*
	 * Read starting date from the memory key
	 */
   	printf("\nReading starting date at the memory key\n");
   	ret3 = lecture(LAST_USAGE_W1, ((unsigned short *)&starttime));
   	ret4 = lecture(LAST_USAGE_W2, (((unsigned short *)&starttime)+1));
	printf("Last Usage: %lx  ret3=%d  ret4=%d \n", starttime,ret3,ret4);
	printf("Last Usage String: %s \n", asctime(localtime(&starttime)));
#endif /* CHECK_LAST_USAGE */

#ifdef CHECK_NUMOF_USAGE
	/*
	 * Read number of usage from the memory key
	 */
   	printf("\nReading number of usage at the memory key\n");
   	ret5 = lecture(NUMOF_USAGE,&usage);
	printf("Number of Usage: %d  ret5=%d  \n", usage,ret5);
#endif 	/* CHECK_NUMOF_USAGE */

	/* 
	 * Get the current time 
	 */
	t = time(0);
	printf("\nCurrent Time: %lx \n", t);
	printf("Current Time String: %s \n", asctime(localtime(&t)));

	/*
	 * Check expiration date 
	 */
	if ( ret1 != 0 || ret2 != 0 || t > exptime) {
		fprintf(stderr, "\nDemo limit has expired (1) \n");
		als_exit(1);
	}

#ifdef CHECK_LAST_USAGE
	/*
	 * Check last usage 
	 */
	if ( ret3 != 0 || ret4 != 0 || t < starttime ) {
		fprintf(stderr, "\nDemo limit has expired (2) \n");
		als_exit(1);
	}
   	printf("\nWriting current date at the memory key\n");
   	ret3 = ecriture(LAST_USAGE_W1, *((unsigned short *)&t));
   	ret4 = ecriture(LAST_USAGE_W2, *(((unsigned short *)&t)+1));
	if (ret3 != 0 || ret4 != 0) {
		fprintf(stderr, "\nDemo limit has expired (3) \n");
		als_exit(1);
	}
   	printf("Writing error : ret3=%d ret4=%d\n", ret3,ret4);
#endif /* CHECK_LAST_USAGE */

#ifdef CHECK_NUMOF_USAGE
	/*
	 * Check number of usage 
	 */
	if ( ret5 != 0 || usage == 0 ) {
		fprintf(stderr, "\nDemo limit has expired (4) \n");
		als_exit(1);
	}
	usage--;
   	printf("\nWriting number of usage at the memory key\n");
   	ret5 = ecriture(NUMOF_USAGE, usage);
	if (ret5 != 0) {
		fprintf(stderr, "\nDemo limit has expired (5) \n");
		als_exit(1);
	}
   	printf("Writing error : ret5=%d \n", ret5);
#endif 	/* CHECK_NUMOF_USAGE */

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


#endif 	/* PharLap */
#endif 	/* als_i386 */
#endif /* EXP_DATE */

