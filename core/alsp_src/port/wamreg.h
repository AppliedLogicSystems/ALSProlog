/* for m88k */

#ifdef LargeRegModel
register PWord *mr_E, *mr_SP, *mr_H, *mr_SPB, *mr_HB, *mr_TR, *mr_B;
/*
register Code *mr_FAIL;
*/
#endif

/* for the 386 */

#ifdef SmallRegModel
register PWord *mr_E, *mr_SP, *mr_H;
PWord *mr_SPB, *mr_HB, *mr_TR, *mr_B;
/*
Code *mr_FAIL;
*/
#endif

/* for m68k macines */

#ifdef DataRegModel
register PWord rg_E, rg_SP, rg_H, rg_SPB, rg_TR;
/*
register PWord rg_FAIL;
*/
PWord *mr_B, *mr_HB;

#define mr_E        PWPTR(rg_E)
#define mr_SP       PWPTR(rg_SP)
#define mr_H        PWPTR(rg_H)
#define mr_SPB      PWPTR(rg_SPB)
#define mr_TR       PWPTR(rg_TR)
/*
#define mr_FAIL     ((Code *)rg_FAIL)
*/
#endif

#ifdef NoRegModel
PWord *mr_E, *mr_SP, *mr_H, *mr_SPB, *mr_HB, *mr_TR, *mr_B;
/*
Code *mr_FAIL;
*/
#endif

