#include "defs.h"

#ifdef INTCONSTR
#include "intrv.h"
#include "freeze.h"     
#include "intrv_pr.h"
	/*--- Vars for args to the primitives: ---*/
extern double zl,zh,xl,xh,yl,yh,ul,uh,vl,vh;
extern int status;
/*================================================================
          intrv.c
          --Generated from: pseudoc.ode
          Date: 95/12/8   Time: 5:49:2
		  -- by genC_ie(make_C)

    Interval Primitives: May 3 95 Version of make_C_interval_engine
 *===============================================================*/

     /*----------------*
      |   i_unequal 
      *----------------*/

 int	i_unequal		(void);

int
i_unequal()
{
		/* Macro version of call: unequal(zl,zh,xl,xh)  */
	if (zh GT xl)  goto u_B;
	if (zh LT xl)  goto u_Done;
	if (zl GT zh)  goto u_Fail;
	if (zl LT zh)  goto u_Eq12;
	xl = xl;
	next(xl);
	xlchng();
	goto u_Done;
   u_Eq12:
	if (xl GT xh)  goto u_Fail;
	if (xl LT xh)  goto u_Exit;
	zh = zh;
	prev(zh);
	zhchng();
	goto u_Done;
   u_B:
	if (xh GT zl)  goto u_Exit;
	if (xh LT zl)  goto u_Done;
	if (zl GT zh)  goto u_Fail;
	if (zl LT zh)  goto u_Eq22;
	xh = xh;
	prev(xh);
	xhchng();
	goto u_Done;
   u_Eq22:
	if (xl GT xh)  goto u_Fail;
	if (xl LT xh)  goto u_Exit;
	zl = zl;
	next(zl);
	zlchng();
	goto u_Done;
   u_Fail:
	FAIL;
	goto u_Exit;
   u_Done:
	deact();
	goto u_Exit;
   u_Exit:
	return(0);
}

     /*----------------*
      |   i_equal 
      *----------------*/

 int	i_equal		(void);

int
i_equal()
{
		/* Macro version of call: equal(zl,zh,xl,xh)  */
	if (xl GT zl)  goto e_Chkzl;
	if (xl EQ zl)  goto e_Ub;
	xl = zl;
	xlchng();
	goto e_Ub;
   e_Chkzl:
	zl = xl;
	zlchng();
   e_Ub:
	if (xh GT zh)  goto e_Chkxh;
	if (xh EQ zh)  goto e_Exit;
	zh = xh;
	zhchng();
	goto e_Exit;
   e_Chkxh:
	xh = zh;
	xhchng();
	goto e_Exit;
   e_Exit:
	return(0);
}

     /*----------------*
      |   i_greatereq 
      *----------------*/

 int	i_greatereq		(void);

int
i_greatereq()
{
		/* Macro version of call: greatereq(zl,zh,xl,xh)  */
	if (xh LT zl)  goto g_Done;
	if (xl LE zl)  goto g_Ub;
	zl = xl;
	zlchng();
   g_Ub:
	if (xh LE zh)  goto g_Exit;
	xh = zh;
	xhchng();
	goto g_Exit;
   g_Done:
	deact();
	goto g_Exit;
   g_Exit:
	return(0);
}

     /*----------------*
      |   i_higher 
      *----------------*/

 int	i_higher		(void);

int
i_higher()
{
		/* Macro version of call: higher(zl,zh,xl,xh)  */
	if (zh LT xl)  goto h_Done;
	if (zl LT xl)  goto h_Ub;
	xl = zl;
	next(xl);
	xlchng();
   h_Ub:
	if (zh LT xh)  goto h_Exit;
	zh = xh;
	prev(zh);
	zhchng();
	goto h_Exit;
   h_Done:
	deact();
	goto h_Exit;
   h_Exit:
	return(0);
}

     /*----------------*
      |   i_add 
      *----------------*/

 int	i_add		(void);

int
i_add()
{
	lowerbd(xl+yl,vl);
	if (vl GT zl)  goto a_MUZL;
	if (vl EQ zl)  goto a_Upper;
	lowerbd(zl-yh,vl);
	if (vl LE xl)  goto a_BackLY;
	xl = vl;
	xlchng();
   a_BackLY:
	lowerbd(zl-xh,vl);
	if (vl LE yl)  goto a_Upper;
	yl = vl;
	ylchng();
	goto a_Upper;
   a_MUZL:
	zl = vl;
	zlchng();
   a_Upper:
	upperbd(xh+yh,vh);
	if (vh LT zh)  goto a_MUZH;
	if (vh EQ zh)  goto a_Exit;
	upperbd(zh-yl,vh);
	if (vh GE xh)  goto a_BackHY;
	xh = vh;
	xhchng();
   a_BackHY:
	upperbd(zh-xl,vh);
	if (vh GE yh)  goto a_Exit;
	yh = vh;
	yhchng();
	goto a_Exit;
   a_MUZH:
	zh = vh;
	zhchng();
	goto a_Exit;
   a_Exit:
	return(0);
}

     /*----------------*
      |   i_begin_tog 
      *----------------*/

 int	i_begin_tog		(void);

int
i_begin_tog()
{
	if (xl GT zl)  goto b_Chkzl;
	if (xl EQ zl)  goto b_Exit;
	xl = zl;
	xlchng();
	goto b_Exit;
   b_Chkzl:
	zl = xl;
	zlchng();
	goto b_Exit;
   b_Exit:
	return(0);
}

     /*----------------*
      |   i_cos 
      *----------------*/

 int	i_cos		(void);

int
i_cos()
{
	if (xl GE 0)  goto c_Right;
	if (xh GT 0)  goto c_Center;
	swap(x);
   c_Right:
	upperbd2(cos(xl),uh);
	if (uh GT zh)  goto c_Uxl;
	if (uh EQ zh)  goto c_Low;
	zh = uh;
	zhchng();
	goto c_Low;
   c_Uxl:
	lowerbd2(acos(zh),vl);
	if (vl LE xl)  goto c_Low;
	xl = vl;
	xlchng();
   c_Low:
	lowerbd2(cos(xh),ul);
	if (ul LT zl)  goto c_Uxh;
	if (ul EQ zl)  goto c_Exit;
	zl = ul;
	zlchng();
	goto c_Exit;
   c_Uxh:
	upperbd2(acos(zl),vh);
	if (vh GE xh)  goto c_Exit;
	xh = vh;
	xhchng();
	goto c_Exit;
   c_Center:
	ul = -xl;
	if (xh GE ul)  goto c_Hright;
	swap(x);
   c_Hright:
	lowerbd2(cos(xl),ul);
	upperbd2(cos(xh),uh);
	vl = 1;
	if (zh LT uh)  goto c_Fail;
	if (zh GE ul)  goto c_Chklo;
	lowerbd2(acos(zh),vl);
	if (vl LE xl)  goto c_Chklo;
	xl = vl;
	xlchng();
   c_Chklo:
	if (zl GT uh)  goto c_Uxh;
	if (zl EQ uh)  goto c_Exit;
	zl = uh;
	zlchng();
	goto c_Exit;
   c_Fail:
	FAIL;
   c_Exit:
	return(0);
}

     /*----------------*
      |   i_finish_tog 
      *----------------*/

 int	i_finish_tog		(void);

int
i_finish_tog()
{
	if (xh GT zh)  goto f_Chkxh;
	if (xh EQ zh)  goto f_Exit;
	zh = xh;
	zhchng();
	goto f_Exit;
   f_Chkxh:
	xh = zh;
	xhchng();
	goto f_Exit;
   f_Exit:
	return(0);
}

     /*----------------*
      |   i_inf 
      *----------------*/

 int	i_inf		(void);

int
i_inf()
{
	if (zl LE xl)  goto i_Ubzx;
	xl = zl;
	xlchng();
   i_Ubzx:
	if (zh LE xh)  goto i_LtY;
	zh = xh;
	zhchng();
   i_LtY:
	if (zl LE yl)  goto i_Ubzy;
	yl = zl;
	ylchng();
   i_Ubzy:
	if (zh LE yh)  goto i_Ubx;
	zh = yh;
	zhchng();
   i_Ubx:
	if (zh GE yl)  goto i_Uby;
	if (zh GE xh)  goto i_LbZ;
	xh = zh;
	xhchng();
	goto i_LbZ;
   i_Uby:
	if (zh GE xl)  goto i_LbZ;
	if (zh GE yh)  goto i_LbZ;
	yh = zh;
	yhchng();
   i_LbZ:
	if (xl GT yl)  goto i_Usey;
	vl = xl;
	goto i_Chkzl;
   i_Usey:
	vl = yl;
   i_Chkzl:
	if (vl LE zl)  goto i_Exit;
	zl = vl;
	zlchng();
	goto i_Exit;
   i_Exit:
	return(0);
}

     /*----------------*
      |   i_j_less 
      *----------------*/

 int	i_j_less		(void);

int
i_j_less()
{
	if (yl GT 0) {
		/* Macro version of call: greatereq(zl,zh,xl,xh)  */
	if (xh LT zl)  goto g_j_Done;
	if (xl LE zl)  goto g_j_Ub;
	zl = xl;
	zlchng();
   g_j_Ub:
	if (xh LE zh)  goto g_j_Exit;
	xh = zh;
	xhchng();
	goto g_j_Exit;
   g_j_Done:
	deact();
	goto g_j_Exit;
   g_j_Exit:
	; }
	if (yl LT 0)  iaerror();
	if (yh LT 0)  iaerror();
	if (yh EQ 0) {
		/* Macro version of call: higher(zl,zh,xl,xh)  */
	if (zh LT xl)  goto h_j_Done;
	if (zl LT xl)  goto h_j_Ub;
	xl = zl;
	next(xl);
	xlchng();
   h_j_Ub:
	if (zh LT xh)  goto h_j_Exit;
	zh = xh;
	prev(zh);
	zhchng();
	goto h_j_Exit;
   h_j_Done:
	deact();
	goto h_j_Exit;
   h_j_Exit:
	; }
	if (xh LE zl)  goto j_True;
	if (zh LT xl)  goto j_False;
	goto j_Exit;
   j_True:
	yl = 1;
	ylchng();
	goto j_Exit;
   j_False:
	yh = 0;
	yhchng();
	goto j_Exit;
   j_Exit:
	return(0);
}

     /*----------------*
      |   i_k_equal 
      *----------------*/

 int	i_k_equal		(void);

int
i_k_equal()
{
	if (yl GT 0) {
		/* Macro version of call: equal(zl,zh,xl,xh)  */
	if (xl GT zl)  goto e_k_Chkzl;
	if (xl EQ zl)  goto e_k_Ub;
	xl = zl;
	xlchng();
	goto e_k_Ub;
   e_k_Chkzl:
	zl = xl;
	zlchng();
   e_k_Ub:
	if (xh GT zh)  goto e_k_Chkxh;
	if (xh EQ zh)  goto e_k_Exit;
	zh = xh;
	zhchng();
	goto e_k_Exit;
   e_k_Chkxh:
	xh = zh;
	xhchng();
	goto e_k_Exit;
   e_k_Exit:
	; }
	if (yl LT 0)  iaerror();
	if (yh LT 0)  iaerror();
	if (yh EQ 0) {
		/* Macro version of call: unequal(zl,zh,xl,xh)  */
	if (zh GT xl)  goto u_k_B;
	if (zh LT xl)  goto u_k_Done;
	if (zl GT zh)  goto u_k_Fail;
	if (zl LT zh)  goto u_k_Eq12;
	xl = xl;
	next(xl);
	xlchng();
	goto u_k_Done;
   u_k_Eq12:
	if (xl GT xh)  goto u_k_Fail;
	if (xl LT xh)  goto u_k_Exit;
	zh = zh;
	prev(zh);
	zhchng();
	goto u_k_Done;
   u_k_B:
	if (xh GT zl)  goto u_k_Exit;
	if (xh LT zl)  goto u_k_Done;
	if (zl GT zh)  goto u_k_Fail;
	if (zl LT zh)  goto u_k_Eq22;
	xh = xh;
	prev(xh);
	xhchng();
	goto u_k_Done;
   u_k_Eq22:
	if (xl GT xh)  goto u_k_Fail;
	if (xl LT xh)  goto u_k_Exit;
	zl = zl;
	next(zl);
	zlchng();
	goto u_k_Done;
   u_k_Fail:
	FAIL;
	goto u_k_Exit;
   u_k_Done:
	deact();
	goto u_k_Exit;
   u_k_Exit:
	; }
	if (zl GT xh)  goto k_Un;
	if (zl EQ xh)  goto k_Tryeq;
	if (xl LE zh)  goto k_Exit;
   k_Un:
	yh = 0;
	yhchng();
	goto k_Exit;
   k_Tryeq:
	if (zl NE zh)  goto k_Exit;
	if (xl NE xh)  goto k_Exit;
	yl = 1;
	ylchng();
	goto k_Exit;
   k_Exit:
	return(0);
}

     /*----------------*
      |   i_lub 
      *----------------*/

 int	i_lub		(void);

int
i_lub()
{
	if (xl LE zl)  goto l_Ubx;
	zl = xl;
	zlchng();
   l_Ubx:
	if (xh LE zh)  goto l_LtY;
	xh = zh;
	xhchng();
   l_LtY:
	if (yl LE zl)  goto l_Uby;
	zl = yl;
	zlchng();
   l_Uby:
	if (yh LE zh)  goto l_Lbx;
	yh = zh;
	yhchng();
   l_Lbx:
	if (zl LE xh)  goto l_Lby;
	if (zl LE yl)  goto l_UbZ;
	yl = zl;
	ylchng();
	goto l_UbZ;
   l_Lby:
	if (zl LE yh)  goto l_UbZ;
	if (zl LE xl)  goto l_UbZ;
	xl = zl;
	xlchng();
   l_UbZ:
	if (yh GT xh)  goto l_Usey;
	vh = xh;
	goto l_Chkzh;
   l_Usey:
	vh = yh;
   l_Chkzh:
	if (vh GE zh)  goto l_Exit;
	zh = vh;
	zhchng();
	goto l_Exit;
   l_Exit:
	return(0);
}

     /*----------------*
      |   i_mul 
      *----------------*/

 int	i_mul		(void);

int
i_mul()
{
	if (xl GT 0)  goto m_Xpos;
	if (xl EQ 0)  goto m_XMpos;
	if (xh GT 0)  goto m_Xindef;
	swap(x);
	if (yl GT 0)  goto m_Zneg;
	if (yl EQ 0)  goto m_ZMneg;
	if (yh GT 0)  goto m_Yindef1;
	swap(y);
	goto m_Zpos;
   m_Yindef1:
	if (zl GE 0)  goto m_MakeYneg;
	if (zh GT 0)  goto m_FZ;
	swap(z);
	goto m_MakeYpos;
   m_FZ:
	swap(z);
	goto m_MpyBX;
   m_ZMneg:
	if (yh LT 0)  goto m_Fail;
	if (yh GT 0)  goto m_Zneg;
	goto m_ZZero;
   m_Xindef:
	if (yl GT 0)  goto m_Ypos;
	if (yl EQ 0)  goto m_YMpos;
	if (yh GT 0)  goto m_MpyC;
	swap(y);
	if (zl GE 0)  goto m_MakeXneg;
	if (zh GT 0)  goto m_FZ2;
	swap(z);
	goto m_MakeXpos;
   m_FZ2:
	swap(z);
	goto m_MpyBY;
   m_YMpos:
	if (yh LT 0)  goto m_Fail;
	if (yh EQ 0)  goto m_ZZero;
   m_Ypos:
	if (zl GE 0)  goto m_MakeXpos;
	if (zh GT 0)  goto m_MpyBY;
	swap(z);
	goto m_MakeXneg;
   m_XMpos:
	if (xh LT 0)  goto m_Fail;
	if (xh EQ 0)  goto m_ZZero;
   m_Xpos:
	if (yl GT 0)  goto m_Zpos;
	if (yl EQ 0)  goto m_ZMpos;
	if (yh GT 0)  goto m_Yindef;
	swap(y);
	goto m_Zneg;
   m_Yindef:
	if (zl GE 0)  goto m_MakeYpos;
	if (zh GT 0)  goto m_MpyBX;
	swap(z);
	goto m_MakeYneg;
   m_ZMpos:
	if (yh LT 0)  goto m_Fail;
	if (yh GT 0)  goto m_Zpos;
	goto m_ZZero;
   m_MpyC:
	lowerbd(xl*yh,vh);
	lowerbd(xh*yl,vl);
	if (vh LE vl)  goto m_Usevh;
	vh = vl;
   m_Usevh:
	if (vh LE zl)  goto m_MpyCH;
	zl = vh;
	zlchng();
   m_MpyCH:
	upperbd(xl*yl,vl);
	upperbd(xh*yh,vh);
	if (vl GE vh)  goto m_Usevl2;
	vl = vh;
   m_Usevl2:
	if (vl GE zh)  goto m_Exit;
	zh = vl;
	zhchng();
	goto m_Exit;
   m_MpyBX:
	lowerbd(xh*yl,vl);
	if (vl LT zl)  goto m_Backxzl;
	if (vl EQ zl)  goto m_MpyBHX;
	zl = vl;
	zlchng();
	goto m_MpyBHX;
   m_Backxzl:
	if (xl LE 0)  goto m_MpyBHX;
	lowerbd(zl/xl,vl);
	if (vl LE yl)  goto m_MpyBHX;
	yl = vl;
	ylchng();
   m_MpyBHX:
	upperbd(xh*yh,vh);
	if (vh LT zh)  goto m_MUZH;
	if (vh EQ zh)  goto m_Exit;
	if (xl LE 0)  goto m_Exit;
	upperbd(zh/xl,vh);
	if (vh LT yh)  goto m_MUYH;
	goto m_Exit;
   m_MpyBY:
	lowerbd(yh*xl,vl);
	if (vl LT zl)  goto m_Backyzl;
	if (vl EQ zl)  goto m_MpyBHY;
	zl = vl;
	zlchng();
	goto m_MpyBHY;
   m_Backyzl:
	if (yl LE 0)  goto m_MpyBHY;
	lowerbd(zl/yl,vl);
	if (vl LE xl)  goto m_MpyBHY;
	xl = vl;
	xlchng();
   m_MpyBHY:
	upperbd(xh*yh,vh);
	if (vh LT zh)  goto m_MUZH;
	if (vh EQ zh)  goto m_Exit;
	if (yl LE 0)  goto m_Exit;
	upperbd(zh/yl,vh);
	if (vh LT xh)  goto m_MUXH;
	goto m_Exit;
   m_ZZero:
	if (zl GT 0)  goto m_Fail;
	if (zl EQ 0)  goto m_Zh;
	zl = 0;
	zlchng();
   m_Zh:
	if (zh LT 0)  goto m_Fail;
	if (zh EQ 0)  goto m_Exit;
	zh = 0;
	zhchng();
	goto m_Exit;
   m_MakeXneg:
	swap(x);
   m_MakeXpos:
	xl = 0;
	xlchng();
	goto m_MpyA;
   m_MakeYneg:
	swap(y);
   m_MakeYpos:
	yl = 0;
	ylchng();
	goto m_MpyA;
   m_Zneg:
	if (zh LE 0)  goto m_Swz;
	swap(z);
	zl = 0;
	zlchng();
	goto m_MpyA;
   m_Swz:
	swap(z);
	goto m_MpyA;
   m_Zpos:
	if (zl GE 0)  goto m_MpyA;
	zl = 0;
	zlchng();
   m_MpyA:
	lowerbd(xl*yl,vl);
	if (vl GT zl)  goto m_MUZL;
	if (vl EQ zl)  goto m_Upper;
	if (yh LE 0)  goto m_BackLY;
	lowerbd(zl/yh,vl);
	if (vl LE xl)  goto m_BackLY;
	xl = vl;
	xlchng();
   m_BackLY:
	if (xh LE 0)  goto m_Upper;
	lowerbd(zl/xh,vl);
	if (vl LE yl)  goto m_Upper;
	yl = vl;
	ylchng();
	goto m_Upper;
   m_MUZL:
	zl = vl;
	zlchng();
   m_Upper:
	upperbd(xh*yh,vh);
	if (vh LT zh)  goto m_MUZH;
	if (vh EQ zh)  goto m_Exit;
	if (yl LE 0)  goto m_BackHY;
	upperbd(zh/yl,vh);
	if (vh GE xh)  goto m_BackHY;
   m_MUXH:
	xh = vh;
	xhchng();
   m_BackHY:
	if (xl LE 0)  goto m_Exit;
	upperbd(zh/xl,vh);
	if (vh GE yh)  goto m_Exit;
   m_MUYH:
	yh = vh;
	yhchng();
	goto m_Exit;
   m_MUZH:
	zh = vh;
	zhchng();
	goto m_Exit;
   m_Fail:
	FAIL;
   m_Exit:
	return(0);
}

     /*----------------*
      |   i_narrower 
      *----------------*/

 int	i_narrower		(void);

int
i_narrower()
{
	if (xl GE zl)  goto n_Ub;
	xl = zl;
	xlchng();
   n_Ub:
	if (xh LE zh)  goto n_Exit;
	xh = zh;
	xhchng();
	goto n_Exit;
   n_Exit:
	return(0);
}

     /*----------------*
      |   i_or 
      *----------------*/

 int	i_or		(void);

int
i_or()
{
	if (xl GT yl)  goto o_YX;
	if (zl GT xl)  goto o_DisjX;
	if (zl EQ xl)  goto o_DX3;
	zl = xl;
	zlchng();
	goto o_DX3;
   o_DisjX:
	if (xh LT zl)  goto o_EqZY;
	if (yh LT zl)  goto o_EqZX;
   o_DX3:
	if (zh LT yl)  goto o_EqZX;
	goto o_PartB;
   o_YX:
	if (zl GT yl)  goto o_DisjY;
	if (zl EQ yl)  goto o_DY3;
	zl = yl;
	zlchng();
	goto o_DY3;
   o_DisjY:
	if (yh LT zl)  goto o_EqZX;
	if (xh LT zl)  goto o_EqZY;
   o_DY3:
	if (zh GE xl)  goto o_PartB;
   o_EqZY:
	if (zl GT yl)  goto o_Uyzl;
	if (zl EQ yl)  goto o_HYZ;
	zl = yl;
	zlchng();
	goto o_HYZ;
   o_Uyzl:
	yl = zl;
	ylchng();
   o_HYZ:
	if (zh LT yh)  goto o_Uyzh;
	if (zh GT yh)  goto o_Uzyh;
	goto o_Exit;
   o_EqZX:
	if (zl GT xl)  goto o_Uxzl;
	if (zl EQ xl)  goto o_HXZ;
	zl = xl;
	zlchng();
	goto o_HXZ;
   o_Uxzl:
	xl = zl;
	xlchng();
   o_HXZ:
	if (zh LT xh)  goto o_Uxzh;
	if (zh GT xh)  goto o_Uzxh;
	goto o_Exit;
   o_PartB:
	if (xh GE yh)  goto o_UBX;
	if (zh LE yh)  goto o_Exit;
   o_Uzyh:
	zh = yh;
	zhchng();
	goto o_Exit;
   o_UBX:
	if (zh LE xh)  goto o_Exit;
   o_Uzxh:
	zh = xh;
	zhchng();
	goto o_Exit;
   o_Uyzh:
	yh = zh;
	yhchng();
	goto o_Exit;
   o_Uxzh:
	xh = zh;
	xhchng();
	goto o_Exit;
	FAIL;
   o_Exit:
	return(0);
}

     /*----------------*
      |   i_pow_odd 
      *----------------*/

 int	i_pow_odd		(void);

int
i_pow_odd()
{
	if (zl GE 0)  goto p_Right;
	if (zh LE 0)  goto p_Left;
	if (xl GE 0)  goto p_Rightx;
	if (xh LE 0)  goto p_Leftx;
	vl = -zl;
	upperbd2(ln(vl),vl);
	upperbd(vl/yl,vl);
	upperbd2(exp(vl),vl);
	vl = -vl;
	if (vl GT xl)  goto p_Uxl;
	if (vl EQ xl)  goto p_Ub;
	if (xl GT 0)  goto p_Uzl;
	if (xl EQ 0)  goto p_Zxl;
	vl = -xl;
	upperbd2(ln(vl),vl);
	upperbd(vl*yl,vl);
	upperbd2(exp(vl),vl);
	vl = -vl;
	goto p_UzlC;
   p_Zxl:
	zl = 0;
	zlchng();
	goto p_Ub;
   p_Leftx:
	swap(z);
	swap(x);
   p_Rightx:
	zl = 0;
	zlchng();
	goto p_Right;
   p_Left:
	swap(z);
	swap(x);
   p_Right:
	if (zl GT 0)  goto p_NZl;
	vl = 0;
	goto p_Lbch;
   p_NZl:
	lowerbd2(ln(zl),vl);
	lowerbd(vl/yl,vl);
	lowerbd2(exp(vl),vl);
   p_Lbch:
	if (vl LT xl)  goto p_Uzl;
	if (vl EQ xl)  goto p_Ub;
   p_Uxl:
	xl = vl;
	xlchng();
	goto p_Ub;
   p_Uzl:
	lowerbd2(ln(xl),vl);
	lowerbd(vl*yl,vl);
	lowerbd2(exp(vl),vl);
   p_UzlC:
	if (vl LE zl)  goto p_Ub;
	zl = vl;
	zlchng();
   p_Ub:
	if (zh GT 0)  goto p_NZh;
	vh = 0;
	goto p_Ubch;
   p_NZh:
	upperbd2(ln(zh),vh);
	upperbd(vh/yl,vh);
	upperbd2(exp(vh),vh);
   p_Ubch:
	if (vh GT xh)  goto p_Uzh;
	if (vh EQ xh)  goto p_Exit;
	xh = vh;
	xhchng();
	goto p_Exit;
   p_Uzh:
	if (xh GT 0)  goto p_Uzhp;
	if (xh EQ 0)  goto p_Uzhz;
	vh = -xh;
	lowerbd2(ln(vh),vh);
	lowerbd(vh*yl,vh);
	lowerbd2(exp(vh),vh);
	vh = -vh;
	goto p_UzhC;
   p_Uzhz:
	zh = 0;
	zhchng();
	goto p_Exit;
   p_Uzhp:
	upperbd2(ln(xh),vh);
	upperbd(vh*yl,vh);
	upperbd2(exp(vh),vh);
   p_UzhC:
	if (vh GE zh)  goto p_Exit;
	zh = vh;
	zhchng();
	goto p_Exit;
   p_Exit:
	return(0);
}

     /*----------------*
      |   i_qpow_even 
      *----------------*/

 int	i_qpow_even		(void);

int
i_qpow_even()
{
	if (zl GT 0)  goto q_Nonz;
	if (zl EQ 0)  goto q_Zero;
	if (zh LT 0)  goto q_Fail;
	zl = 0;
	zlchng();
   q_Zero:
	vl = 0;
	if (zh GT 0)  goto q_Dovh;
	vh = 0;
	if (xl GT 0)  goto q_Fail;
	if (xl EQ 0)  goto q_ZChkXh;
	xl = 0;
	xlchng();
   q_ZChkXh:
	if (xh LT 0)  goto q_Fail;
	if (xh EQ 0)  goto q_Exit;
	xh = 0;
	xhchng();
	goto q_Exit;
   q_Nonz:
	lowerbd2(ln(zl),vl);
	lowerbd(vl/yl,vl);
	lowerbd2(exp(vl),vl);
   q_Dovh:
	upperbd2(ln(zh),vh);
	upperbd(vh/yl,vh);
	upperbd2(exp(vh),vh);
	if (xl GE 0)  goto q_Eq;
	if (xh LE 0)  goto q_Flp;
	uh = -xl;
	if (uh LT vl)  goto q_Eq;
	if (xh LT vl)  goto q_Flp;
	if (uh GT xh)  goto q_Lefthi;
	uh = xh;
   q_Lefthi:
	if (uh GT vh)  goto q_Trimxl;
	if (uh EQ vh)  goto q_Exit;
	vh = uh;
	goto q_Uzhvh;
   q_Trimxl:
	uh = -xl;
	if (uh LE vh)  goto q_Trimxh;
	xl = -vh;
	xlchng();
   q_Trimxh:
	if (xh LE vh)  goto q_Exit;
	goto q_Uxh;
   q_Flp:
	uh = vh;
	swap(x);
	vh = uh;
   q_Eq:
	if (vl LT xl)  goto q_Uzl;
	if (vl EQ xl)  goto q_Ub;
	xl = vl;
	xlchng();
	goto q_Ub;
   q_Uzl:
	lowerbd2(ln(xl),vl);
	lowerbd(vl*yl,vl);
	lowerbd2(exp(vl),vl);
	if (vl LE zl)  goto q_Ub;
	zl = vl;
	zlchng();
   q_Ub:
	if (vh GT xh)  goto q_Uzh;
	if (vh EQ xh)  goto q_Exit;
   q_Uxh:
	xh = vh;
	xhchng();
	goto q_Exit;
   q_Uzh:
	vh = xh;
   q_Uzhvh:
	upperbd2(ln(vh),vh);
	upperbd(vh*yl,vh);
	upperbd2(exp(vh),vh);
	if (vh GE zh)  goto q_Exit;
	zh = vh;
	zhchng();
	goto q_Exit;
   q_Fail:
	FAIL;
   q_Exit:
	return(0);
}

     /*----------------*
      |   i_rootsquare 
      *----------------*/

 int	i_rootsquare		(void);

int
i_rootsquare()
{
	if (xl GE 0)  goto r_Eq;
	if (xh LE 0)  goto r_Flp;
	upperbd(xl*xl,ul);
	if (ul LT zl)  goto r_Zeroxl;
	upperbd(xh*xh,uh);
	if (uh LT zl)  goto r_Zeroxh;
	if (zl GE 0)  goto r_Zub;
	zl = 0;
	zlchng();
   r_Zub:
	if (ul GT uh)  goto r_Zhul;
	if (uh GT zh)  goto r_Mxh;
	if (uh EQ zh)  goto r_Exit;
	zh = uh;
	zhchng();
	goto r_Exit;
   r_Mxh:
	upperbd(sqrt(zh),uh);
	if (uh GE xh)  goto r_Exit;
	xh = uh;
	xhchng();
	uh = -uh;
	if (uh LE xl)  goto r_Exit;
	xl = uh;
	xlchng();
	goto r_Exit;
   r_Zhul:
	if (ul GT zh)  goto r_Mxh2;
	if (ul EQ zh)  goto r_Exit;
	zh = ul;
	zhchng();
	goto r_Exit;
   r_Mxh2:
	upperbd(sqrt(zh),uh);
	uh = -uh;
	if (uh LE xl)  goto r_Exit;
	xl = uh;
	xlchng();
	uh = -uh;
	if (uh GE xh)  goto r_Exit;
	xh = uh;
	xhchng();
	goto r_Exit;
   r_Flp:
	swap(x);
	goto r_Eq;
   r_Zeroxh:
	swap(x);
   r_Zeroxl:
	xl = 0;
	xlchng();
   r_Eq:
	lowerbd(xl*xl,ul);
	upperbd(xh*xh,uh);
	if (ul GT zl)  goto r_Chkzl;
	if (ul EQ zl)  goto r_Ub;
	lowerbd(sqrt(zl),ul);
	if (ul LE xl)  goto r_Ub;
	xl = ul;
	xlchng();
	goto r_Ub;
   r_Chkzl:
	zl = ul;
	zlchng();
   r_Ub:
	if (uh GT zh)  goto r_Chkxh;
	if (uh EQ zh)  goto r_Exit;
	zh = uh;
	zhchng();
	goto r_Exit;
   r_Chkxh:
	upperbd(sqrt(zh),uh);
	if (uh GE xh)  goto r_Exit;
	xh = uh;
	xhchng();
	goto r_Exit;
   r_Exit:
	return(0);
}

     /*----------------*
      |   i_sin 
      *----------------*/

 int	i_sin		(void);

int
i_sin()
{
	vl = 2;
	vh = pi();
	vh = vh/vl;
	vl = -vh;
	if (xh GT vh)  goto s_MRight;
	if (xl LT vl)  goto s_MLeft;
	upperbd2(sin(xh),uh);
	lowerbd2(sin(xl),ul);
	if (zh GT uh)  goto s_Uzh;
	if (zh EQ uh)  goto s_Low;
   s_Uxh:
	upperbd2(asin(zh),uh);
	if (uh GE xh)  goto s_Low;
	xh = uh;
	xhchng();
	goto s_Low;
   s_Uzh:
	zh = uh;
	zhchng();
   s_Low:
	if (zl LT ul)  goto s_Uzl;
	if (zl GT ul)  goto s_Uxl;
	goto s_Exit;
   s_MLeft:
	swap(x);
	swap(z);
	vh = -vl;
   s_MRight:
	if (xl LT 0)  goto s_ForceR;
	if (xl LT vh)  goto s_CR;
	goto s_Right;
   s_ForceR:
	if (zl GE 0)  goto s_CR;
	if (xl LT vl)  goto s_Exit;
	goto s_CR;
   s_Right:
	upperbd2(sin(xl),ul);
	lowerbd2(sin(xh),uh);
	if (ul GT zh)  goto s_Urxl;
	if (ul EQ zh)  goto s_Rlow;
	zh = ul;
	zhchng();
	goto s_Rlow;
   s_Urxl:
	lowerbd2(acos(zh),vl);
	lowerbd(vl+vh,vl);
	if (vl LE xl)  goto s_Rlow;
	xl = vl;
	xlchng();
   s_Rlow:
	if (uh LT zl)  goto s_Urxh;
	if (uh EQ zl)  goto s_Exit;
	zl = uh;
	zlchng();
	goto s_Exit;
   s_Urxh:
	upperbd2(acos(zl),vl);
	upperbd(vl+vh,vh);
	if (vh GE xh)  goto s_Exit;
	xh = vh;
	xhchng();
	goto s_Exit;
   s_CR:
	lowerbd2(sin(xl),ul);
	lowerbd2(sin(xh),uh);
	if (ul GT uh)  goto s_CRR;
	if (zh LT uh)  goto s_Uxh;
	goto s_Low;
   s_CRR:
	if (zh LT ul)  goto s_Urxl;
	goto s_Rlow;
   s_Uxl:
	lowerbd2(asin(zl),vl);
	if (vl LE xl)  goto s_Exit;
	xl = vl;
	xlchng();
	goto s_Exit;
   s_Uzl:
	zl = ul;
	zlchng();
	goto s_Exit;
   s_Exit:
	return(0);
}

     /*----------------*
      |   i_tan 
      *----------------*/

 int	i_tan		(void);

int
i_tan()
{
	lowerbd2(tan(xl),ul);
	upperbd2(tan(xh),uh);
	if (ul LT zl)  goto t_BackL;
	if (ul EQ zl)  goto t_NonsingH;
	zl = ul;
	zlchng();
	goto t_NonsingH;
   t_BackL:
	lowerbd2(atan(zl),ul);
	if (ul LE xl)  goto t_NonsingH;
	xl = ul;
	xlchng();
   t_NonsingH:
	if (uh GT zh)  goto t_BackH;
	if (uh EQ zh)  goto t_Exit;
	zh = uh;
	zhchng();
	goto t_Exit;
   t_BackH:
	upperbd2(atan(zh),uh);
	if (uh GE xh)  goto t_Exit;
	xh = uh;
	xhchng();
	goto t_Exit;
   t_Exit:
	return(0);
}

     /*----------------*
      |   i_vabs 
      *----------------*/

 int	i_vabs		(void);

int
i_vabs()
{
	if (xl GE 0)  goto v_Eq;
	if (xh LE 0)  goto v_Flp;
	ul = -xl;
	if (ul LT zl)  goto v_Zeroxl;
	if (xh LT zl)  goto v_Zeroxh;
	if (zl GE 0)  goto v_Zub;
	zl = 0;
	zlchng();
   v_Zub:
	if (ul GT xh)  goto v_Zhul;
	if (xh GT zh)  goto v_Mxh;
	if (xh EQ zh)  goto v_Exit;
	zh = xh;
	zhchng();
	goto v_Exit;
   v_Zhul:
	if (ul GT zh)  goto v_Muh;
	if (ul EQ zh)  goto v_Exit;
	zh = ul;
	zhchng();
	goto v_Exit;
   v_Muh:
	xl = -zh;
	xlchng();
	if (xh LE zh)  goto v_Exit;
	goto v_Chkxh;
   v_Mxh:
	xh = zh;
	xhchng();
	if (ul LE zh)  goto v_Exit;
	xl = -zh;
	xlchng();
	goto v_Exit;
   v_Zeroxl:
	xl = 0;
	xlchng();
	goto v_Eq;
   v_Zeroxh:
	xh = 0;
	xhchng();
   v_Flp:
	swap(x);
   v_Eq:
	if (xl GT zl)  goto v_Chkzl;
	if (xl EQ zl)  goto v_Ub;
	xl = zl;
	xlchng();
	goto v_Ub;
   v_Chkzl:
	zl = xl;
	zlchng();
   v_Ub:
	if (xh GT zh)  goto v_Chkxh;
	if (xh EQ zh)  goto v_Exit;
	zh = xh;
	zhchng();
	goto v_Exit;
   v_Chkxh:
	xh = zh;
	xhchng();
	goto v_Exit;
   v_Exit:
	return(0);
}

     /*----------------*
      |   i_wrap 
      *----------------*/

 int	i_wrap		(void);

int
i_wrap()
{
	ul = 2;
	ul = ul*yh;
	lowerbd(xl/ul,vl);
	vl = int_round(vl);
	lowerbd(xh/ul,vh);
	vh = int_round(vh);
	if (vl GT vh)  iaerror();
	if (vl EQ vh)  goto w_Same;
	ul = -yh;
	if (zl GE ul)  goto w_Dif2;
	zl = ul;
	zlchng();
   w_Dif2:
	if (zh LE yh)  goto w_Exit;
	zh = yh;
	zhchng();
	goto w_Exit;
   w_Same:
	ul = ul*vl;
	upperbd(xh-ul,vh);
	lowerbd(xl-ul,vl);
	if (zh GT vh)  goto w_Uzh;
	if (zh EQ vh)  goto w_Same2;
	upperbd(zh+ul,vh);
	if (xh LE vh)  goto w_Same2;
	xh = vh;
	xhchng();
	goto w_Same2;
   w_Uzh:
	zh = vh;
	zhchng();
   w_Same2:
	if (zl LT vl)  goto w_Uzl;
	if (zl EQ vl)  goto w_Exit;
	lowerbd(zl+ul,vl);
	if (xl GE vl)  goto w_Exit;
	xl = vl;
	xlchng();
	goto w_Exit;
   w_Uzl:
	zl = vl;
	zlchng();
	goto w_Exit;
   w_Exit:
	return(0);
}

     /*----------------*
      |   i_xp 
      *----------------*/

 int	i_xp		(void);

int
i_xp()
{
	if (zl GT 0)  goto x_Loglo;
	if (zl EQ 0)  goto x_Zeroch;
	zl = 0;
	zlchng();
   x_Zeroch:
	if (zh LE 0)  goto x_Fail;
	goto x_Expl;
   x_Loglo:
	lowerbd2(ln(zl),ul);
	if (xl GT ul)  goto x_Expl;
	if (xl EQ ul)  goto x_Loghi;
	xl = ul;
	xlchng();
	goto x_Loghi;
   x_Expl:
	lowerbd2(exp(xl),ul);
	if (ul LE zl)  goto x_Loghi;
	zl = ul;
	zlchng();
   x_Loghi:
	upperbd2(ln(zh),uh);
	if (uh GT xh)  goto x_Exph;
	if (uh EQ xh)  goto x_Exit;
	xh = uh;
	xhchng();
	goto x_Exit;
   x_Exph:
	upperbd2(exp(xh),uh);
	if (uh GE zh)  goto x_Exit;
	zh = uh;
	zhchng();
	goto x_Exit;
   x_Fail:
	FAIL;
   x_Exit:
	return(0);
}
#endif
