/*---------------------------------------------------------*
 |			wxwin.h
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

/* wxwin.h
   Note that this header is used to both generate the wxWindows interface
   code and also compile that interface code.  The interface is generated
   as ANSI C, but compiled as C++.
*/

#ifdef __cplusplus
#include "wx.h"
#include "wxextend.h"
#ifdef wx_mac
#include "wxmacpatch.h"
#endif
#else
#include "c_wxextend.h"
#endif

void StartwxWindows(void);
