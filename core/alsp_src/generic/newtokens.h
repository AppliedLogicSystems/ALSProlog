#ifndef __newtokens__
#define __newtokens__

#define TK(ppsym, name)	ppsym
#define OP(ppsym, name, a, b) ppsym

enum {
	_TK_START = 0,
#include "newtokini.h"
};

#undef TK
#undef OP

#endif /* __newtokens__ */