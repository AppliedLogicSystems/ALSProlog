#include "defs.h"

#include "cassert.h"

void throw_assertion(const char *test, const char *file, int line)
{
#ifdef __MWERKS__
#pragma unused(file,line)
#endif
#ifdef macintosh
	DebugStr((unsigned char *)test);
#else
	fprintf(stderr, "Assertion Failed: %s\nIn File %s, Line %d\n",
		test, file, line);
#endif
}
