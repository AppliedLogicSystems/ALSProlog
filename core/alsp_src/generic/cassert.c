#include "defs.h"

#include "cassert.h"

void throw_assertion(const char *test, const char *file, int line)
{
#pragma unused(file,line)
#ifdef macintosh
	DebugStr(test);
#else
	fprintf(stderr, "Assertion Failed: %s\nIn File %s, Line %d\n",
		test, file, line);
#endif
}
