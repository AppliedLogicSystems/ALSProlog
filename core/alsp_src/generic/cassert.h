void throw_assertion(const char *test, const char *file, int line);

#ifdef DEBUG
#define ASSERT(x) if (!(x)) throw_assertion(#x, __FILE__, __LINE__);
#else
#define ASSERT(X)
#endif
