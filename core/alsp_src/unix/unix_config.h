#include "dfltsys.h"

#define OSStr "unix"

#if   defined(UNIX_AIX)
#include "aix_config.h"
#elif defined(UNIX_SUNOS)
#include "sunos_config.h"
#elif defined(UNIX_SOLARIS)
#include "solaris_config.h"
#elif defined(UNIX_HPUX)
#include "hpux_config.h"
#elif defined(UNIX_LINUX)
#include "linux_config.h"
#elif defined(UNIX_IRIX)
#include "linux_config.h"
#else
#error
#endif
