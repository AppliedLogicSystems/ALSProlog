#include "defs.h"

int os_copy_file(const char *from_file, const char *to_file)
{
    if (CopyFile(from_file, to_file, FALSE)) return 1;
    else return 0;
}


static double process_start_time;

#define FT2SEC(x) ((double) x.dwHighDateTime * 429.4967296 \
		   + (double) x.dwLowDateTime * 0.0000001)

void os_init_time(void)
{
	FILETIME creation, exit, kernal, user;

	if (GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernal, &user))
		process_start_time = FT2SEC(creation);
	else process_start_time = 0.0;
}

double os_cputime(void)
{
	FILETIME creation, exit, kernal, user;

	if (GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernal, &user))
		return FT2SEC(kernal) + FT2SEC(user);
	else return 0.0;
}

double os_realtime(void)
{
	FILETIME now;
	
	GetSystemTimeAsFileTime(&now);
	return FT2SEC(now) - process_start_time;
}
