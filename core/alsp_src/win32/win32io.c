#include "defs.h"

int os_copy_file(const char *from_file, const char *to_file)
{
    if (CopyFile(from_file, to_file, FALSE)) return 1;
    else return 0;
}


static double process_start_time;
DWORD platform;

#define FT2SEC(x) ((double) x.dwHighDateTime * 429.4967296 \
		   + (double) x.dwLowDateTime * 0.0000001)

void os_init_time(void)
{
	{
		OSVERSIONINFO osvi;
		char  szVersion [80];

		memset(&osvi, 0, sizeof(OSVERSIONINFO));
		osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
		GetVersionEx (&osvi);

		platform = osvi.dwPlatformId;
	}

	if (platform == VER_PLATFORM_WIN32_NT) {
		FILETIME creation, exit, kernal, user;

		if (GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernal, &user))
			process_start_time = FT2SEC(creation);
		else /* error */;
	} else {
		SYSTEMTIME now;
		FILETIME ft_now;
		
		GetSystemTime(&now);
		if (SystemTimeToFileTime(&now, &ft_now))
			process_start_time = FT2SEC(ft_now);
		else /* error */;
	}
}

double os_cputime(void)
{
	if (platform == VER_PLATFORM_WIN32_NT) {
	FILETIME creation, exit, kernal, user;

	if (GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernal, &user))
		return FT2SEC(kernal) + FT2SEC(user);
	else /* error */;
	} else
		return os_realtime();
}

double os_realtime(void)
{
	SYSTEMTIME now;
	FILETIME ft_now;
	
	GetSystemTime(&now);
	SystemTimeToFileTime(&now, &ft_now);
	return FT2SEC(ft_now) - process_start_time;
}
