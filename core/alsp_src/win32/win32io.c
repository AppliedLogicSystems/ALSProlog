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
	else return 0.0 /* error */;
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


static BOOL timer_initialized = FALSE;

static struct {
	DWORD initial;
	DWORD interval;
} timer_state;

static CRITICAL_SECTION timer_state_critical_section;
static HANDLE timer_reset_event;

static DWORD WINAPI timer_thread( LPVOID p)
{
	DWORD time_out, next_time_out;
		
	time_out = next_time_out = INFINITE;
	
	while (1) {
		switch (WaitForSingleObject(timer_reset_event, time_out)) {
		case WAIT_OBJECT_0:
			EnterCriticalSection(&timer_state_critical_section);
			time_out = timer_state.initial ? timer_state.initial : INFINITE;
			next_time_out = timer_state.interval ? timer_state.interval : INFINITE;
			LeaveCriticalSection(&timer_state_critical_section);
			break;
		case WAIT_TIMEOUT:
			wm_safety = -1;
			wm_interrupt_caught = ALSSIG_ALARM;
			time_out = next_time_out;
			break;
		}
	}
	
	return 0;
}

static void initialize_timer(void)
{
	DWORD id;
	
	timer_state.initial = 0;
	timer_state.interval = 0;
	InitializeCriticalSection(&timer_state_critical_section);
	timer_reset_event = CreateEvent(NULL, FALSE, FALSE, NULL);
	CreateThread(NULL, 0, timer_thread, NULL, 0, &id);
	timer_initialized = TRUE;
} 

int os_set_timer(double initial, double interval)
{
	if (!timer_initialized) initialize_timer();
	
	
	EnterCriticalSection(&timer_state_critical_section);
	
	timer_state.initial = initial * 1000.0;
	timer_state.interval = interval * 1000.0;
	
	LeaveCriticalSection(&timer_state_critical_section);
	
	SetEvent(timer_reset_event);
	
	return 0;
}
