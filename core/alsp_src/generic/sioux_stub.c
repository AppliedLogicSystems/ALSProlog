#include <console.h>
#include <SIOUX.h>

int ccommand(char ***)
{
	return 0;
}

short SIOUXHandleOneEvent(struct EventRecord *)
{
	return 0;
}

short InstallConsole(short fd)
{
#pragma unused (fd)

	return 0;
}

void RemoveConsole(void)
{
}

long WriteCharsToConsole(char *buffer, long n)
{
#pragma unused (buffer, n)

	return 0;
}

long ReadCharsFromConsole(char *buffer, long n)
{
#pragma unused (buffer, n)

	return 0;
}

extern char *__ttyname(long fildes)
{
#pragma unused (fildes)
	static char *__devicename = "null device";

	if (fildes >= 0 && fildes <= 2)
		return (__devicename);

	return (0L);
}
