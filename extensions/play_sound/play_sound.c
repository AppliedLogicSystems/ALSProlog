#include "alspi.h"

static int play_sound_file(void)
{
    PWord arg;
    int arg_type;
    char path[MAX_PATH];

    PI_getan(&arg, &arg_type, 1);
  
  	
  	
	switch (arg_type) {
	case PI_SYM:
		PI_getsymname(path, arg, MAX_PATH);
		break;
	case PI_UIA:
		PI_getuianame(path, arg, MAX_PATH);
		break;	
	}

	if (PlaySound(path, NULL, SND_FILENAME)) PI_SUCCEED;
	else PI_FAIL;
}

PI_BEGIN
    PI_DEFINE("play_sound_file",1,play_sound_file)
PI_END

void pi_init(void)
{
    PI_INIT;
}
