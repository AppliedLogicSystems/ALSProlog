#include "alspi.h"

static int play_sound_file(void)
{
    PWord arg;
    int arg_type;
#ifdef macintosh
	FSSpec spec;
	short l, ref;
	#define MAX_PATH 256
#endif
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

#ifdef macintosh
	l = strlen(path);
	memcpy(path+1, path, l);
	path[0] = l;
	FSMakeFSSpec (0, 0, (unsigned char *)path, &spec);	
	if (FSpOpenDF(&spec, fsRdPerm, &ref) != noErr) PI_FAIL;
	if (SndStartFilePlay(NULL, ref, 0, NULL, NULL, NULL, NULL, FALSE) != noErr)
		PI_FAIL;
	FSClose(ref);
	PI_SUCCEED;
#elif WIN32
	if (PlaySound(path, NULL, SND_FILENAME)) PI_SUCCEED;
	else PI_FAIL;
#endif
}

PI_BEGIN
    PI_DEFINE("play_sound_file",1,play_sound_file)
PI_END

void pi_init(void)
{
    PI_INIT;
}
