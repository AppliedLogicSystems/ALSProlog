#include <string.h>
#include "alspi.h"
#include <Speech.h>

static Boolean SpeechAvailable (void) {
	OSErr err;
	long result;
	err = Gestalt(gestaltSpeechAttr, &result);
	if ((err != noErr) || !(result &
		(1 << gestaltSpeechMgrPresent)))
		return FALSE;
	else
		return TRUE;
}

static unsigned char *pstrcpy(unsigned char *ps, const char *cs)
{
	size_t l = strlen(cs);
	if (l > 255) l = 255;
	memmove(ps+1, cs, l);
	ps[0] = l;
	
	return ps;
}

static int say(void)
{
	PWord arg;
	int type;
	Str255 pstring;
	OSErr result;

	if (SpeechAvailable()) {
		PI_getan(&arg, &type, 1);
		if (type == PI_SYM) PI_getsymname((char *) pstring, arg, 256);
		else if (type == PI_UIA) PI_getuianame((char *) pstring, arg, 256);
		else PI_FAIL;
		 
		pstrcpy(pstring, (char *) pstring);

		result = SpeakString(pstring);
		
		if (result == noErr) PI_SUCCEED;
		else PI_FAIL;
	} else PI_FAIL;
}

PI_BEGIN
  PI_DEFINE("say",1,say)
PI_END

void pi_init(void);
void pi_init(void)
{
    PI_INIT;
}


