/*
 * next.spc
 */

#exclude <ansi/ *>
#exclude <bsd/ *>
#exclude <mach/ *>
#exclude <streams/ *>
#exclude <remote/ *>
#exclude <machkit/ *>

als$begin_silent; /* dont output the following */

#define __ARCHITECTURE__ "m68k"

typedef int va_list;
typedef int size_t;

als$end_silent;

#import "next.h"


extern void "DPSFlush"();

extern int "DPSGetEvent"(DPSContext context, NXEvent *anEvent, int mask, double timeout, int threshold);

extern int "DPSPeekEvent"(DPSContext context, NXEvent *anEvent, int mask, double timeout, int threshold);

extern void "NXConvertColorToRGB"(NXColor color, float *red, float *green, float *blue);

extern void "NXConvertColorToCMYK"(NXColor color, float *cyan, float *magenta, float *yellow, float *black);

extern void "NXConvertColorToHSB"(NXColor color, float *hue, float *saturation, float *brightness);

extern void "NXConvertColorToGray"(NXColor color, float *gray);

extern NXColor "NXConvertRGBToColor"(float red, float green, float blue);

extern NXColor "NXConvertCMYKToColor"(float cyan, float magenta, float yellow, float black);
	
extern NXColor "NXConvertHSBToColor"(float hue, float saturation, float brightness);

extern NXColor "NXConvertGrayToColor"(float gray);

extern NXTopLevelErrorHandler *"NXSetTopLevelErrorHandler"(NXTopLevelErrorHandler *procedure);

extern NXTopLevelErrorHandler *"NXTopLevelErrorHandler"(void);

extern int "NXPutc"(NXStream *stream, char c);

extern int "NXRead"(NXStream *stream, void *buf, int count);

extern int "NXWrite"(NXStream *stream, const void *buf, int count);

extern BOOL "NXAtEOS"(NXStream *stream);

extern NXUncaughtExceptionHandler *"NXGetUncaughtExceptionHandler"(void);


extern void *"NXZoneMalloc"(NXZone*zonep, size_t size);

extern void *"NXZoneRealloc"(NXZone *zonep, void *ptr, size_t size);

extern void "NXZoneFree"(NXZone *zonep, void *ptr);

extern void "NXDestroyZone"(NXZone *zonep);

extern void "NX_ASSERT"(int exp, char *msg);

extern int "NX_EVENTCODEMASK"(int eventType);

extern void "NX_FREE"(void *pointer);

extern void "NX_RAISE"(int code, const void *data1, const void *data2);

extern NXCoord "NX_X"(NXRect *aRect);

extern NXCoord "NX_Y"(NXRect *aRect);

extern NXCoord "NX_WIDTH"(NXRect *aRect);

extern NXCoord "NX_HEIGHT"(NXRect *aRect);

extern NXCoord "NX_MAXX"(NXRect *aRect);

extern NXCoord "NX_MAXY"(NXRect *aRect);

extern NXCoord "NX_MIDX"(NXRect *aRect);

extern NXCoord "NX_MIDY"(NXRect *aRect);

