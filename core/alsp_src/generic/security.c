/*=============================================================*
 | security.c
 | Copyright (c) 1996 Applied Logic Systems
 |
 | Hardware/software copy-protection security for timed-demos
 | and hardware-key protected versions of ALS Prolog.
 |
 | Author: Chuck Houpt
 *=============================================================*/

#include <time.h>
#include "security.h"

#include "defs.h"

/*-----------------------------------------* 
	Hardware Key Abstraction Layer 
 *-----------------------------------------*/

#ifdef HARDWARE_KEY

#if defined(unix)
#define SSIWS_UNIKEY
#elif defined(MSWin32)
#define SSI_UNIKEY
#elif defined(MacOS)
#define SENTINEL_EVE3
#else
#error
#endif

#if defined(SSIWS_UNIKEY)

#include <netinet/in.h>

#include <unix_e.h>

/* Applied Logic System's Company Code */
BYTE _SSIWS_PrivateCompIdCodes[] =
{
0x01, 0xd4, 0xc8, 0x43, 0xe3, 0xad, 0x8a, 0xe7, 0xeb, 0x13, 0xe3, 0x14, 0x81,
0xc6, 0xfe, 0x70, 0x7b, 0x1f, 0x0b, 0x50, 0x8d, 0xe7, 0xc0, 0x4e, 0xa6, 0x91,
0x7b, 0x64, 0x48, 0xd4
};

/* ALS Prolog Application ID */
#define SSIWS_APPLICATION_ID    0x948576

#elif defined(SSI_UNIKEY)
#include <windows.h>
#include <ssi_cw32.h>

#elif defined(SENTINEL_EVE3)
#include <Memory.h>
#include <OSUtils.h>
#include <eve3demo.h>

#define ALS_EVE3_DEVELOPER_ID	0xBC58
#define ALS_EVE3_WRITE_PASSWORD	0x2997

#define ALS_EVE3_TYPE_GPR	0
#define ALS_EVE3_START_LGPR	2
#define ALS_EVE3_END_LGPR	4
#define ALS_EVE3_DURATION_LGPR	6

static Handle EveHandle;

#else
#error
#endif

#define ALS_SIMPLE_KEY	0
#define ALS_TIMED_KEY	1

static void hardware_key_error(void)
{
#if defined(SSIWS_UNIKEY)
    PI_app_printf(PI_app_printf_error,
"\
Error: UniKey Hardware key required.\n\
This version of ALS Prolog requires a UniKey hardware key.\n\
Please check that the hardware key is correctly attached to the serial port.\n\
Exiting ALS Prolog.\n\
"
    );

#elif defined(SSI_UNIKEY)
    PI_app_printf(PI_app_printf_error,
"\
Error: UniKey Hardware key required.\n\
This version of ALS Prolog requires a UniKey hardware key.\n\
Please check that the hardware key is correctly attached to the parallel port.\n\
Exiting ALS Prolog.\n\
"
    );
#elif defined(SENTINEL_EVE3)
    PI_app_printf(PI_app_printf_error,
"\
Error: Sentinel Eve 3 Hardware key required.\n\
This version of ALS Prolog requires a Sentinel Eve 3 hardware key.\n\
Please check that the hardware key is correctly attached to the ABD port.\n\
Exiting ALS Prolog.\n\
"
    );
#else
#error
#endif
    exit(EXIT_ERROR);
}

static void time_limit_expired_error(void)
{
    PI_app_printf(PI_app_printf_error,
"\
Error: Time limit expired.\n\
This time limited version of ALS Prolog has expired.\n\
Exiting ALS Prolog.\n\
"
    );
    exit(EXIT_ERROR);
}


#if defined(SSIWS_UNIKEY)
static void hardware_key_device_error(void)
{
    PI_app_printf(PI_app_printf_error,
"\
Error: Unable to access hardware key device.\n\
Please check that the path to the correct key device is in the evironment\n\
variable ALS_KEY_DEVICE or in the file 'key_device' in the ASL Prolog\n\
directory and that you have read/write permission on the device.\n\
Exiting ALS Prolog.\n\
"
    );
    exit(EXIT_ERROR);
}
#endif

static void shutdown_hardware_key(void)
{
#if defined(SSIWS_UNIKEY)
    if (SSIWS_ShutDown()) {
	PI_app_printf(PI_app_printf_error, "Error: UniKey close failed.\n");
	exit(EXIT_ERROR);
    }
#elif defined(SSI_UNIKEY)
    if (SSI_Close()) {
	PI_app_printf(PI_app_printf_error, "Error: UniKey close failed.\n");
	exit(EXIT_ERROR);
    }
#elif defined(SENTINEL_EVE3)
    /* No action necessary. */   
#else
#error
#endif
}

static void do_check_hardware_key(void)
{
#if defined(SSIWS_UNIKEY)
    AppINFO info;
    if (SSIWS_Interrogate(SSIWS_APPLICATION_ID, &info)) hardware_key_error();
#elif defined(SSI_UNIKEY)
    if (SSI_Check(0)) hardware_key_error();
#elif defined(SENTINEL_EVE3)
    unsigned short developer_id;
    
    if (EveHandle == NULL
        || RBEREAD(E3_READ_ID, &developer_id, 0, EveHandle)
        || developer_id != ALS_EVE3_DEVELOPER_ID) hardware_key_error();
#else
#error
#endif
}

static void check_hardware_key(void)
{
    static time_t last_check = 0;
    time_t now;

    now = time(NULL);

    if (difftime(now, last_check) >= 3600.0) {
      do_check_hardware_key(); 
      last_check = now;
    }
}

#ifdef SENTINEL_EVE3
static unsigned long get_eve3_long_gpr(short lgpr)
{
    unsigned short lo, hi;
    unsigned long value;
    
    if (EveHandle == NULL || RBEREAD(E3_READ_GPR + lgpr, &hi, 0, EveHandle))
	hardware_key_error();
    if (EveHandle == NULL || RBEREAD(E3_READ_GPR + lgpr + 1, &lo, 0, EveHandle))
	hardware_key_error();

    return lo + (hi << 16);
}

static void set_eve3_long_gpr(short lgpr, unsigned long value)
{
    unsigned short lo, hi;
    
    lo = value & 0xFFFF; hi = value >> 16;
    
    if (EveHandle == NULL
        || RBEWRITE(E3_SET_GPR + lgpr, hi, ALS_EVE3_WRITE_PASSWORD, EveHandle))
	hardware_key_error();
    if (EveHandle == NULL
        || RBEWRITE(E3_SET_GPR + lgpr + 1, lo, ALS_EVE3_WRITE_PASSWORD, EveHandle))
	hardware_key_error();
    
}
#endif /* SENTINEL_EVE3 */

/* It might seem odd for this function to return the current time, but some
hardware keys have built-in clocks. */

static void get_hardware_key_times(unsigned long *now, unsigned long *start,
			    unsigned long *end, unsigned long *duration)
{
#if defined(SSIWS_UNIKEY)
    unsigned long key_memory[3];

    time((time_t *)now);

    if (SSIWS_ReadMemory(SSIWS_APPLICATION_ID, 2, (WORD *)key_memory, 6))
        hardware_key_error();

    *start = ntohl(key_memory[0]);
    *end = ntohl(key_memory[1]);
    *duration = ntohl(key_memory[2]);   	

#elif defined(SSI_UNIKEY)
    SYSTEMTIME sys_time;
    FILETIME file_time;
    int ROMSize;
    long key_memory[3];
    
    GetSystemTime(&sys_time);
    if (!SystemTimeToFileTime(&sys_time, &file_time))
    	hardware_key_error();
    *now = file_time.dwHighDateTime;
    
    ROMSize = SSI_GetROMSize();
    if (SSI_Read(ROMSize + 2, 6, (unsigned short *)key_memory))
    	hardware_key_error();
    *start = key_memory[0];
    *end = key_memory[1];
    *duration = key_memory[2];   	

#elif defined(SENTINEL_EVE3)
    GetDateTime(now);
    *start = get_eve3_long_gpr(ALS_EVE3_START_LGPR);
    *end = get_eve3_long_gpr(ALS_EVE3_END_LGPR);
    *duration = get_eve3_long_gpr(ALS_EVE3_DURATION_LGPR);
#else
#error
#endif
}

static void set_hardware_key_times(unsigned long start, unsigned long end)
{
#if defined(SSIWS_UNIKEY)
    unsigned long key_memory[2];

    key_memory[0] = htonl(start);
    key_memory[1] = htonl(end);

    if (SSIWS_WriteMemory(SSIWS_APPLICATION_ID, 2, key_memory, 4))
    	hardware_key_error();
    
#elif defined(SSI_UNIKEY)
    int ROMSize;
    long key_memory[2];

    key_memory[0] = start;
    key_memory[1] = end;

    ROMSize = SSI_GetROMSize();
    if (SSI_Write(ROMSize + 2, 4, (unsigned short *)key_memory))
    	hardware_key_error();
    
#elif defined(SENTINEL_EVE3)
    set_eve3_long_gpr(ALS_EVE3_START_LGPR, start);
    set_eve3_long_gpr(ALS_EVE3_END_LGPR, end);
#else
#error
#endif
}

/*-------------------------------------------------------------------------------------*
 |		Hardware Key Abstract Check Level.
 |	
 |	This level implements the logic of hardware key checking.  Two types of hardware
 |	key are handled at the moment: simple keys and timed keys.  check_hardware_key
 |	determines which type of key is present, tests and updates the hardware key.
 *-------------------------------------------------------------------------------------*/

static void test_hardware_key_time_limit(void)
{
    unsigned long now, start, end, duration;
    
    get_hardware_key_times(&now, &start, &end, &duration);

    if (start == 0 && end == 0) {
      /* A start and end of 0 means this is the first time this
         key has been used, so initilize the time limit. */
      set_hardware_key_times(now, now + duration);
    } else if (now < start || now > end) {
        time_limit_expired_error();
    } else if (now > start) {
      set_hardware_key_times(now, end);
    }
}

static void init_hardware_key(void)
{
#if defined(SSIWS_UNIKEY)
#define DEVPATH_MAX	512
    FILE *dev_file;
    const char *device;
    char dev_file_path[DEVPATH_MAX], line[DEVPATH_MAX];
    int line_length;
    unsigned long type;
    
    /* Determine the device to use for communicating with the hardware key */
    device = getenv("ALS_KEY_DEVICE");
    
    if (!device) {
    	
    	strncpy(dev_file_path, imagedir, DEVPATH_MAX);
    	dev_file_path[DEVPATH_MAX] = 0;
    	strncat(dev_file_path, "key_device", DEVPATH_MAX - strlen(dev_file_path));
    	dev_file_path[DEVPATH_MAX] = 0;
    	
	    dev_file = fopen(dev_file_path, "r");
	    if (dev_file) {
	    	device = fgets(line, DEVPATH_MAX, dev_file);
	    	if (device == 0) {
		    fclose(dev_file);
	    		
		    hardware_key_device_error();
	    	}
	    	line_length = strlen(line);
	    	if (line[line_length-1] == '\n') line[line_length-1] = 0;
	    } else {
	    	hardware_key_device_error();
	    }
    }

    if (access(device, F_OK | R_OK | W_OK)) {
    	hardware_key_device_error();
    }

    if (SSIWS_Initialize(device)) hardware_key_error();
    
    /* Determine the type of key */
    
    if (SSIWS_ReadMemory(SSIWS_APPLICATION_ID, 0, (WORD *)&type, 2))
    	hardware_key_error();
    type = ntohl(type);
    
#elif defined(SSI_UNIKEY)
    int ROMSize;
    unsigned long type;
        
    /* No init action necessary */
    if (SSI_Open(0)) hardware_key_error();
    
    /* Determine the type of key */
    
    ROMSize = SSI_GetROMSize();

    if (SSI_Read(ROMSize, 2, (unsigned short *)&type))
    	hardware_key_error();
    
#elif defined(SENTINEL_EVE3)
    unsigned long serial_number;
    unsigned short type;

    /* Initilize the Eve handle and find the key. */
    EveHandle = RBEHANDLE();
    if (EveHandle == NULL) hardware_key_error();
    if (RBEFINDFIRST(ALS_EVE3_DEVELOPER_ID, &serial_number, EveHandle))
        hardware_key_error();
        
    /* Determine the type of key. */
    if (RBEREAD(ALS_EVE3_TYPE_GPR + E3_READ_GPR, &type, 0, EveHandle))
        hardware_key_error();
#else
#error
#endif

    switch (type) {
    case ALS_SIMPLE_KEY:
    	check_hardware_key();
	break;
    case ALS_TIMED_KEY:
    	test_hardware_key_time_limit();
    	break;
    default:
    	hardware_key_error();
    	break;
    }
}

#endif /* HARDWARE_KEY */

/*------------------------------------------------------------------------------------*
 |			Security Abstract Layer
 |	
 |	The top level abstract copy protection security interface.
 |	
 |	The security system is public to the rest of the ALS Prolog code and
 |	it implements a flexible means of checking security.
 |	
 |	The security system has three states:
 |	
 |	1) 	un-initilized - before security is initilized, and after security is
 |   	shutdown, the security system is in the un-initilized state.  The only
 |   	valid call for this state is init_security.
 |   
 |	2) 	disabled - After the security system is initilized it is in the disabled
 |   	state.  Calls to check_security() are no-ops.  To enable the security system,
 |   	enable_security() must be called.
 |	
 |	3) 	enabled - After the security system is enabled, calls to check_security() will
 |	   	perform a test of the software or hardware copy protection.
 |	
 |	init_security() - performs any necessary setup, and puts the security system into
 |					  the disabled state.
 *------------------------------------------------------------------------------------*/

enum {security_uninit, security_disabled, security_enabled} security_state = security_uninit;

void init_security(void)
{
    
    switch (security_state) {
    case security_uninit:
    	security_state = security_disabled;
    	break;
    default:
	/* Error */
	break;
    }
}

void shutdown_security(void)
{
    switch (security_state) {
    case security_disabled:
    case security_enabled:
#ifdef HARDWARE_KEY
	if (security_state == security_enabled) shutdown_hardware_key();
#endif
	security_state = security_uninit;
	break;
    default:
	/* Error */
	break;
    }
}

void enable_security(void)
{
    switch (security_state) {
    case security_disabled:
    	security_state = security_enabled;
#ifdef HARDWARE_KEY
	    init_hardware_key();
#endif    	
	break;
    default:
	break;
    }
}

	/* -------------------------------------------------------------------*
		Date expiration when NOT using the hardware keys:
		#define DIEYEAR 0 	-- normal, non-restricted version
		#define DIEYEAR 96	-- year in which expiration occurs
		#define DIEMON 		-- expiration month: jan = 0, feb = 1, ....
		#define DIEDAY 		-- expiration day of month:  1,2,3,....
	 * -------------------------------------------------------------------*/
#ifndef DIEYEAR
#define DIEDAY 1
#define DIEMON 0
#define DIEYEAR 0
#endif

void check_security(void)
{
#ifdef HARDWARE_KEY
    if (security_state == security_enabled) check_hardware_key();
#elif !defined(MacOS)
/* kills Mac compiler (Windows too???):::: */
	time_t tv;
	struct tm *tp;

    tv = time(0L);
	tp = localtime(&tv);

	if (DIEYEAR > 0 && (
			(tp->tm_year > DIEYEAR) ||
		 	(tp->tm_mon > DIEMON)  ||
		 	((tp->tm_mon == DIEMON)  && (tp->tm_mday >= DIEDAY)))  )
	{
    PI_app_printf(PI_app_printf_error,
"\
------------------------------------------------------------------\n\
Sorry: Demonstration/Evaluation time limit [%d/%d/%d] exceeded.\n\
Please contact ALS to purchase unrestricted version.\n\
Exiting ALS Prolog.\n\
------------------------------------------------------------------\n\
",DIEYEAR,DIEMON+1,DIEDAY
    );
	exit(EXIT_ERROR);
	}
#endif

}

/* ---------------------------------------------*
	Prolog builtin for enabling security.
 * ---------------------------------------------*/

int pbi_enable_security(void)
{
    enable_security();
    PI_SUCCEED;
}