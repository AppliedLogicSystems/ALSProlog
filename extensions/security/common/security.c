
/*-----------------------------------------* 
	Hardware Key Abstraction Layer 
 *-----------------------------------------*/

#if defined(unix)
#define SSIWS_UNIKEY
#elif defined(WIN32)
#define SSI_UNIKEY
#elif defined(macintosh)
#define SENTINEL_EVE3
#else
#error
#endif

#if defined(SSIWS_UNIKEY)

#include <unistd.h>
#include <limits.h>
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

typedef enum {
	no_error,
	general_error,
	device_name_error,
	device_permisson_error,
	hardware_key_error,
	system_service_error,
	max_codes
} error_code;

static const char *result_symbols[max_codes] = {
	"no_error",
	"general_error",
	"device_name_error",
	"device_permisson_error",
	"hardware_key_error",
	"system_service_error"
};

#if defined(SSI_UNIKEY)
static error_code SSI_error_code(void)
{
	error_code c;
	switch (SSI_GetError()) {
	case EtbDevAccess:
	case EtbApplAccess:
		c = hardware_key_error;
		break;
	case EsysInitFailed:
		c = system_service_error;
		break;
	default:
		c = general_error;
		break;
	}
	
	return c;
}
#endif


static error_code init_hardware_key(const char *device)
{
#if defined(SSIWS_UNIKEY)
    
    if (access(device, F_OK | R_OK | W_OK)) {
    	return device_permisson_error;
    }

    if (SSIWS_Initialize(device)) return hardware_key_error;

#elif defined(SSI_UNIKEY)
        
    /* No init action necessary */
    if (SSI_Open(0)) return SSI_error_code();
        
#elif defined(SENTINEL_EVE3)
    unsigned long serial_number;

    /* Initilize the Eve handle and find the key. */
    EveHandle = RBEHANDLE();
    if (EveHandle == NULL) return Eve_error_code();
    if (RBEFINDFIRST(ALS_EVE3_DEVELOPER_ID, &serial_number, EveHandle))
        return Eve_error_code();
        
#else
#error
#endif

	return no_error;
}

static error_code shutdown_hardware_key(void)
{
#if defined(SSIWS_UNIKEY)
    if (SSIWS_ShutDown()) return general_error;
#elif defined(SSI_UNIKEY)
    if (SSI_Close()) return SSI_error_code();
#elif defined(SENTINEL_EVE3)
    /* No action necessary. */   
#else
#error
#endif

	return no_error;
}

static error_code check_hardware_key(void)
{
#if defined(SSIWS_UNIKEY)
    AppINFO info;
    if (SSIWS_Interrogate(SSIWS_APPLICATION_ID, &info)) return hardware_key_error;
#elif defined(SSI_UNIKEY)
    if (SSI_Check(0)) return SSI_error_code();
#elif defined(SENTINEL_EVE3)
    unsigned short developer_id;
    
    if (EveHandle == NULL
        || RBEREAD(E3_READ_ID, &developer_id, 0, EveHandle)
        || developer_id != ALS_EVE3_DEVELOPER_ID) return 0;
#else
#error
#endif

	return no_error;
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

#include "alspi.h"

static int check_security(void)
{
    PWord device, arg, code;
    int device_type, arg_type, code_type;
    error_code result = no_error;
    char *device_name;
	
    PI_getan(&device, &device_type, 1);
    PI_getan(&arg, &arg_type, 2);

#ifdef SSIWS_UNIKEY
	switch (device_type) {
	case PI_SYM:
		device_name = PI_getsymname(NULL, device, 0);
		break;
	case PI_UIA: 
		device_name = PI_getuianame(NULL, device, 0);
		break;
	default:
		result = device_name_error;
		break;
	}
#endif
	if (result == no_error) result = init_hardware_key(device_name);
	if (result == no_error) result = check_hardware_key();
	if (result == no_error) result = shutdown_hardware_key();

	PI_makesym(&code, &code_type, result_symbols[result]);
    
    return PI_unify(arg, arg_type, code, code_type);
}

PI_BEGIN
    PI_DEFINE("check_security", 2, check_security)
PI_END

void pi_init(void)
{
    PI_INIT;
}
