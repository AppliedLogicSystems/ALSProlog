#include <sys/systeminfo.h>

#include "alspi.h"

static long calculate_checksum(long n)
{
  long s, d1, d2, d3, d4, d5;
  
  s = n + 4328;
  
  d1 = (s % 10)/1;
  d2 = (s % 100)/10;
  d3 = (s % 1000)/100;
  d4 = (s % 10000)/1000;
  d5 = (s % 100000)/10000;
  
  s = d3*10000 + d5*1000 + d1*100 + d2*10 + d4;
  
  s = s * 5;
  
  s = s - n * 3;
  
  if (s < 0) s = -s;
  
  s = s % 100000;
  
  return s;
}

static int check_sysinfo_security(void)
{
    PWord arg;
    int arg_type;
    char *args, id[256];
    long test_sum, machine_id;
	
    PI_getan(&arg, &arg_type, 1);

    switch (arg_type) {
    case PI_SYM:
      args = PI_getsymname(NULL, arg, 0);
      break;
    case PI_UIA: 
      args = PI_getuianame(NULL, arg, 0);
      break;
    default:
      PI_FAIL;
      break;
    }

    sscanf(args, "%ld", &test_sum);

    sysinfo(SI_HW_SERIAL, id, 255);

    sscanf(id, "%ld", &machine_id);

    if (test_sum == calculate_checksum(machine_id)) PI_SUCCEED;
    else PI_FAIL;
}

PI_BEGIN
    PI_DEFINE("check_sysinfo_security", 1, check_sysinfo_security)
PI_END

void pi_init(void)
{
    PI_INIT;
}
