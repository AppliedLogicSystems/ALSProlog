/*================================================================
          odbc1.c
          --Generated from: odbc.src
          Date: 96/4/25   Time: 14:41:55
		--by ALS Interface Generator

 *===============================================================*/

#include "alspi.h"
#include "cinterf.h"
#include "odbc.h"


CI_BEGARRAY(fieldsDATE_STRUCT)
  CI_FIELD("year",year,DATE_STRUCT,8,""),
  CI_FIELD("month",month,DATE_STRUCT,8,""),
  CI_FIELD("day",day,DATE_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsTIME_STRUCT)
  CI_FIELD("hour",hour,TIME_STRUCT,8,""),
  CI_FIELD("minute",minute,TIME_STRUCT,8,""),
  CI_FIELD("second",second,TIME_STRUCT,8,""),
  {0}
CI_ENDARRAY

CI_BEGARRAY(fieldsTIMESTAMP_STRUCT)
  CI_FIELD("year",year,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("month",month,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("day",day,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("hour",hour,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("minute",minute,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("second",second,TIMESTAMP_STRUCT,8,""),
  CI_FIELD("fraction",fraction,TIMESTAMP_STRUCT,3,""),
  {0}
CI_ENDARRAY

void odbc2_init(void)
{
  CI_CTYPEDEF("BOOKMARK",BOOKMARK,3)
  CI_STRUCT("TIMESTAMP_STRUCT",TIMESTAMP_STRUCT,fieldsTIMESTAMP_STRUCT)
  CI_STRUCT("TIME_STRUCT",TIME_STRUCT,fieldsTIME_STRUCT)
  CI_STRUCT("DATE_STRUCT",DATE_STRUCT,fieldsDATE_STRUCT)
  CI_CTYPEDEF("RETCODE",RETCODE,8)
  CI_CTYPEDEF("HSTMT",HSTMT,5)
  CI_CTYPEDEF("HDBC",HDBC,5)
  CI_CTYPEDEF("HENV",HENV,5)
  CI_CTYPEDEF("PTR",PTR,5)
  CI_CTYPEDEF("SFLOAT",SFLOAT,12)
  CI_CTYPEDEF("LDOUBLE",LDOUBLE,13)
  CI_CTYPEDEF("SDOUBLE",SDOUBLE,13)
  CI_CTYPEDEF("USHORT",USHORT,8)
  CI_CTYPEDEF("ULONG",ULONG,3)
  CI_CTYPEDEF("SSHORT",SSHORT,8)
  CI_CTYPEDEF("SLONG",SLONG,3)
  CI_CTYPEDEF("UWORD",UWORD,8)
  CI_CTYPEDEF("UDWORD",UDWORD,3)
  CI_CTYPEDEF("SWORD",SWORD,8)
  CI_CTYPEDEF("SDWORD",SDWORD,3)
  CI_CTYPEDEF("SCHAR",SCHAR,6)
  CI_CTYPEDEF("UCHAR",UCHAR,6)
  CI_CTYPEDEF("HWND",HWND,5)
  CI_CTYPEDEF("ptr",void *,5)
  CI_CTYPEDEF("double",double,13)
  CI_CTYPEDEF("float",float,12)
  CI_CTYPEDEF("short",short,8)
  CI_CTYPEDEF("char",char,6)
  CI_CTYPEDEF("long",long,3)
  CI_CTYPEDEF("int",int,1)
}
