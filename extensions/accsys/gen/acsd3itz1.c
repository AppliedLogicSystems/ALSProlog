/*================================================================
          acsd3itz1.c
          --Generated from: acsd3itz.src
          Date: 95/12/6   Time: 19:53:16
		--by ALS Interface Generator

 *===============================================================*/

#include "defs.h"
#include "cinterf.h"
#include "accsys.h"


CI_BEGARRAY(fieldsTAG)
  CI_FIELD("tagid",tagid,TAG,1,""),
  CI_ARRAYFIELD("tagnm",tagnm,TAG,6,"",11),
  CI_FIELD("keytype",keytype,TAG,1,""),
  {0}
CI_ENDARRAY

void acsd3itz2_init(void)
{
  CI_CTYPEDEF("TAG_PTR",TAG_PTR,5)
  CI_STRUCT("TAG",TAG,fieldsTAG)
  CI_CTYPEDEF("NDX",NDX,5)
  CI_CTYPEDEF("IDX",IDX,5)
  CI_CTYPEDEF("MDX",MDX,5)
  CI_CTYPEDEF("DBT",DBT,5)
  CI_CTYPEDEF("DBF",DBF,5)
  CI_CTYPEDEF("VOID_PTR",VOID_PTR,5)
  CI_CTYPEDEF("DOUBLE_PTR",DOUBLE_PTR,5)
  CI_CTYPEDEF("LONG_PTR",LONG_PTR,5)
  CI_CTYPEDEF("UINT_PTR",UINT_PTR,5)
  CI_CTYPEDEF("INT_PTR",INT_PTR,5)
  CI_CTYPEDEF("CHAR_PTR",CHAR_PTR,5)
  CI_CTYPEDEF("ptr",void *,5)
  CI_CTYPEDEF("double",double,13)
  CI_CTYPEDEF("float",float,12)
  CI_CTYPEDEF("short",short,8)
  CI_CTYPEDEF("char",char,6)
  CI_CTYPEDEF("long",long,3)
  CI_CTYPEDEF("int",int,1)
}
