#define WAIT	{ic_put(0x9b);}

#define FADDI(r1,disp)	{ic_put(0xda); icPutAddrMode(0x00,r1,disp); }
#define FADDF(r1,disp)	{ic_put(0xdc); icPutAddrMode(0x00,r1,disp); }

#define FSUBI(r1,disp)	{ic_put(0xda); icPutAddrMode(0x05,r1,disp); }
#define FSUBF(r1,disp)	{ic_put(0xdc); icPutAddrMode(0x05,r1,disp); }

#define FMULI(r1,disp)	{ic_put(0xda); icPutAddrMode(0x01,r1,disp); }
#define FMULF(r1,disp)	{ic_put(0xdc); icPutAddrMode(0x01,r1,disp); }

#define FDIVI(r1,disp)	{ic_put(0xda); icPutAddrMode(0x07,r1,disp); }
#define FDIVF(r1,disp)	{ic_put(0xdc); icPutAddrMode(0x07,r1,disp); }

#define FSIN	{ic_put(0xd9); ic_put(0xfe);}
#define FCOS	{ic_put(0xd9); ic_put(0xff);}
#define FPTAN	{ic_put(0xd9); ic_put(0xf2);}
#define FPATAN	{ic_put(0xd9); ic_put(0xf3);}
