char lx_chtb[256] = {
/* 00 nul | 01 soh  | 02 stx | 03 etx  | 04 eot | 05 enq  | 06 ack | 07 bel */
   LX_EOF,  LX_WSP,   LX_WSP,  LX_WSP,   LX_EOF,  LX_WSP,   LX_WSP,  LX_WSP,

/* 08 bs  | 09 ht   | 0a nl  | 0b vt   | 0c np  | 0d cr   | 0e so  | 0f si  */
   LX_WSP,  LX_WSP,   LX_NL ,  LX_WSP,   LX_WSP,  LX_NL ,   LX_WSP,  LX_WSP,

/* 10 dle | 11 dc1  | 12 dc2 | 13 dc3  | 14 dc4 | 15 nak  | 16 syn | 17 etb */
   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,

/* 18 can | 19 em   | 1a sub | 1b esc  | 1c fs  | 1d gs   | 1e rs  | 1f us  */
   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,   LX_WSP,  LX_WSP,

/* 20 sp  | 21 !    | 22 "   | 23 #    | 24 $   | 25 %    | 26 &   | 27 '   */
   LX_WSP,  LX_SNGL,  LX_DQT, LX_SPEC,  LX_LCAL, LX_CMNT,  LX_SPEC, LX_SQT, 

/* 28 (   | 29 )    | 2a *   | 2b +    | 2c ,   | 2d -    | 2e .   | 2f /   */
   LX_SNGL, LX_SNGL,  LX_SPEC, LX_SPEC,  LX_SNGL, LX_SPEC,  LX_SPEC, LX_SPEC, 

/* 30 0   | 31 1    | 32 2   | 33 3    | 34 4   | 35 5    | 36 6   | 37 7   */
   LX_NUM,  LX_NUM,   LX_NUM,  LX_NUM,   LX_NUM,  LX_NUM,   LX_NUM,  LX_NUM,

/* 38 8   | 39 9    | 3a :   | 3b ;    | 3c <   | 3d =    | 3e >   | 3f ?   */
   LX_NUM,  LX_NUM,   LX_SPEC, LX_SNGL,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, 

/* 40 @   | 41 A    | 42 B   | 43 C    | 44 D   | 45 E    | 46 F   | 47 G   */
   LX_SPEC, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,

/* 48 H   | 49 I    | 4a J   | 4b K    | 4c L   | 4d M    | 4e N   | 4f O   */
   LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,

/* 50 P   | 51 Q    | 52 R   | 53 S    | 54 T   | 55 U    | 56 V   | 57 W   */
   LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,  LX_UCAL, LX_UCAL,

/* 58 X   | 59 Y    | 5a Z   | 5b [    | 5c \   | 5d ]    | 5e ^   | 5f _   */
   LX_UCAL, LX_UCAL,  LX_UCAL, LX_SNGL,  LX_SPEC, LX_SNGL,  LX_SPEC, LX_UCAL, 

/* 60 `   | 61 a    | 62 b   | 63 c    | 64 d   | 65 e    | 66 f   | 67 g   */
   LX_SPEC, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,

/* 68 h   | 69 i    | 6a j   | 6b k    | 6c l   | 6d m    | 6e n   | 6f o   */
   LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,

/* 70 p   | 71 q    | 72 r   | 73 s    | 74 t   | 75 u    | 76 v   | 77 w   */
   LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,  LX_LCAL, LX_LCAL,

/* 78 x   | 79 y    | 7a z   | 7b {    | 7c |   | 7d }    | 7e ~   | 7f del */
   LX_LCAL, LX_LCAL,  LX_LCAL, LX_SNGL,  LX_SNGL, LX_SNGL,  LX_CHRQ, LX_SPEC, 

   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_SPEC,
   LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC,  LX_SPEC, LX_SPEC, LX_SPEC,  LX_EOF
};
