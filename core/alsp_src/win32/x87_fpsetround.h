/* This is defined to match the rounding bit patterns of the x87 */
typedef enum {
    FP_RN,
    FP_RM,
    FP_RP,
    FP_RZ
} fp_rnd;

fp_rnd fpsetround(fp_rnd mode);
