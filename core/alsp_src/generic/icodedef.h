ICODE(I_START_CAPTURE,	"I_START_CAPTURE", ic_start_capture,	FR(U,U,U,U))
ICODE(I_END_CAPTURE,	"I_END_CAPTURE", ic_end_capture,	FR(U,U,U,U))
ICODE(I_CALLINFO,	"I_CALLINFO", ic_callinfo,	FR(L,W,W,U))

ICODE(I_MOVE,	"I_MOVE", 	ic_move,	FR(B,W,B,W))
ICODE(I_ADDTOSP,	"I_ADDTOSP", ic_addtosp,	FR(W,U,U,U))

ICODE(I_CALL,	"I_CALL", 	ic_call,	FR(X,W,U,U))
ICODE(I_EXECUTE,	"I_EXECUTE", ic_execute,	FR(X,W,U,U))
ICODE(I_PROCEED,	"I_PROCEED", ic_proceed,	FR(B,W,U,U))
ICODE(I_INLINE_PROCEED,	"I_INLINE_PROCEED", ic_inline_proceed,FR(U,U,U,U))
ICODE(I_ALLOCATE1,	"I_ALLOCATE1", ic_allocate1,	FR(W,U,U,U))
ICODE(I_ENDALLOC1,	"I_ENDALLOC1", ic_endallocate1,	FR(U,U,U,U))
ICODE(I_ALLOCATE,	"I_ALLOCATE", ic_allocate,	FR(W,U,U,U))
ICODE(I_DEALLOCATE1,	"I_DEALLOCATE1", ic_deallocate1,	FR(U,U,U,U))
ICODE(I_DEALLOCATE2,	"I_DEALLOCATE2", ic_deallocate2,	FR(W,U,U,U))
ICODE(I_DEALLOCATE3,	"I_DEALLOCATE3", ic_deallocate3,	FR(U,U,U,U))
ICODE(I_DEALLOCATE4,	"I_DEALLOCATE4", ic_deallocate4,	FR(W,U,U,U))
ICODE(I_TRIM,	"I_TRIM", 	ic_trim,	FR(W,W,B,U))
ICODE(I_INIT_YVAR1,	"I_INIT_YVAR1", ic_init_yvar1,	FR(B,W,U,U))
ICODE(I_INIT_YVAR2,	"I_INIT_YVAR2", ic_init_yvar2,	FR(W,U,U,U))

ICODE(I_CUT_PROCEED,	"I_CUT_PROCEED", ic_cut_proceed,	FR(B,W,U,U))
ICODE(I_DEALLOCATE_CUT_PROCEED,"I_DEALLOCATE_CUT_PROCEED", ic_deallocate_cut_proceed,FR(U,U,U,U))
ICODE(I_DOCUT,	"I_DOCUT", 	ic_docut,	FR(B,W,U,U))
ICODE(I_CUTMACRO,	"I_CUTMACRO", ic_cutmacro,	FR(B,W,B,W))

ICODE(I_G_SYM,	"I_G_SYM", 	ic_g_sym,	FR(X,B,W,U))
ICODE(I_G_INT,	"I_G_INT", 	ic_g_int,	FR(L,B,W,U))
ICODE(I_G_VALUE,	"I_G_VALUE", ic_g_value,	FR(B,W,B,W))
ICODE(I_G_LIST,	"I_G_LIST", 	ic_g_list,	FR(B,W,U,U))
ICODE(I_G_STRUCTURE,	"I_G_STRUCTURE", ic_g_structure,	FR(X,W,B,W))
ICODE(I_G_UIA,	"I_G_UIA", 	ic_g_uia,	FR(S,B,W,U))

ICODE(I_P_UNSAFE,	"I_P_UNSAFE", ic_p_unsafe,	FR(B,W,B,W))
ICODE(I_P_INT,	"I_P_INT", 	ic_p_int,	FR(L,B,W,U))
ICODE(I_P_SYM,	"I_P_SYM", 	ic_p_sym,	FR(X,B,W,U))
ICODE(I_P_YVAR,	"I_P_YVAR", 	ic_p_yvar,	FR(B,W,B,W))
ICODE(I_P_XVAR,	"I_P_XVAR", 	ic_p_xvar,	FR(B,W,U,U))
ICODE(I_P_LIST,	"I_P_LIST", 	ic_p_list,	FR(B,W,U,U))
ICODE(I_P_STRUCTURE,	"I_P_STRUCTURE", ic_p_structure,	FR(X,W,B,W))
ICODE(I_P_UIA,	"I_P_UIA", 	ic_p_uia,	FR(S,B,W,U))


ICODE(I_U_SYM,	"I_U_SYM", 	ic_u_sym,	FR(X,W,U,U))
ICODE(I_U_INT,	"I_U_INT", 	ic_u_int,	FR(L,W,U,U))
ICODE(I_U_VAR,	"I_U_VAR", 	ic_u_var,	FR(B,W,W,B))
ICODE(I_U_VAL,	"I_U_VAL", 	ic_u_val,	FR(B,W,W,U))
ICODE(I_U_LVAL,	"I_U_LVAL", 	ic_u_lval,	FR(B,W,W,U))
ICODE(I_U_VOID,	"I_U_VOID", 	ic_u_void,	FR(W,U,U,U))

ICODE(I_ENDSTRUCT,	"I_ENDSTRUCT", ic_endstruct,	FR(U,U,U,U))

#ifdef InMath

#ifdef NewMath
ICODE(I_MTH_INIT1,	"I_MTH_INIT1", ic_mth_init1,	FR(B,L,U,U))
ICODE(I_MTH_INIT2,	"I_MTH_INIT2", ic_mth_init2,	FR(L,W,U,U))
ICODE(I_MTH_FIN,	"I_MTH_FIN", ic_mth_fin,	FR(U,U,U,U))
#else	/* NewMath */
ICODE(I_MTH_INIT,	"I_MTH_INIT", ic_mth_init,	FR(B,U,U,U))
#endif	/* NewMath */

ICODE(I_MTH_EQ,	"I_MTH_EQ", 	ic_mth_eq,	FR(U,U,U,U))
ICODE(I_MTH_LT,	"I_MTH_LT", 	ic_mth_lt,	FR(U,U,U,U))
ICODE(I_MTH_GT,	"I_MTH_GT", 	ic_mth_gt,	FR(U,U,U,U))
ICODE(I_MTH_LE,	"I_MTH_LE", 	ic_mth_le,	FR(U,U,U,U))
ICODE(I_MTH_GE,	"I_MTH_GE", 	ic_mth_ge,	FR(U,U,U,U))
ICODE(I_MTH_NE,	"I_MTH_NE", 	ic_mth_ne,	FR(U,U,U,U))

ICODE(I_MTH_GETVAL,	"I_MTH_GETVAL", ic_mth_getval,	FR(B,W,U,U))
#ifndef NewMath
ICODE(I_MTH_GETINT,	"I_MTH_GETINT", ic_mth_getint,	FR(B,W,U,U))
ICODE(I_MTH_PUTINT,	"I_MTH_PUTINT", ic_mth_putint,	FR(B,W,U,U))
#else
ICODE(I_MTH_GETNUM,	"I_MTH_GETNUM", ic_mth_getnum,	FR(B,W,U,U))
ICODE(I_MTH_PUTNUM,	"I_MTH_PUTNUM", ic_mth_putnum,	FR(B,W,U,U))
ICODE(I_MTH_PUSHDBL,	"I_MTH_PUSHDBL", ic_mth_pushdbl,	FR(L,L,U,U))
ICODE(I_MTH_ADDI,	"I_MTH_ADDI", ic_mth_addi,	FR(L,U,U,U))
ICODE(I_MTH_SUBI,	"I_MTH_SUBI", ic_mth_subi,	FR(L,U,U,U))
ICODE(I_MTH_CALLOUT_INIT, "I_MTH_CALLOUT_INIT", ic_mth_callout_init, FR(U,U,U,U))
ICODE(I_MTH_CALLOUT,	"I_MTH_CALLOUT", ic_mth_callout,	FR(U,U,U,U))
#endif
ICODE(I_MTH_PUSHINT,	"I_MTH_PUSHINT", ic_mth_pushint,	FR(L,U,U,U))

ICODE(I_MTH_ADD,	"I_MTH_ADD", ic_mth_add,	FR(U,U,U,U))
ICODE(I_MTH_SUB,	"I_MTH_SUB", ic_mth_sub,	FR(U,U,U,U))
ICODE(I_MTH_MUL,	"I_MTH_MUL", ic_mth_mul,	FR(U,U,U,U))
ICODE(I_MTH_DIV,	"I_MTH_DIV", ic_mth_div,	FR(U,U,U,U))
ICODE(I_MTH_NEG,	"I_MTH_NEG", ic_mth_neg,	FR(U,U,U,U))
ICODE(I_MTH_MOD,	"I_MTH_MOD", ic_mth_mod,	FR(U,U,U,U))
ICODE(I_MTH_BAND,	"I_MTH_BAND", ic_mth_band,	FR(U,U,U,U))
ICODE(I_MTH_BOR,	"I_MTH_BOR", ic_mth_bor,	FR(U,U,U,U))
ICODE(I_MTH_BXOR,	"I_MTH_BXOR", ic_mth_bxor,	FR(U,U,U,U))
ICODE(I_MTH_NOT,	"I_MTH_NOT", ic_mth_not,	FR(U,U,U,U))
ICODE(I_MTH_LSHFT,	"I_MTH_LSHFT", ic_mth_lshft,	FR(U,U,U,U))
ICODE(I_MTH_RSHFT,	"I_MTH_RSHFT", ic_mth_rshft,	FR(U,U,U,U))

#ifdef FMath
ICODE(I_MTH_POWER,	"I_MTH_POWER", ic_mth_power,	FR(U,U,U,U))
ICODE(I_MTH_ABS,	"I_MTH_ABS", ic_mth_abs,	FR(U,U,U,U))
ICODE(I_MTH_SIN,	"I_MTH_SIN", ic_mth_sin,	FR(U,U,U,U))
ICODE(I_MTH_COS,	"I_MTH_COS", ic_mth_cos,	FR(U,U,U,U))
ICODE(I_MTH_TAN,	"I_MTH_TAN", ic_mth_tan,	FR(U,U,U,U))
ICODE(I_MTH_ASIN,	"I_MTH_ASIN", ic_mth_asin,	FR(U,U,U,U))
ICODE(I_MTH_ACOS,	"I_MTH_ACOS", ic_mth_acos,	FR(U,U,U,U))
ICODE(I_MTH_ATAN,	"I_MTH_ATAN", ic_mth_atan,	FR(U,U,U,U))
ICODE(I_MTH_SQRT,	"I_MTH_SQRT", ic_mth_sqrt,	FR(U,U,U,U))
ICODE(I_MTH_EXP,	"I_MTH_EXP", ic_mth_exp,	FR(U,U,U,U))
ICODE(I_MTH_EXP10,	"I_MTH_EXP10", ic_mth_exp10,	FR(U,U,U,U))
ICODE(I_MTH_LOG,	"I_MTH_LOG", ic_mth_log,	FR(U,U,U,U))
ICODE(I_MTH_LOG10,	"I_MTH_LOG10", ic_mth_log10,	FR(U,U,U,U))
ICODE(I_MTH_FLOOR,	"I_MTH_FLOOR", ic_mth_floor,	FR(U,U,U,U))
ICODE(I_MTH_ROUND,	"I_MTH_ROUND", ic_mth_round,	FR(U,U,U,U))
ICODE(I_MTH_TRUNC,	"I_MTH_TRUNC", ic_mth_trunc,	FR(U,U,U,U))
ICODE(I_MTH_FDIV,	"I_MTH_FDIV", ic_mth_fdiv,	FR(U,U,U,U))
ICODE(I_MTH_HEAPUSED,	"I_MTH_HEAPUSED", ic_mth_heapused,FR(U,U,U,U))
ICODE(I_MTH_CPUTIME,	"I_MTH_CPUTIME", ic_mth_cputime,	FR(U,U,U,U))
ICODE(I_MTH_REALTIME,	"I_MTH_REALTIME", ic_mth_realtime,FR(U,U,U,U))
ICODE(I_MTH_RANDOM,	"I_MTH_RANDOM", ic_mth_random,	FR(U,U,U,U))
ICODE(I_MTH_ATAN2,	"I_MTH_ATAN2",	ic_mth_atan2,	FR(U,U,U,U))
ICODE(I_MTH_CEIL,	"I_MTH_CEIL",	ic_mth_ceil,	FR(U,U,U,U))
ICODE(I_MTH_COSH,	"I_MTH_COSH",	ic_mth_cosh,	FR(U,U,U,U))
ICODE(I_MTH_ERF,	"I_MTH_ERF",	ic_mth_erf,	FR(U,U,U,U))
ICODE(I_MTH_ERFC,	"I_MTH_ERFC",	ic_mth_erfc,	FR(U,U,U,U))
ICODE(I_MTH_FMOD,	"I_MTH_FMOD",	ic_mth_fmod,	FR(U,U,U,U))
ICODE(I_MTH_GAMMA,	"I_MTH_GAMMA",	ic_mth_gamma,	FR(U,U,U,U))
ICODE(I_MTH_HYPOT,	"I_MTH_HYPOT",	ic_mth_hypot,	FR(U,U,U,U))
ICODE(I_MTH_J0,		"I_MTH_J0",	ic_mth_j0,	FR(U,U,U,U))
ICODE(I_MTH_J1,		"I_MTH_J1",	ic_mth_j1,	FR(U,U,U,U))
ICODE(I_MTH_JN,		"I_MTH_JN",	ic_mth_jn,	FR(U,U,U,U))
ICODE(I_MTH_SINH,	"I_MTH_SINH",	ic_mth_sinh,	FR(U,U,U,U))
ICODE(I_MTH_TANH,	"I_MTH_TANH",	ic_mth_tanh,	FR(U,U,U,U))
ICODE(I_MTH_Y0,		"I_MTH_Y0",	ic_mth_y0,	FR(U,U,U,U))
ICODE(I_MTH_Y1,		"I_MTH_Y1",	ic_mth_y1,	FR(U,U,U,U))
ICODE(I_MTH_YN,		"I_MTH_YN",	ic_mth_yn,	FR(U,U,U,U))

#endif /* FMath */

#endif /* InMath */

#ifdef DoubleType
ICODE(I_G_DBL,	"I_G_DBL", 	ic_g_dbl,	FR(L,L,B,W))
ICODE(I_P_DBL,	"I_P_DBL", 	ic_p_dbl,	FR(L,L,B,W))
#endif /* DoubleType */

