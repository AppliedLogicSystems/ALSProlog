#define TK_CALL         	1	/* call */
#define TK_OCALL        	2	/* callWithDelayedInterrupt */
#define TK_DBG_CALL     	3	/* dbg_call */
#define TK_SCOLON3      	4	/* $semicolon */
#define TK_COMMA3       	5	/* $comma */
#define TK_IFARROW3     	6	/* $arrow */
#define TK_SCOLON       	7	/* ; */
#define TK_COMMA        	8	/* , */
#define TK_COLON        	9	/* : */
#define TK_VBAR         	10	/* | */
#define TK_IFARROW      	11	/* -> */
#define TK_CUT          	12	/* ! */
#define TK_IS           	13	/* is */
#define TK_LESS         	14	/* < */
#define TK_GRT          	15	/* > */
#define TK_LEQ          	16	/* =< */
#define TK_GEQ          	17	/* >= */
#define TK_ZEBRA        	18	/* =:= */
#define TK_ZEBRA2       	19	/* =\= */
#define TK_HEAPUSED     	20	/* heapused */
#define TK_CPUTIME      	21	/* cputime */
#define TK_REALTIME     	22	/* realtime */
#define TK_RANDOM       	23	/* random */
#define TK_NOT          	24	/* not */
#define TK_BACKSLASH    	25	/* \ */
#define TK_ABS          	26	/* abs */
#define TK_SIN          	27	/* sin */
#define TK_SINH         	28	/* sinh */
#define TK_COS          	29	/* cos */
#define TK_COSH         	30	/* cosh */
#define TK_TAN          	31	/* tan */
#define TK_TANH         	32	/* tanh */
#define TK_ASIN         	33	/* asin */
#define TK_ACOS         	34	/* acos */
#define TK_ATAN         	35	/* atan */
#define TK_SQRT         	36	/* sqrt */
#define TK_EXP          	37	/* exp */
#define TK_EXP10        	38	/* exp10 */
#define TK_LOG          	39	/* log */
#define TK_LOG10        	40	/* log10 */
#define TK_FLOOR        	41	/* floor */
#define TK_ROUND        	42	/* round */
#define TK_CEIL         	43	/* ceil */
#define TK_ERF          	44	/* erf */
#define TK_ERFC         	45	/* erfc */
#define TK_GAMMA        	46	/* gamma */
#define TK_J0           	47	/* j0 */
#define TK_J1           	48	/* j1 */
#define TK_Y0           	49	/* y0 */
#define TK_Y1           	50	/* y1 */
#define TK_TRUNC        	51	/* trunc */
#define TK_PLUS         	52	/* + */
#define TK_MINUS        	53	/* - */
#define TK_ATAN2        	54	/* atan2 */
#define TK_FMOD         	55	/* fmod */
#define TK_HYPOT        	56	/* hypot */
#define TK_JN           	57	/* jn */
#define TK_YN           	58	/* yn */
#define TK_STAR         	59	/* * */
#define TK_SLASHSLASH   	60	/* // */
#define TK_DIV          	61	/* div */
#define TK_MOD          	62	/* mod */
#define TK_BAND         	63	/* /\ */
#define TK_BOR          	64	/* \/ */
#define TK_BXOR         	65	/* xor */
#define TK_LSHFT        	66	/* << */
#define TK_RSHFT        	67	/* >> */
#define TK_SLASH        	68	/* / */
#define TK_IDCL         	69	/* :: */
#define TK_INE          	70	/* <> */
#define TK_IAEQ         	71	/* @= */
#define TK_ISBI         	72	/* <= */
#define TK_IBIP         	73	/* => */
#define TK_ISTG         	74	/* |= */
#define TK_IETG         	75	/* =| */
#define TK_IBNG         	76	/* ~ */
#define TK_2ST          	77	/* ** */
#define TK_HAT          	78	/* ^ */
#define TK_XFX          	79	/* xfx */
#define TK_XFY          	80	/* xfy */
#define TK_YFX          	81	/* yfx */
#define TK_FX           	82	/* fx */
#define TK_FY           	83	/* fy */
#define TK_XF           	84	/* xf */
#define TK_YF           	85	/* yf */
#define TK_RIF          	86	/* :- */
#define TK_QUEST        	87	/* ?- */
#define TK_PRW          	88	/* --> */
#define TK_EQ           	89	/* = */
#define TK_UNIV         	90	/* =.. */
#define TK_ID           	91	/* == */
#define TK_UID          	92	/* \== */
#define TK_ALS          	93	/* @< */
#define TK_AGR          	94	/* @> */
#define TK_ALEQ         	95	/* @=< */
#define TK_AGEQ         	96	/* @>= */
#define TK_DOT          	97	/* . */
#define TK_LBRAC        	98	/* [ */
#define TK_RBRAC        	99	/* ] */
#define TK_LPAREN       	100	/* ( */
#define TK_RPAREN       	101	/* ) */
#define TK_LISTING      	102	/* listing */
#define TK_CONSULT      	103	/* consult */
#define TK_RECONSULT    	104	/* reconsult */
#define TK_NEQ          	105	/* \= */
#define TK_USER         	106	/* user */
#define TK_BUILTINS     	107	/* builtins */
#define TK_NIL          	108	/* [] */
#define TK_MODULE       	109	/* module */
#define TK_USE          	110	/* use */
#define TK_ENDMOD       	111	/* endmod */
#define TK_EXPORT       	112	/* export */
#define TK_LCURLY       	113	/* { */
#define TK_RCURLY       	114	/* } */
#define TK_CURLYS       	115	/* {} */
#define TK_SQUOTE       	116	/* ' */
#define TK_UNDERSCORE   	117	/* _ */
#define TK_DBREF        	118	/* $dbref */
#define TK_DDOUBLE      	119	/* $double */
#define TK_NOTAGAIN     	120	/* \+ */
#define TK_EXPAND       	121	/* expand */
#define TK_ALSCD        	122	/* als$cd */
#define TK_STREAM_DESCRIPTOR	123	/* stream_descriptor */
#define TK_SYMBOL       	124	/* symbol */
#define TK_QSYMBOL      	125	/* qsymbol */
#define TK_FUNCTOR      	126	/* functor */
#define TK_STRING       	127	/* string */
#define TK_LONG_STRING  	128	/* long_string */
#define TK_LONG_SYMBOL  	129	/* long_symbol */
#define TK_LONG_QSYMBOL 	130	/* long_qsymbol */
#define TK_VAR          	131	/* var */
#define TK_REAL         	132	/* real */
#define TK_INTEGER      	133	/* integer */
#define TK_BOOLEAN      	134	/* boolean */
#define TK_UINTEGER     	135	/* uinteger */
#define TK_FULLSTOP     	136	/* fullstop */
#define TK_LEXERR       	137	/* lexerr */
#define TK_COMMENT      	138	/* comment */
#define TK_BYTE         	139	/* byte */
#define TK_UBYTE        	140	/* ubyte */
#define TK_CHAR         	141	/* char */
#define TK_UCHAR        	142	/* uchar */
#define TK_SHORT        	143	/* short */
#define TK_USHORT       	144	/* ushort */
#define TK_RSHORT       	145	/* rshort */
#define TK_RUSHORT      	146	/* rushort */
#define TK_INT          	147	/* int */
#define TK_UINT         	148	/* uint */
#define TK_LONG         	149	/* long */
#define TK_ULONG        	150	/* ulong */
#define TK_FLOAT        	151	/* float */
#define TK_DOUBLE       	152	/* double */
#define TK_FAR          	153	/* dosfar */
#define TK_PREPROC      	154	/* preproc */
#define TK_POUND        	155	/* # */
#define TK_EOLN         	156	/* eoln */
#define TK_LINEINFO     	157	/* lineinfo */
#define TK_ATOM_LENGTH  	158	/* atom_length */
#define TK_ATOM         	159	/* atom */
#define TK_BODY         	160	/* body */
#define TK_CALLABLE     	161	/* callable */
#define TK_CHARACTER    	162	/* character */
#define TK_COMPOUND     	163	/* compound */
#define TK_CONSTANT     	164	/* constant */
#define TK_LIST         	165	/* list */
#define TK_NUMBER       	166	/* number */
#define TK_VARIABLE     	167	/* variable */
#define TK_CHARACTER_CODE_LIST	168	/* character_code_list */
#define TK_CHARACTER_LIST	169	/* character_list */
#define TK_CLOSE_OPTION 	170	/* close_option */
#define TK_FLAG_VALUE   	171	/* flag_value */
#define TK_IO_MODE      	172	/* IO_MODE */
#define TK_NOT_LESS_THAN_ZERO	173	/* not_less_than_zero */
#define TK_OPERATOR_PRIORITY	174	/* operator_priority */
#define TK_OPERATOR_SPECIFIER	175	/* operator_specifier */
#define TK_PROLOG_FLAG  	176	/* prolog_flag */
#define TK_READ_OPTION  	177	/* read_option */
#define TK_SOURCE_SINK  	178	/* source_sink */
#define TK_STREAM_OR_ALIAS	179	/* stream_or_alias */
#define TK_STREAM_OPTION	180	/* stream_option */
#define TK_STREAM_POSITION	181	/* stream_position */
#define TK_WRITE_OPTION 	182	/* write_option */
#define TK_OPERATOR     	183	/* operator */
#define TK_PAST_END_OF_STREAM	184	/* past_end_of_stream */
#define TK_PROCEDURE    	185	/* procedure */
#define TK_STATIC_PROCEDURE	186	/* static_procedure */
#define TK_STREAM       	187	/* stream */
#define TK_ACCESS_CLAUSE	188	/* access_clause */
#define TK_CREATE       	189	/* create */
#define TK_INPUT        	190	/* input */
#define TK_MODIFY       	191	/* modify */
#define TK_OPEN         	192	/* open */
#define TK_OUTPUT       	193	/* output */
#define TK_REPOSITION   	194	/* reposition */
#define TK_CHARACTER_CODE	195	/* character_code */
#define TK_EXCEEDED_MAX_ARITY	196	/* exceeded_max_arity */
#define TK_FLAG         	197	/* flag */
#define TK_DELAY        	198	/* $delay */
#define TK_INTVL        	199	/* intvl */
#define TK_CMBVARS      	200	/* $combine_dvars */
#define TK_OVERFLOW     	201	/* overflow */
#define TK_UNDERFLOW    	202	/* underflow */
#define TK_ZERO_DIVIDE  	203	/* zero_divide */
#define TK_UNDEFINED    	204	/* undefined */
#define TK_ERROR        	205	/* error */
#define TK_INSTANTIATION_ERROR	206	/* instantiation_error */
#define TK_TYPE_ERROR   	207	/* type_error */
#define TK_DOMAIN_ERROR 	208	/* domain_error */
#define TK_EXISTENCE_ERROR	209	/* existence_error */
#define TK_PERMISSION_ERROR	210	/* permission_error */
#define TK_REPRESENTATION_ERROR	211	/* representation_error */
#define TK_CALCULATION_ERROR	212	/* calculation_error */
#define TK_RESOURCE_ERROR	213	/* resource_error */
#define TK_SYNTAX_ERROR 	214	/* syntax_error */
#define TK_SYSTEM_ERROR 	215	/* system_error */
#define TK_CHAR_CODE    	216	/* char_code */
#define TK_ATOM_CHARS   	217	/* atom_chars */
#define TK_ATOM_CODES   	218	/* atom_codes */
#define TK_NUMBER_CHARS 	219	/* number_chars */
#define TK_NUMBER_CODES 	220	/* number_codes */
#define TK_SNR          	221	/* stream_not_ready */
#define TK_UNEQUAL      	222	/* unequal */
#define TK_EQUAL        	223	/* equal */
#define TK_GREATEREQ    	224	/* greatereq */
#define TK_HIGHER       	225	/* higher */
#define TK_ADD          	226	/* add */
#define TK_BEGIN_TOG    	227	/* begin_tog */
#define TK_FINISH_TOG   	228	/* finish_tog */
#define TK_INF          	229	/* inf */
#define TK_J_LESS       	230	/* j_less */
#define TK_K_EQUAL      	231	/* k_equal */
#define TK_LUB          	232	/* lub */
#define TK_MUL          	233	/* mul */
#define TK_NARROWER     	234	/* narrower */
#define TK_OR           	235	/* or */
#define TK_POW_ODD      	236	/* pow_odd */
#define TK_QPOW_EVEN    	237	/* qpow_even */
#define TK_ROOTSQUARE   	238	/* rootsquare */
#define TK_VABS         	239	/* vabs */
#define TK_WRAP         	240	/* wrap */
#define TK_XP           	241	/* xp */
#define TK_PI           	242	/* pi */
#define TK_PI_2         	243	/* cnst_pi_half */
#define TK_E            	244	/* e */
#define TK_PI_4         	245	/* cnst_pi_4 */
#define TK_1_PI         	246	/* cnst_1_pi */
#define TK_2_PI         	247	/* cnst_2_pi */
#define TK_2_SQRTPI     	248	/* cnst_2sqrtpi */
#define TK_LOG2E        	249	/* cnst_log2e */
#define TK_LOG10E       	250	/* cnst_log10e */
#define TK_LN2          	251	/* cnst_ln2 */
#define TK_LN10         	252	/* cnst_ln10 */
#define TK_SQRT2        	253	/* cnst_sqrt2 */
#define TK_SQRT1_2      	254	/* cnst_sqrt1_2 */
#define TK_EOF          	255	/* end_of_file */
