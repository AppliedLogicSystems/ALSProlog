/*===================================================================*
 |		tokini.h		
 |	Copyright (c) 1990-1995 Applied Logic Systems, Inc.
 |
 |		-- tokens and operators
 |
 | Author: Kevin Buettner
 | Creation: 1985
 | 12/90 - K.Buettner -- reorganization to handle cutmacros differently
 *===================================================================*/

	/*------------------------------------------------------------------*
	 * The following tokens and operators are cutmacros.
	 * TK_CUT must be the last token which is a cutmacro.  There must
	 * not be any tokens which are not cutmacros appearing before
	 * the cutmacros.
	 *
	 * If it is necessary to change this, then cutmacro.h and cutmacro.c
	 * must also be changed.
	 *------------------------------------------------------------------*/
	
	TK(TK_CALL,"call"),
	TK(TK_OCALL,"callWithDelayedInterrupt"),
	TK(TK_DBG_CALL,"dbg_call"),	
	TK(TK_SCOLON3,"$semicolon"),		/* needs cutmacro ?? */
	TK(TK_COMMA3,"$comma"),				/* needs cutmacro ?? */
	TK(TK_IFARROW3,"$arrow"),			/* needs cutmacro ?? */
		OP(TK_SCOLON,";",  0,OP_XFY(1100)),
		OP(TK_COMMA,",", 0, OP_XFY(1000)),
		OP(TK_COLON,":",0,OP_XFY(950)),
		OP(TK_VBAR,"|",0,OP_XFY(1100)),	
		OP(TK_IFARROW,"->",0,OP_YFX(1050)),
	TK(TK_CUT,"!"),				

	/*
	 * Finished with the cutmacros
	 */
	
	/*------------------------------------------------------------------*
	 * The following tokens are arithmetic predicates.  We expect
	 * TK_IS to be the first and TK_ZEBRA2 to be the last.
	 *------------------------------------------------------------------*/
	
        OP(TK_IS,"is",0, OP_XFX(700)),
        OP(TK_LESS,"<",0,OP_XFX(700)),
        OP(TK_GRT,">",0,OP_XFX(700)),
        OP(TK_LEQ,"=<",0,OP_XFX(700)),
        OP(TK_GEQ,">=",0,OP_XFX(700)),
        OP(TK_ZEBRA,"=:=",0,OP_XFX(700)),
        OP(TK_ZEBRA2,"=\\=",0,OP_XFX(700)),

	/*------------------------------------------------------------------*
	 * The following tokens appear as symbols in arithmetic expressions
	 * TK_HEAPUSED is the first and TK_RANDOM is the last.
	 * 
	 * If the order of these are changed or any new arithmetic tokens
	 * added, the table of icode numbers in compmath.c must be changed.
	 *------------------------------------------------------------------*/

	TK(TK_HEAPUSED,"heapused"),
	TK(TK_CPUTIME,"cputime"),
	TK(TK_REALTIME,"realtime"),
	TK(TK_RANDOM,"random"),

	/*------------------------------------------------------------------*
	 * The following tokens appear as unary operators or functions in
	 * arithmetic expressions.  Note that there is an overlap between
	 * these and the binary operators.  The first is TK_NOT and
	 * the last is TK_TRUNC.
	 *------------------------------------------------------------------*/

        OP(TK_NOT,"not",OP_FY(900),0),
	TK(TK_BACKSLASH,"\\"),
	TK(TK_ABS,"abs"),
	TK(TK_SIN,"sin"),
	TK(TK_SINH,"sinh"),
	TK(TK_COS,"cos"),
	TK(TK_COSH,"cosh"),
	TK(TK_TAN,"tan"),
	TK(TK_TANH,"tanh"),
	TK(TK_ASIN,"asin"),
	TK(TK_ACOS,"acos"),
	TK(TK_ATAN,"atan"),
	TK(TK_SQRT,"sqrt"),
	TK(TK_EXP,"exp"),
	TK(TK_EXP10,"exp10"),
	TK(TK_LOG,"log"),
	TK(TK_LOG10,"log10"),
	TK(TK_FLOOR,"floor"),
	TK(TK_ROUND,"round"),
	TK(TK_CEIL,"ceil"),
	TK(TK_ERF,"erf"),
	TK(TK_ERFC,"erfc"),
	TK(TK_GAMMA,"gamma"),
	TK(TK_J0,"j0"),
	TK(TK_J1,"j1"),
	TK(TK_Y0,"y0"),
	TK(TK_Y1,"y1"),

	TK(TK_TRUNC,"trunc"),

	/*------------------------------------------------------------------*
	 * TK_PLUS and TK_MINUS may appear as both unary an binary operators
	 * in arithmetic expressions.
	 *------------------------------------------------------------------*/
	
        OP(TK_PLUS,"+",OP_FX(100),OP_YFX(500)),
        OP(TK_MINUS,"-",OP_FX(100),OP_YFX(500)),

	/*------------------------------------------------------------------*
	 * The following tokens appear as binary operators in arithmetic
	 * expressions.  This list begins with TK_PLUS above and ends with
	 * TK_HAT.
	 *------------------------------------------------------------------*/

    	TK(TK_ATAN2,"atan2"),
	TK(TK_FMOD,"fmod"),
	TK(TK_HYPOT,"hypot"),
	TK(TK_JN,"jn"),
	TK(TK_YN,"yn"),
        OP(TK_STAR,"*",0,OP_YFX(400)),
        OP(TK_SLASHSLASH,"//",0,OP_YFX(400)),
        OP(TK_DIV,"div",0,OP_YFX(400)),
        OP(TK_MOD,"mod",0,OP_YFX(300)),
        OP(TK_BAND,"/\\",0,OP_YFX(500)),
        OP(TK_BOR,"\\/",0,OP_YFX(500)),
		OP(TK_BXOR,"xor",0,OP_YFX(500)),
        OP(TK_LSHFT,"<<",0,OP_YFX(400)),
        OP(TK_RSHFT,">>",0,OP_YFX(400)),
        OP(TK_SLASH,"/",0,OP_YFX(400)),

		/* Operators specifically added for the
		   interval constraint subsystem
		 */
        OP(TK_IDCL, "::", 0, OP_XFX(700)),
        OP(TK_INE,  "<>", 0, OP_XFX(700)),
        OP(TK_IAEQ, "@=", 0, OP_XFX(700)),
        OP(TK_ISBI, "<=", 0, OP_XFX(600)),
        OP(TK_IBIP, "=>", 0, OP_XFX(600)),

        OP(TK_ISTG, "|=", 0, OP_XFX(720)),
        OP(TK_IETG, "=|", 0, OP_XFX(720)),

        OP(TK_IBNG, "~",  OP_FX(150), 0),

        OP(TK_2ST, "**", 0, OP_XFY(200)),
        OP(TK_HAT, "^",  0, OP_XFY(200)),

	/*------------------------------------------------------------------*
	 * Rest of the tokens
	 *------------------------------------------------------------------*/

	TK(TK_XFX,"xfx"),
	TK(TK_XFY,"xfy"),
	TK(TK_YFX,"yfx"),
	TK(TK_FX,"fx"),
	TK(TK_FY,"fy"),
	TK(TK_XF,"xf"),
	TK(TK_YF,"yf"),
        OP(TK_RIF,":-", OP_FX(1200), OP_XFX(1200)),
        OP(TK_QUEST,"?-", OP_FX(1200),0),
        OP(TK_PRW,"-->",0,OP_XFX(1200)), /* Production Arrow */
        OP(TK_EQ,"=",0, OP_XFX(700)),
        OP(TK_UNIV,"=..",0,OP_XFX(700)),
        OP(TK_ID,"==",0,OP_XFX(700)),
        OP(TK_UID,"\\==",0,OP_XFX(700)),
        OP(TK_ALS,"@<",0,OP_XFX(700)),
        OP(TK_AGR,"@>",0,OP_XFX(700)),
        OP(TK_ALEQ,"@=<",0,OP_XFX(700)),
        OP(TK_AGEQ,"@>=",0,OP_XFX(700)),
        OP(TK_DOT,".",0,OP_XFY(800)),
	TK(TK_LBRAC,"["),
	TK(TK_RBRAC,"]"),
	TK(TK_LPAREN,"("),
	TK(TK_RPAREN,")"),
	TK(TK_LISTING,"listing"),
	TK(TK_CONSULT,"consult"),
	TK(TK_RECONSULT,"reconsult"),
        OP(TK_NEQ,"\\=",0,OP_XFX(700)),
	TK(TK_USER,"user"),
	TK(TK_BUILTINS,"builtins"),
	TK(TK_NIL,"[]"),
		OP(TK_MODULE,"module",OP_FX(1200),0),
		OP(TK_USE,"use",OP_FX(1200),0),
	TK(TK_ENDMOD,"endmod"),
		OP(TK_EXPORT,"export",OP_FX(1200),0),
	TK(TK_LCURLY,"{"),
	TK(TK_RCURLY,"}"),
	TK(TK_CURLYS,"{}"),
	TK(TK_SQUOTE,"'"),
	TK(TK_UNDERSCORE,"_"),
	TK(TK_DBREF,"$dbref"),
	TK(TK_DDOUBLE,"$double"),
        OP(TK_NOTAGAIN,"\\+",OP_FY(900),0),
	TK(TK_EXPAND,"expand"),
	TK(TK_ALSCD,"als$cd"),
	TK(TK_STREAM_DESCRIPTOR,"stream_descriptor"),
	TK(TK_SYMBOL,"symbol"),
	TK(TK_QSYMBOL,"qsymbol"),
	TK(TK_FUNCTOR,"functor"),
	TK(TK_STRING,"string"),
	TK(TK_LONG_STRING,"long_string"),
	TK(TK_LONG_SYMBOL,"long_symbol"),
	TK(TK_LONG_QSYMBOL,"long_qsymbol"),
	TK(TK_VAR,"var"),
	TK(TK_REAL,"real"),
	TK(TK_INTEGER,"integer"),
	TK(TK_BOOLEAN,"boolean"),
	TK(TK_UINTEGER,"uinteger"),
	TK(TK_FULLSTOP,"fullstop"),
	TK(TK_LEXERR,"lexerr"),
	TK(TK_COMMENT,"comment"),
	TK(TK_BYTE,"byte"),
	TK(TK_UBYTE,"ubyte"),
	TK(TK_CHAR,"char"),
	TK(TK_UCHAR,"uchar"),
	TK(TK_SHORT,"short"),
	TK(TK_USHORT,"ushort"),
	TK(TK_RSHORT,"rshort"),
	TK(TK_RUSHORT,"rushort"),
	TK(TK_INT,"int"), 
	TK(TK_UINT,"uint"),
	TK(TK_LONG,"long"),
	TK(TK_ULONG,"ulong"),
	TK(TK_FLOAT,"float"),
	TK(TK_DOUBLE,"double"),
	TK(TK_FAR,"dosfar"),

	TK(TK_PREPROC,"preproc"),
	TK(TK_POUND,"#"),
	TK(TK_EOLN,"eoln"),
	TK(TK_LINEINFO,"lineinfo"),
	TK(TK_ATOM_LENGTH,"atom_length"),
	TK(TK_ATOM,"atom"),
	TK(TK_BODY,"body"),
	TK(TK_CALLABLE,"callable"),
	TK(TK_CHARACTER,"character"),
	TK(TK_COMPOUND,"compound"),
	TK(TK_CONSTANT,"constant"),
	TK(TK_LIST,"list"),
	TK(TK_NUMBER,"number"),
	TK(TK_VARIABLE,"variable"),
	TK(TK_CHARACTER_CODE_LIST,"character_code_list"),
	TK(TK_CHARACTER_LIST,"character_list"),
	TK(TK_CLOSE_OPTION,"close_option"),
	TK(TK_FLAG_VALUE,"flag_value"),
	TK(TK_IO_MODE,"IO_MODE"),
	TK(TK_NOT_LESS_THAN_ZERO,"not_less_than_zero"),
	TK(TK_OPERATOR_PRIORITY,"operator_priority"),
	TK(TK_OPERATOR_SPECIFIER,"operator_specifier"),
	TK(TK_PROLOG_FLAG,"prolog_flag"),
	TK(TK_READ_OPTION,"read_option"),
	TK(TK_SOURCE_SINK,"source_sink"),
	TK(TK_STREAM_OR_ALIAS,"stream_or_alias"),
	TK(TK_STREAM_OPTION,"stream_option"),
	TK(TK_STREAM_POSITION,"stream_position"),
	TK(TK_WRITE_OPTION,"write_option"),
	TK(TK_OPERATOR,"operator"),
	TK(TK_PAST_END_OF_STREAM,"past_end_of_stream"),
	TK(TK_PROCEDURE,"procedure"),
	TK(TK_STATIC_PROCEDURE,"static_procedure"),
	TK(TK_STREAM,"stream"),
	TK(TK_ACCESS_CLAUSE,"access_clause"),
	TK(TK_CREATE,"create"),
	TK(TK_INPUT,"input"),
	TK(TK_MODIFY,"modify"),
	TK(TK_OPEN,"open"),
	TK(TK_OUTPUT,"output"),
	TK(TK_REPOSITION,"reposition"),
	TK(TK_CHARACTER_CODE,"character_code"),
	TK(TK_EXCEEDED_MAX_ARITY,"exceeded_max_arity"),
	TK(TK_FLAG,"flag"),
	TK(TK_DELAY,"$delay"),
	TK(TK_INTVL,"intvl"),
	TK(TK_CMBVARS,"$combine_dvars"),
	TK(TK_OVERFLOW,"overflow"),
	TK(TK_UNDERFLOW,"underflow"),
	TK(TK_ZERO_DIVIDE,"zero_divide"),
	TK(TK_UNDEFINED,"undefined"),
	TK(TK_ERROR,"error"),
	TK(TK_INSTANTIATION_ERROR,"instantiation_error"),
	TK(TK_TYPE_ERROR,"type_error"),
	TK(TK_DOMAIN_ERROR,"domain_error"),
	TK(TK_EXISTENCE_ERROR,"existence_error"),
	TK(TK_PERMISSION_ERROR,"permission_error"),
	TK(TK_REPRESENTATION_ERROR,"representation_error"),
	TK(TK_CALCULATION_ERROR,"calculation_error"),
	TK(TK_RESOURCE_ERROR,"resource_error"),
	TK(TK_SYNTAX_ERROR,"syntax_error"),
	TK(TK_SYSTEM_ERROR,"system_error"),
	TK(TK_CHAR_CODE,"char_code"),
	TK(TK_ATOM_CHARS,"atom_chars"),
	TK(TK_ATOM_CODES,"atom_codes"),
	TK(TK_NUMBER_CHARS,"number_chars"),
	TK(TK_NUMBER_CODES,"number_codes"),
	TK(TK_SNR,"stream_not_ready"),

		/*------------------------------------------------------------------*
		 |	Tokens specifically added for the primitive operators of the
		 |	interval constraint subsystem;
		 |  	TK_COS, TK_SIN, TK_TAN are defined above;
		 *------------------------------------------------------------------*/

	TK(TK_UNEQUAL, "unequal"),
	TK(TK_EQUAL, "equal"),
	TK(TK_GREATEREQ, "greatereq"),
	TK(TK_HIGHER, "higher"),
	TK(TK_ADD, "add"),
	TK(TK_BEGIN_TOG, "begin_tog"),
	TK(TK_FINISH_TOG, "finish_tog"),
	TK(TK_INF, "inf"),
	TK(TK_J_LESS, "j_less"),
	TK(TK_K_EQUAL,  "k_equal"),
	TK(TK_LUB, "lub"),
	TK(TK_MUL, "mul"),
	TK(TK_NARROWER, "narrower"),
	TK(TK_OR, "or"),
	TK(TK_POW_ODD, "pow_odd"),
	TK(TK_QPOW_EVEN, "qpow_even"),
	TK(TK_ROOTSQUARE, "rootsquare"),
	TK(TK_VABS, "vabs"),
	TK(TK_WRAP, "wrap"),
	TK(TK_XP, "xp"),
/*        OP(TK_IDCL, "::", 0, OP_XFX(700)), */

		/*------------------------------------------------------------------*
		 |	Tokens specifically added for representing real number
		 |	constants for the interval constraint subsystem;
		 |	These are passed down symbolically as far as possible, and
		 |	then are given explicit floating point values as accurately
		 |	as possible (at the "last possible minute").  For each such
		 |	token added, there should be an entry in symbolic_constant/2
		 |	in the builtins file ra_basis.pro.
		 *------------------------------------------------------------------*/
	TK(TK_PI,   		"pi"),
	TK(TK_PI_2, 		"cnst_pi_half"),
	TK(TK_E,    		"e"),
	TK(TK_PI_4, 		"cnst_pi_4"),
	TK(TK_1_PI,  		"cnst_1_pi"),
	TK(TK_2_PI,  		"cnst_2_pi"),
	TK(TK_2_SQRTPI,		"cnst_2sqrtpi"),
	TK(TK_LOG2E, 		"cnst_log2e"),
	TK(TK_LOG10E,		"cnst_log10e"),
	TK(TK_LN2,  		"cnst_ln2"),
	TK(TK_LN10,			"cnst_ln10"),
	TK(TK_SQRT2,        "cnst_sqrt2"),
	TK(TK_SQRT1_2,      "cnst_sqrt1_2"),
	
	/*------------------------------------------------------------------*
	 * TK_EOF is the last token and must remain so.  If adding a token
	 * is necessary, it should be done before this point.  If it is
	 * absolutely necessary to add tokens beyond TK_EOF, then symtab.c
	 * must be changed as appropriate.  (Look at declaration of ts_next).
	 *------------------------------------------------------------------*/

	TK(TK_EOF,"end_of_file")
