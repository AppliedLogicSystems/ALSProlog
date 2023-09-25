/*=========================================================================*
 |			built.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |			-- prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | 06/28/85	 - K. Buettner	-- Conversion to wam and compiled prolog
 | 09/12/85K - K. Buettner	-- arithmetic predicates moved to separate file.
 | 01/28/86K - K. Buettner	-- IBM PC conversion
 | 10/26/94	 - C. Houpt		-- Various UCHAR* casts.
 | ------------------------------------------------------------------------
 | Notes :
 |
 | Q. What do we mean by builtins ?
 | A. Procedures defined in C or assembly or even foreign that form
 |    the core of the system.
 |
 | Q. How do we connect a prolog call to a builtin and the corresponding
 |    C/assembly/foreign procedure ?
 | A. The prolog-name, memory address, and procedure-name of procedures
 |    defined in C are stored in an array called "blt_tab". For builtins
 |    defined in assembly, this information is stored in an array called
 |    "blt_tab2". For foreign procedures this information is stored in
 |    a local array called "pi_init_array".
 |
 |    At system init time, each builtin is loaded in the procedure
 |    name table and code is placed to call the C/assembly/foreign
 |    function whenever the corresponding prolog-predicate is invoked.
 |
 | Q. How are builtins packaged ?
 | A. First of all at packaging time we can identify a builtin
 |    from its name table entry flags. We have to generate relocation
 |    information for the absolute memory address of the builtin.
 |    In order to do this we need to be able to get the C/assembly
 |    procedure name from its address. This information is built up
 |    in a table called "blt_addr_tbl" at system init time by
 |    builtin_addr_table_init() and individual foreign initializations.
 *=========================================================================*/
#include "defs.h"
#include "wintcode.h"
#include "module.h"
#include "icodegen.h"

/*//extern Code *wm_cutaddr;*/

/* FIXME: Nuke PACKAGE stuff. */
#ifdef PACKAGE
extern
      pbi_get_pckg_error(), pbi_package_listasm_clause(), pbi_package_listasm_ntblentry(),
      pbi_package_mark_proc(), pbi_package_unmark_proc(), pbi_package_toktbl(),
      pbi_current_package(), pbi_get_default_proc(), pbi_get_default_use(),
      pbi_coff_operation();

#endif /* PACKAGE */

#ifdef arch_i386
#ifdef DOS

extern
      pbi_int86_9arg(void), pbi_int86_10arg(void), pbi_int86_16arg(void);

extern
      pbi_dos_first_match(void), pbi_dos_next_match(void);

#endif /* DOS */
#endif /* arch_i386 */

#ifdef MotorolaMath
extern	int	padd		( void );
extern	int	psub		( void );
extern	int	pmul		( void );
extern	int	pdiv		( void );
extern	int	pmac		( void );
extern	int	eiexp		( void );

extern	int	psin		( void );
extern	int	pcos		( void );
extern	int	ptan		( void );
extern	int	psinh		( void );
extern	int	pcosh		( void );
extern	int	ptanh		( void );
extern	int	pasin		( void );
extern	int	pacos		( void );
extern	int	patan		( void );
extern	int	plog10		( void );
extern	int	plog		( void );
extern	int	pexp		( void );
extern	int	psqrt		( void );
extern	int	pfloor		( void );
extern	int	pceil		( void );
extern	int	pfabs		( void );
extern	int	pgamma		( void );
extern	int	ppow		( void );
extern	int	pfmod		( void );
extern	int	patan2		( void );
extern	int	phypot		( void );
extern	int	perf		( void );
extern	int	perfc		( void );

extern	int	pc_add		( void );
extern	int	pc_sub		( void );
extern	int	pc_mul		( void );
extern	int	pcj_mul		( void );
extern	int	pc_mac		( void );
extern	int	pc_mag2		( void );
extern	int	pc_mag		( void );
extern	int	pc_conj		( void );
extern	int	pc_rec		( void );
extern	int	pc_div		( void );
extern	int	prc_mul		( void );
extern	int	prc_mac		( void );

extern	int	make_vector	( void );
extern	int	vrr_add		( void );
extern	int	vrr_sub		( void );
extern	int	vrr_mul		( void );
extern	int	vrr_div		( void );
extern	int	vrr_dot		( void );

extern	int	varg		( void );
extern	int	varg_first	( void );
extern	int	varg_last	( void );
extern	int	vlength		( void );

extern	int	vr_sum		( void );
extern	int	vrs_scale	( void );
extern	int	vr_mag2		( void );
extern	int	vr_neg		( void );
extern	int	vrsr_mul	( void );
extern	int	vr_select	( void );

extern	int	vcc_add		( void );
extern	int	vcc_sub		( void );
extern	int	vcc_mul		( void );
extern	int	vcc_div		( void );
extern	int	vcc_dot		( void );
extern	int	vrc_mul		( void );
extern	int	vcjc_dot	( void );
extern	int	vc_conj		( void );
extern	int	vc_neg		( void );
extern	int	vc_sum		( void );
extern	int	vcsc_mul	( void );
extern	int	vcsr_mul	( void );

extern	int	vc_cmag2	( void );
extern	int	vrc_mul		( void );

extern	int	vc_select	( void );

extern	int	lrr_dot		( void );
extern	int	lrr_mul		( void );
extern	int	lcc_dot		( void );
extern	int	lcc_add		( void );
extern	int	lcc_sub		( void );
extern	int	lcc_mul		( void );
extern	int	lsplit2		( void );

#endif /* MotorolaMath */

#define BLT(n,a,b,c) {n,a,b,c}

static struct blt_struct {
    const char *name;
    int   arity;
    int   (*blt) ( void );
    const char *bltname;
} blt_tab[] = {
	BLT("<", 2, pbi_less, "_pbi_less"),
	BLT("=:=", 2, pbi_arithequal, "_pbi_arithequal"),
	BLT("=<", 2, pbi_equalorless, "_pbi_equalorless"),
	BLT("=\\=", 2, pbi_arithnotequal, "_pbi_arithnotequal"),
	BLT(">", 2, pbi_greater, "_pbi_greater"),
	BLT(">=", 2, pbi_greaterorequal, "_pbi_greaterorequal"),
	BLT("is", 2, pbi_is, "_pbi_is"),
#ifdef HAVE_TIME
	BLT("$time", 10, pbi_time, "_pbi_time"),
#endif /* HAVE_TIME */
	BLT("srandom", 1, pbi_srandom, "_pbi_srandom"),
	BLT("fpconst_val", 2, pbi_fpconst_val, "_pbi_fpconst_val"),
	BLT("uia_poke_fpconst", 4, pbi_uia_poke_fpconst, "_pbi_uia_poke_fpconst"),
	BLT("$clauseinfo", 4, pbi_clauseinfo, "_pbi_clauseinfo"),
	BLT("$firstargkey", 2, pbi_firstargkey, "_pbi_firstargkey"),
	BLT("$resolve_module", 4, pbi_resolve_module, "_pbi_resolve_module"),
	BLT("$exported_proc", 3, pbi_exported_proc, "_pbi_exported_proc"),
	BLT("$icode", 5, pbi_icode, "_pbi_icode"),
#ifdef LIBBRK
	BLT("$libbreak", 4, pbi_libbreak, "_pbi_libbreak"),
#endif /* LIBBRK */
#ifndef KERNAL
	BLT("$listasm_clause", 1, pbi_listasm_clause, "_pbi_listasm_clause"),
	BLT("$listasm_ntblentry", 3, pbi_listasm_ntblentry,
	    "_pbi_listasm_ntblentry"),
#endif /* KERNAL */
	BLT("$next_module", 4, pbi_next_module, "_pbi_next_module"),
	BLT("$create_mod_close", 4, pbi_cr_mod_close, "_pbi_cr_mod_close"),
	BLT("$nextproc", 3, pbi_nextproc, "_pbi_nextproc"),
	BLT("$procinfo", 6, pbi_procinfo, "_pbi_procinfo"),
	BLT("abolish", 3, pbi_abolish, "_pbi_abolish"),
	BLT("abolish_clausegroup", 4, pbi_abolish_clausegroup,
	    "_pbi_abolish_clausegroup"),
		/* SPECIAL -- Debugging-related */
#ifdef DEBUGSYS
	BLT("cptx", 0, pbi_cptx, "_pbi_cptx"),
	BLT("display_heap", 2, disp_heap, "_disp_heap"),
	BLT("display_item_addr", 1, disp_item, "_disp_item"),
	BLT("swp_tr", 0, pbi_swp_tr, "_pbi_swp_tr"),
	BLT("walk_cps", 0, pbi_walk_cps, "_pbi_walk_cps"),
	BLT("cptz", 0, pbi_cptz, "_pbi_cptz"),
	BLT("x_display_heap", 2, x_disp_heap, "_x_disp_heap"),
	BLT("x_swp_tr", 0, pbi_x_swp_tr, "_pbi_x_swp_tr"),
#endif /* DEBUGSYS */
		/* SPECIAL -- Freeze-related */
#ifdef FREEZE
	BLT("cptx", 0, pbi_cptx, "_pbi_cptx"),
	BLT("clct_tr", 1, pbi_clct_tr, "_pbi_clct_tr"),
	BLT("unset_2nd", 1, pbi_unset_2nd, "_pbi_unset_2nd"),
	BLT("$delay", 4, pbi_delay, "_pbi_delay"),
	BLT("$is_delay_var", 1, pbi_is_delay_var, "_pbi_is_delay_var"),
	BLT("$kill_freeze", 1, pbi_kill_freeze, "_pbi_kill_freeze"),
	BLT("$delay_term_for", 2, pbi_del_tm_for, "_pbi_del_tm_for"),
	BLT("$bind_vars", 2, pbi_bind_vars, "_pbi_bind_vars"),
#endif /* FREEZE */

#ifdef INTCONSTR
	BLT("fuzz_float",  4, pbi_fuzz, "_pbi_fuzz"),
/*	BLT("$iter_link_net",  5, ilinknet, "_ilinknet"),  */
	BLT("reset_cstr_ctrs",  0, reset_cstr_ctrs, "_reset_cstr_ctrs"),
	BLT("get_cstr_ctrs",    2, get_cstr_ctrs_vals, "_get_cstr_ctrs_vals"),
	BLT("set_max_iters",    1, set_max_iters_val, "_set_max_iters_val"),
	BLT("x_int_op",    6, x_int_op, "_x_int_op"),

#if 0
	BLT("$run_grteq_cstrs",    5, run_grteq_cstrs, "_run_grteq_cstrs"),
#endif
#endif /* INTCONSTR */

#ifdef CONSTRDEBUG
	BLT("debug_constr0",  0, debugconstr, "_debugconstr"),
#endif /* CONSTRDEBUG */

#ifdef SCO_UNIX			/* procedure names must be < 32 chars */
	BLT("massively_abolish_clausegroup", 1,
	    pbi_massively_abolish_clausegroup,
	    "_pbi_massively_abolish_clausegro"),
#else
	BLT("massively_abolish_clausegroup", 1,
	    pbi_massively_abolish_clausegroup,
	    "_pbi_massively_abolish_clausegroup"),
#endif /* SCO_UNIX */
	BLT("asserta", 4, pbi_asserta, "_pbi_asserta"),
	BLT("assertz", 4, pbi_assertz, "_pbi_assertz"),
	BLT("addclause", 2, pbi_addclause, "_pbi_addclause"),
	BLT("$dynamic", 3, pbi_dynamic, "_pbi_dynamic"),
	BLT("execcommand", 1, pbi_execcommand, "_pbi_execcommand"),
	BLT("erase", 1, pbi_erase, "_pbi_erase"),
	BLT("index_proc", 3, pbi_index_proc, "_pbi_index_proc"),
	BLT("push_clausegroup", 1, pbi_push_clausegroup,
	    "_pbi_push_clausegroup"),
	BLT("pop_clausegroup", 1, pbi_pop_clausegroup, "_pbi_pop_clausegroup"),
#ifndef KERNAL
	BLT("collectcode", 0, pbi_collectcode, "_pbi_collectcode"),
#endif /* KERNAL */

#ifdef PACKAGE
	BLT("$get_pckg_error", 1, pbi_get_pckg_error, "_pbi_get_pckg_error"),
	BLT("$listasm_clause", 2, pbi_package_listasm_clause,
	    "_pbi_package_listasm_clause"),
	BLT("$listasm_ntblentry", 4, pbi_package_listasm_ntblentry,
	    "_pbi_package_listasm_ntblentry"),
	BLT("$package_mark_proc", 3, pbi_package_mark_proc,
	    "_pbi_package_mark_proc"),
	BLT("$package_unmark_proc", 3, pbi_package_unmark_proc,
	    "_pbi_package_unmark_proc"),
	BLT("$package_toktbl", 1, pbi_package_toktbl, "_pbi_package_toktbl"),
	BLT("$current_package", 1, pbi_current_package,
	    "_pbi_current_package"),
	BLT("$get_default_proc", 3, pbi_get_default_proc,
	    "_pbi_get_default_proc"),
	BLT("$get_default_use", 2, pbi_get_default_use,
	    "_pbi_get_default_use"),
	BLT("$coff_operation", 5, pbi_coff_operation, "_pbi_coff_operation"),
#endif /* PACKAGE */

#ifdef DOS
	BLT("$int86", 9, pbi_int86_9arg, "_pbi_int86_9arg"),
	BLT("$int86", 10, pbi_int86_10arg, "_pbi_int86_10arg"),
	BLT("$int86", 16, pbi_int86_16arg, "_pbi_int86_16arg"),

	BLT("$dos_first_match", 7, pbi_dos_first_match,
	    "_pbi_dos_first_match"),
	BLT("$dos_next_match", 5, pbi_dos_next_match, "_pbi_dos_next_match"),
#endif /* DOS */

	BLT("name", 2, pbi_name, "_pbi_name"),
	BLT("atom_chars", 2, pbi_atom_chars, "_pbi_atom_chars"),
	BLT("atom_codes", 2, pbi_atom_codes, "_pbi_atom_codes"),
	BLT("atom_length", 2, pbi_atom_length, "_pbi_atom_length"),
	BLT("$sub_atom", 4, pbi_sub_atom, "_pbi_sub_atom"),
	BLT("char_code", 2, pbi_char_code, "_pbi_char_code"),
	BLT("pbi_op", 3, pbi_op, "_pbi_op"),
	BLT("tokid", 2, pbi_tokid, "_pbi_tokid"),

	BLT("$uia_alloc", 2, pbi_uia_alloc, "_pbi_uia_alloc"),
	BLT("$uia_clip", 2, pbi_uia_clip, "_pbi_uia_clip"),
	BLT("$uia_size", 2, pbi_uia_size, "_pbi_uia_size"),
	BLT("$uia_peek", 4, pbi_uia_peek, "_pbi_uia_peek"),
	BLT("$uia_peekb", 3, pbi_uia_peekb, "_pbi_uia_peekb"),
	BLT("$uia_peekw", 3, pbi_uia_peekw, "_pbi_uia_peekw"),
	BLT("$uia_peekl", 3, pbi_uia_peekl, "_pbi_uia_peekl"),
	BLT("$uia_peekd", 3, pbi_uia_peekd, "_pbi_uia_peekd"),
	BLT("$uia_peeks", 3, pbi_uia_peeks, "_pbi_uia_peeks"),
	BLT("$uia_peeks", 4, pbi_uia_peeks4, "_pbi_uia_peeks4"),
	BLT("$uia_poke", 5, pbi_uia_poke, "_pbi_uia_poke"),
	BLT("$uia_pokeb", 3, pbi_uia_pokeb, "_pbi_uia_pokeb"),
	BLT("$uia_pokew", 3, pbi_uia_pokew, "_pbi_uia_pokew"),
	BLT("$uia_pokel", 3, pbi_uia_pokel, "_pbi_uia_pokel"),
	BLT("$uia_poked", 3, pbi_uia_poked, "_pbi_uia_poked"),
	BLT("$uia_pokes", 3, pbi_uia_pokes, "_pbi_uia_pokes"),
	BLT("$atom_concat", 3, pbi_atom_concat, "_pbi_atom_concat"),

#ifndef PURE_ANSI
	BLT("$access", 2, pbi_access, "_pbi_access"),
#ifdef OSACCESS
	BLT("getenv", 2, pbi_getenv, "_pbi_getenv"),
	BLT("get_user_home", 2, pbi_get_user_home, "_pbi_get_user_home"),
#endif /* OSACCESS */
#endif /* KERNAL */

	BLT("system", 1, pbi_system, "_pbi_system"),
	BLT("$protect_bottom_stack_page", 0, pbi_protect_bottom_stack_page,
	    "_pbi_protect_bottom_stack_page"),
	BLT("pbi_get_command_line", 1, pbi_command_line, "_pbi_command_line"),
#if defined(UNIX) /* _XOPEN_CRYPT */
	BLT("crypt", 3, pbi_crypt, "_pbi_crypt"),
#endif
#ifndef PURE_ANSI
	BLT("pbi_copy_file", 2, pbi_copy_file, "_pbi_copy_file"),
#endif /* PURE_ANSI */

	BLT("gv_alloc", 1, pbi_gv_alloc, "_pbi_gv_alloc"),
	BLT("gv_free", 1, pbi_gv_free, "_pbi_gv_free"),
	BLT("gv_get", 2, pbi_gv_get, "_pbi_gv_get"),
	BLT("gv_set", 2, pbi_gv_set, "_pbi_gv_set"),
	BLT("gv_alloc_init", 2, pbi_gv_alloc_init, "_pbi_gv_alloc_init"),
	BLT("gv_isfree", 1, pbi_gv_isfree, "_pbi_gv_isfree"),
	BLT("gv_maxpossible", 1, pbi_gv_maxpossible, "_pbi_gv_maxpossible"),

	BLT("pbi_debug", 1, pbi_debug, "_pbi_debug"),

#ifndef KERNAL
	BLT("$load", 2, pbi_load, "_pbi_load"),
#ifdef DynamicForeign
	BLT("$loadforeign", 3, pbi_load_foreign, "_pbi_load_foreign"),
#endif /* DynamicForeign */
	BLT("pbi_ttyflush", 0, pbi_ttyflush, "_pbi_ttyflush"),
	BLT("pbi_nl", 0, pbi_nl, "_pbi_nl"),
	BLT("pbi_write", 1, pbi_write, "_pbi_write"),
#endif /* KERNAL */
#ifdef OLDCIO
	BLT("pbi_display", 1, pbi_display, "_pbi_display"),
	BLT("pbi_get", 1, pbi_get, "_pbi_get"),
	BLT("pbi_get0", 1, pbi_get0, "_pbi_get0"),
	BLT("pbi_put", 1, pbi_put, "_pbi_put"),
	BLT("pbi_read", 1, pbi_read, "_pbi_read"),
	BLT("pbi_see", 1, pbi_see, "_pbi_see"),
	BLT("pbi_seeing", 1, pbi_seeing, "_pbi_seeing"),
	BLT("pbi_seen", 0, pbi_seen, "_pbi_seen"),
	BLT("pbi_tell", 1, pbi_tell, "_pbi_tell"),
	BLT("pbi_telling", 1, pbi_telling, "_pbi_telling"),
	BLT("pbi_told", 0, pbi_told, "_pbi_told"),
	BLT("pbi_writeq", 1, pbi_writeq, "_pbi_writeq"),
#endif /* OLDCIO */
#ifdef SYS_OBP
	BLT("obp_open", 1, pbi_obp_open, "_pbi_obp_open"),
	BLT("obp_close", 0, pbi_obp_close, "_pbi_obp_close"),
	BLT("obp_load", 2, pbi_obp_load, "_pbi_obp_load"),
	BLT("obp_push_stop", 0, pbi_obp_push_stop, "_pbi_obp_push_stop"),
	BLT("obp_pop", 0, pbi_obp_pop, "_pbi_obp_pop"),
#endif /* SYS_OBP */
#ifdef OLDCONSULT
	BLT("old_consult", 2, pbi_old_consult, "_pbi_old_consult"),
#endif /* OLDCONSULT */
	BLT("save_image_with_state_to_file", 1,
	    pbi_save_image_with_state_to_file,
	    "_pbi_save_image_with_state_to_file"),
	BLT("attach_state_to_file", 1, pbi_attach_state_to_file, ""),
	BLT("get_current_image", 1, pbi_get_current_image, ""),
#ifndef PURE_ANSI
	BLT("save_state_to_file", 1, pbi_save_state_to_file,
	    "_pbi_save_state_to_file"),
#endif /* PURE_ANSI */
#if 0
	BLT("save_app_with_obp", 5, pbi_save_app_with_obp,
	    "_pbi_save_app_with_obp"),
#endif /* MacOS */
#ifndef KERNAL
#ifndef PURE_ANSI
	BLT("load_plugin", 4, prolog_load_plugin, "_prolog_load_plugin"),
#endif
	BLT("sio_mkstream", 2, sio_mkstream, "_sio_mkstream"),
	BLT("sio_errcode", 2, sio_errcode, "_sio_errcode"),
	BLT("sio_set_errcode", 2, sio_set_errcode, "_sio_set_errcode"),
	BLT("sio_errno", 2, sio_errno, "_sio_errno"),
	BLT("sio_aux", 2, sio_aux, "_sio_aux"),
	BLT("sio_fd", 2, sio_fd, "_sio_fd"),
	BLT("sio_cpos", 2, sio_cpos, "_sio_cpos"),
	BLT("sio_lpos", 2, sio_lpos, "_sio_lpos"),
	BLT("sio_buf_params", 3, sio_buf_params, "_sio_buf_params"),
	BLT("sio_increment_bufpos", 1, sio_increment_bufpos,
	    "_sio_increment_bufpos"),
	BLT("sio_set_position", 3, sio_set_position, "_sio_set_position"),
	BLT("sio_file_open", 5, sio_file_open, "_sio_file_open"),
	BLT("sio_console_open", 6, sio_console_open, "_sio_console_open"),
	BLT("sio_set_eof", 1, sio_set_eof, "_sio_set_eof"),
	BLT("sio_reset_eof", 1, sio_reset_eof, "_sio_reset_eof"),
#ifdef SysVIPC
	BLT("sio_sysVq_open", 7, sio_sysVq_open, "_sio_sysVq_open"),
	BLT("ftok", 3, pbi_ftok, "_pbi_ftok"),
	BLT("msgctl", 4, pbi_msgctl, "_pbi_msgctl"),
#endif /* SysVIPC */
#ifdef SSBQ
	BLT("sio_ssbq_open", 6, sio_ssbq_open, "_sio_ssbq_open"),
#endif /* SSBQ */
#ifdef HAVE_SOCKET
	BLT("sio_nsocket", 5, sio_nsocket, "_sio_nsocket"),
	BLT("sio_nsocket_connect", 5, sio_nsocket_connect, "_sio_nsocket_connect"),
	BLT("sio_nsocket_bind", 4, sio_nsocket_bind, "_sio_nsocket_bind"),
	BLT("sio_nsocket_listen", 4, sio_nsocket_listen, "_sio_nsocket_listen"),
	BLT("sio_nsocket_accept", 5, sio_nsocket_accept, "_sio_nsocket_accept"),
	BLT("sio_nsocket_close", 2, sio_nsocket_close, "_sio_nsocket_close"),
	BLT("sio_nsocket_select", 9, sio_nsocket_select, "_sio_nsocket_select"),
	BLT("sio_nsocketpair", 3, sio_nsocketpair, "_sio_nsocketpair"),

	BLT("sio_nsocket_open", 6, sio_nsocket_open, "_sio_nsocket_open"),

	BLT("gethostbyname", 4, pbi_gethostbyname, "_pbi_gethostbyname"),
	BLT("gethostbyaddr", 4, pbi_gethostbyaddr, "_pbi_gethostbyaddr"),

	BLT("sio_gethostname",1,sio_gethostname,"_sio_gethostname"),
	BLT("sio_socket_open", 10, sio_socket_open, "_sio_socket_open"),
	BLT("sio_is_server_socket", 1, sio_is_server_socket,
	    "_sio_is_server_socket"),
	BLT("sio_accept_socket_connection", 2, sio_accept_socket_connection,
	    "_sio_accept_socket_connection"),
	BLT("sio_poll",2,sio_poll,"_sio_poll"),
#ifdef HAVE_SELECT
	BLT("sio_simple_select", 3, sio_simple_select, "_sio_simple_select"),
#endif /* HAVE_SELECT */
#endif /* HAVE_SOCKET */

#ifdef REXEC
	BLT("sio_rexec", 7, sio_rexec, "_sio_rexec"),
	BLT("sio_fork", 1, sio_fork, "_sio_fork"),
#endif /* REXEC */

	BLT("sio_generic_open", 5, sio_generic_open, "_sio_generic_open"),
	BLT("sio_close", 1, sio_close, "_sio_close"),
#ifndef SIO_ASM
	BLT("sio_get_byte", 2, sio_get_byte, "_sio_get_byte"),
	BLT("sio_put_byte", 2, sio_put_byte, "_sio_put_byte"),
#endif /* SIO_ASM */
	BLT("sio_unget_byte", 1, sio_unget_byte, "_sio_unget_byte"),
	BLT("sio_getpos", 2, sio_getpos, "_sio_getpos"),
	BLT("sio_seek", 4, sio_seek, "_sio_seek"),
	BLT("sio_readbuffer", 1, sio_readbuffer, "_sio_readbuffer"),
	BLT("sio_set_do_lineedit", 1, sio_set_do_lineedit, "_sio_set_do_lineedit"),
	BLT("sio_set_lineedit_prompt", 1, sio_set_lineedit_prompt, "_sio_set_lineedit_prompt"),
        BLT("sio_set_history_file", 1, sio_set_history_file, "_sio_set_history_file"),
        BLT("sio_set_no_load_prev_history", 0, sio_set_no_load_prev_history, "_sio_set_no_load_prev_history"),
	BLT("sio_writebuffer", 1, sio_writebuffer, "_sio_writebuffer"),
	BLT("sio_bufshift", 1, sio_bufshift, "_sio_bufshift"),
	BLT("sio_next_token", 3, sio_next_token, "_sio_next_token"),
	BLT("sio_next_tokens", 3, sio_next_tokens, "_sio_next_tokens"),
	BLT("sio_skip_layout", 1, sio_skip_layout, "_sio_skip_layout"),
	BLT("sio_linenumber", 2, sio_linenumber, "_sio_linenumber"),
	BLT("sio_put_atom", 2, sio_put_atom, "_sio_put_atom"),
	BLT("sio_put_number", 3, sio_put_number, "_sio_put_number"),
	BLT("sio_get_number", 3, sio_get_number, "_sio_get_number"),
	BLT("sio_qatom", 3, sio_qatom, "_sio_qatom"),
	BLT("sio_var_to_atom", 2, sio_var_to_atom, "_sio_var_to_atom"),
	BLT("sio_lettervar", 2, sio_lettervar, "_sio_lettervar"),
	BLT("sio_sprintf", 4, sio_sprintf, "_sio_sprintf"),
	BLT("sio_sprintf_number", 3, sio_sprintf_number, "_sio_sprintf_number"),
	BLT("sio_isgraphicatom", 1, sio_isgraphicatom, "_sio_isgraphicatom"),
	BLT("sio_nl", 1, sio_nl, "_sio_nl"),
	BLT("sio_readln", 3, sio_readln, "_sio_readln"),
	BLT("sio_position_in_line", 3, sio_position_in_line,
	    "_sio_position_in_line"),
#endif /* KERNAL */
	BLT("gc", 0, gc, "_gc"),
	BLT("pbi_halt", 0, pbi_halt, "_pbi_halt"),
	BLT("setInterruptVector", 1, pbi_set_interrupt_vector,
	    "_pbi_set_interrupt_vector"),
	BLT("uncaught_interrupt", 3, pbi_uncaught_interrupt,
	    "_pbi_uncaught_interrupt"),
	BLT("forcePrologInterrupt", 0, pbi_ouch, "_pbi_ouch"),
	BLT("forceCtlC", 0, pbi_forceCtlC, "_pbi_forceCtlC"),
	BLT("forcePrologError", 0, pbi_forcePrologError,
	    "_pbi_forcePrologError"),
	BLT("reset_wm_normal", 0, pbi_reset_wm_normal, "_pbi_reset_wm_normal"),
#ifndef KERNAL
	BLT("print_warning", 0, pbi_printwarning, "_pbi_printwarning"),
#endif /* KERNAL */
/*	BLT("limits_info", 3, pbi_limits_info, "_pbi_limits_info"),  */

#ifdef OLDSHELL
	BLT("print_no", 0, pbi_printno, "_pbi_printno"),
	BLT("showanswers", 2, pbi_showanswers, "_pbi_showanswers"),
#endif /* OLDSHELL */
	BLT("statistics", 1, pbi_statistics, "_pbi_statistics"),

	BLT("$stack_overflow", 1, pbi_stack_overflow, "_pbi_stack_overflow"),
	BLT("$stack_info", 1, pbi_stack_info, "_pbi_stack_info"),
#ifdef MacOS
	BLT("pbi_debugger", 0, pbi_debugger, "_pbi_debugger"),
#endif /* MacOS */
	BLT("$findterm", 5, pbi_findterm, "_pbi_findterm"),
#ifdef CMeta
	BLT("true", 0, pbi_true, "_pbi_true"),
	BLT("=", 2, pbi_equal, "_pbi_equal"),
	BLT("==", 2, pbi_identical, "_pbi_identical"),
	BLT("\\==", 2, pbi_unidentical, "_pbi_unidentical"),
	BLT("eq", 2, pbi_eq, "_pbi_eq"),
	BLT("noneq", 2, pbi_noneq, "_pbi_noneq"),
	BLT("arg", 3, pbi_arg, "_pbi_arg"),
	BLT("atom", 1, pbi_atom, "_pbi_atom"),
	BLT("atomic", 1, pbi_atomic, "_pbi_atomic"),
	BLT("compound", 1, pbi_compound, "_pbi_compound"),
	BLT("float", 1, pbi_float, "_pbi_float"),
	BLT("functor", 3, pbi_functor, "_pbi_functor"),
	BLT("integer", 1, pbi_integer, "_pbi_integer"),
	BLT("mangle", 3, pbi_mangle, "_pbi_mangle"),
#ifdef TRAILVALS
	BLT("trailed_mangle", 3, pbi_trailed_mangle, "_pbi_trailed_mangle"),
#endif /* TRAILVALS */
	BLT("nonvar", 1, pbi_nonvar, "_pbi_nonvar"),
	BLT("number", 1, pbi_number, "_pbi_number"),
	BLT("var", 1, pbi_var, "_pbi_var"),
	BLT("compare", 3, pbi_compare, "_pbi_compare"),
#endif /* CMeta */

#ifdef HASH
	BLT("hashN", 4, pbi_hashN, "_pbi_hashN"),
#endif /* HASH */

#ifdef GENSYM
	BLT("gensym", 2, pbi_gensym, "_pbi_gensym"),
	BLT("isgensym", 2, pbi_isgensym, "_pbi_isgensym"),
#endif /* GENSYM */

#ifdef DEBUGSYS
	BLT("toggle_sys_debug", 1, pbi_toggle_debug_system,
	    "_pbi_toggle_debug_system"),
#endif /* DEBUGSYS */

#ifdef PRIM_DBG
	BLT("ptermaddr", 1, pbi_ptermaddr, "_pbi_ptermaddr"),
	BLT("traildump", 0, pbi_traildump, "_pbi_traildump"),
	BLT("frame_info", 2, pbi_frame_info, "_pbi_frame_info"),
#endif /* PRIM_DBG */

#ifdef TRACEBWAM
	BLT("trace_bwam",2,toggle_bwam,"_toggle_bwam"),
#endif /* TRACEBWAM */

#if	defined(Portable) && defined(IProfile)
	BLT("init_iprofile", 0, pbi_init_iprofile, "_pbi_init_iprofile"),
	BLT("dump_iprofile", 0, pbi_dump_iprofile, "_pbi_dump_iprofile"),
#endif /* defined(Portable) && defined(IProfile) */

#ifdef BCINTER
	BLT("$c_malloc", 2, pbi_c_malloc, "_pbi_c_malloc"),
	BLT("$c_free", 1, pbi_c_free, "_pbi_c_free"),
	BLT("$c_set", 2, pbi_c_set, "_pbi_c_set"),
	BLT("$c_examine", 2, pbi_c_examine, "_pbi_c_examine"),
#endif /* BCINTER */

#ifndef KERNAL
	BLT("dbg_nospy", 3, pbi_dbg_nospy, "_pbi_dbg_nospy"),
	BLT("dbg_spy", 3, pbi_dbg_spy, "_pbi_dbg_spy"),
	BLT("dbg_spyoff", 0, pbi_dbg_spyoff, "_pbi_dbg_spyoff"),
	BLT("dbg_spyon", 0, pbi_dbg_spyon, "_pbi_dbg_spyon"),
	BLT("dbg_spying", 0, pbi_dbg_spying, "_pbi_dbg_spying"),
	BLT("alarm", 2, pbi_alarm, "_pbi_alarm"),
#endif /* KERNAL */

#ifdef SUBTYPES
	BLT("less_sut_int", 2, pbi_less_sut_int, "_less_sut_int"),
	BLT("eq_sut_int", 2, pbi_eq_sut_int, "_eq_sut_int"),
	BLT("mk_sut_int", 2, pbi_mk_sut_int, "_mk_sut_int"),
	BLT("pos_atom", 2, pbi_pos_atom, "_mk_pos_atom"),
	BLT("t_sut_int", 0, pbi_t_sut_int, "_t_sut_int"),
#endif /* SUBTYPES */

	BLT("signal_name", 2, pbi_signal_name, "_pbi_signal_name"),
	BLT("resize_memory", 2, pbi_resize_memory, "_pbi_resize_memory"),

	BLT("curl_c_builtin", 2, curl_c_builtin, "_curl_c_builtin"),
	BLT("lookup_opt_info", 1, lookup_opt_info, "_lookup_opt_info")

};
	/* blt_tab[] */

#define NULLF ((int (*) ( void )) 0)
#define BLT2(nam,arity,installer,p1,p2,fname) 				\
		{nam,arity, 						\
		(void (*) (ntbl_entry *, PWord, PWord))installer, \
		p1,p2,fname}

/*
 * The following wm_ declarations should not probably not be called as
 *      functions.
 */

extern int wm_catch22 	( void );
extern int wm_colon 	( void );
extern int wm_cut 	( void );
extern int wm_jump 	( void );
extern int wm_ocall 	( void );
extern int wm_dbg_call 	( void );
extern int wm_throw 	( void );

#ifndef CMeta
extern int wm_arg 	( void );
extern int wm_atom 	( void );
extern int wm_atomic 	( void );
extern int wm_compare 	( void );
extern int wm_float 	( void );
extern int wm_functor 	( void );
extern int wm_identical ( void );
extern int wm_integer 	( void );
extern int wm_mangle 	( void );
extern int wm_nonidentical ( void );
extern int wm_nonvar 	( void );
extern int wm_number 	( void );
extern int wm_var 	( void );

extern int wm_call 	( void );
extern int wm_eq 	( void );
extern int wm_noneq 	( void );

#endif /* CMeta */

#ifdef Portable
#define INTF(v) ((int (*) (void)) (v))
#endif

#ifdef SIO_ASM
extern	int	wm_sio_pbyte	( void );
extern	int	wm_sio_gbyte	( void );
#endif /* SIO_ASM */


static struct blt2_struct {
    const char *name;
    int   arity;
    void  (*installer) ( ntbl_entry *, PWord, PWord );
    int   (*p1) ( void ), (*p2) ( void );
    const char *bltname;
} blt2_tab[] = {

#ifdef Portable
BLT2("$comma", 3, ic_install_bref, INTF("$comma"), INTF(4), (char *) -1),
    BLT2("$arrow", 3, ic_install_bref, INTF("$arrow"), INTF(4), (char *) -1),
    BLT2("$semicolon", 3, ic_install_bref, INTF("$semicolon"), INTF(4), (char *) -1),
	BLT2("!", 0, ic_install_instr, INTF(W_MACRO_CUTPROCEED), NULLF, (char *) -1),
	BLT2("$cut", 1, ic_install_instr, INTF(W_MACRO_CUTPROCEED), NULLF, (char *) -1),
	BLT2("$colon", 3, ic_install_instr, INTF(W_COLON), NULLF, (char *) -1),
	BLT2(":", 2, ic_install_instr, INTF(W_COLON), NULLF, (char *) -1),
	BLT2("callWithDelayedInterrupt", 2, ic_install_instr, INTF(W_OCALL), NULLF, (char *) -1),
	BLT2("dbg_call", 2, ic_install_instr, INTF(W_DBG_CALL), NULLF, (char *) -1),
	BLT2("catch22", 0, ic_install_instr, INTF(W_CATCH22), NULLF, (char *) -1),
	BLT2("throw", 0, ic_install_instr, INTF(W_THROW), NULLF, (char *) -1),
	BLT2("jump", 2, ic_install_instr, INTF(W_WEIRD_JUMP), NULLF, (char *) -1),
	BLT2("fail", 0, ic_install_instr, INTF(W_FAIL), NULLF, (char *) -1),
	BLT2("true", 0, ic_install_instr, INTF(W_PROCEED), NULLF, (char *) -1),

#else  /* not-Portable */
	BLT2("!", 0, ic_install_jmp, wm_cut, NULLF, "_wm_cut"),
	BLT2("$colon", 3, ic_install_jmp, wm_colon, NULLF, "_wm_colon"),
	BLT2(":", 2, ic_install_jmp, wm_colon, NULLF, "_wm_colon"),
	BLT2("fail", 0, ic_install_jmp, wm_fail, NULLF, "_wm_fail"),
	BLT2("jump", 2, ic_install_jmp, wm_jump, NULLF, "_wm_jump"),
	BLT2("callWithDelayedInterrupt", 2, ic_install_jmp, wm_ocall, NULLF, "_wm_ocall"),
	BLT2("dbg_call", 2, ic_install_jmp, wm_dbg_call, NULLF, "_wm_dbg_call"),
	BLT2("catch22", 0, ic_install_jmp, wm_catch22, NULLF, "_wm_catch22"),
	BLT2("throw", 0, ic_install_jmp, wm_throw, NULLF, "_wm_throw"),
	BLT2("$cut", 1, ic_install_jmp, wm_cut, NULLF, "_wm_cut"),

#ifndef CMeta
	BLT2("=", 2, ic_install_equal, NULLF, NULLF, (char *) -1),
	BLT2("==", 2, ic_install_jmp, wm_identical, NULLF, "_wm_identical"),
	BLT2("\\==", 2, ic_install_jmp, wm_nonidentical, NULLF, "_wm_nonidentical"),
	BLT2("arg", 3, ic_install_jmp, wm_arg, NULLF, "_wm_arg"),
	BLT2("atom", 1, ic_install_jmp, wm_atom, NULLF, "_wm_atom"),
	BLT2("atomic", 1, ic_install_jmp, wm_atomic, NULLF, "_wm_atomic"),
	BLT2("call", 1, ic_install_call, wm_call, NULLF, "_wm_call"),
	BLT2("compare", 3, ic_install_jmp, wm_compare, NULLF, "_wm_compare"),
	BLT2("eq", 2, ic_install_jmp, wm_eq, NULLF, "_wm_eq"),
	BLT2("float", 1, ic_install_jmp, wm_float, NULLF, "_wm_float"),
	BLT2("functor", 3, ic_install_jmp, wm_functor, NULLF, "_wm_functor"),
	BLT2("integer", 1, ic_install_jmp, wm_integer, NULLF, "_wm_integer"),
	BLT2("mangle", 3, ic_install_jmp, wm_mangle, NULLF, "_wm_mangle"),
	BLT2("noneq", 2, ic_install_jmp, wm_noneq, NULLF, "_wm_noneq"),
	BLT2("nonvar", 1, ic_install_jmp, wm_nonvar, NULLF, "_wm_nonvar"),
	BLT2("number", 1, ic_install_jmp, wm_number, NULLF, "_wm_number"),
	BLT2("true", 0, ic_install_true, NULLF, NULLF, (char *) -1),
	BLT2("var", 1, ic_install_jmp, wm_var, NULLF, "_wm_var"),
#endif /* CMeta */

#ifdef SIO_ASM
	BLT2("sio_get_byte", 2, ic_install_jmp, wm_sio_gbyte, NULLF, "_wm_sio_gbyte"),
	BLT2("sio_put_byte", 2, ic_install_jmp, wm_sio_pbyte, NULLF, "_wm_sio_pbyte"),
#endif /* SIO_ASM */




#ifdef MotorolaMath
	BLT2("padd", 3, ic_install_jmp, padd, NULLF, "_padd"),
	BLT2("psub", 3, ic_install_jmp, psub, NULLF, "_psub"),
	BLT2("pmul", 3, ic_install_jmp, pmul, NULLF, "_pmul"),
	BLT2("pdiv", 3, ic_install_jmp, pdiv, NULLF, "_pdiv"),
	BLT2("pmac", 4, ic_install_jmp, pmac, NULLF, "_pmac"),
	BLT2("eiexp", 3, ic_install_jmp, eiexp, NULLF, "_eiexp"),
	BLT2("psin", 2, ic_install_jmp, psin, NULLF, "_psin"),
	BLT2("pcos", 2, ic_install_jmp, pcos, NULLF, "_pcos"),
	BLT2("ptan", 2, ic_install_jmp, ptan, NULLF, "_ptan"),
	BLT2("psinh", 2, ic_install_jmp, psinh, NULLF, "_psinh"),
	BLT2("pcosh", 2, ic_install_jmp, pcosh, NULLF, "_pcosh"),
	BLT2("ptanh", 2, ic_install_jmp, ptanh, NULLF, "_ptanh"),
	BLT2("pasin", 2, ic_install_jmp, pasin, NULLF, "_pasin"),
	BLT2("pacos", 2, ic_install_jmp, pacos, NULLF, "_pacos"),
	BLT2("patan", 2, ic_install_jmp, patan, NULLF, "_patan"),
	BLT2("plog10", 2, ic_install_jmp, plog10, NULLF, "_plog10"),
	BLT2("plog", 2, ic_install_jmp, plog, NULLF, "_plog"),
	BLT2("pexp", 2, ic_install_jmp, pexp, NULLF, "_pexp"),
	BLT2("psqrt", 2, ic_install_jmp, psqrt, NULLF, "_psqrt"),
	BLT2("pfloor", 2, ic_install_jmp, pfloor, NULLF, "_pfloor"),
	BLT2("pceil", 2, ic_install_jmp, pceil, NULLF, "_pceil"),
	BLT2("pfabs", 2, ic_install_jmp, pfabs, NULLF, "_pfabs"),
	BLT2("pgamma", 2, ic_install_jmp, pgamma, NULLF, "_pgamma"),
	BLT2("ppow", 3, ic_install_jmp, ppow, NULLF, "_ppow"),
	BLT2("pfmod", 3, ic_install_jmp, pfmod, NULLF, "_pfmod"),
	BLT2("patan2", 3, ic_install_jmp, patan2, NULLF, "_patan2"),
	BLT2("phypot", 3, ic_install_jmp, phypot, NULLF, "_phypot"),
	BLT2("perf", 2, ic_install_jmp, perf, NULLF, "_perf"),
	BLT2("perfc", 2, ic_install_jmp, perfc, NULLF, "_perfc"),
	BLT2("pc_add", 3, ic_install_jmp, pc_add, NULLF, "_pc_add"),
	BLT2("pc_sub", 3, ic_install_jmp, pc_sub, NULLF, "_pc_sub"),
	BLT2("pc_mul", 3, ic_install_jmp, pc_mul, NULLF, "_pc_mul"),
	BLT2("pcj_mul", 3, ic_install_jmp, pcj_mul, NULLF, "_pcj_mul"),
	BLT2("pc_mac", 4, ic_install_jmp, pc_mac, NULLF, "_pc_mac"),
	BLT2("pc_mag2", 2, ic_install_jmp, pc_mag2, NULLF, "_pc_mag2"),
	BLT2("pc_mag", 2, ic_install_jmp, pc_mag, NULLF, "_pc_mag"),
	BLT2("pc_conj", 2, ic_install_jmp, pc_conj, NULLF, "_pc_conj"),
	BLT2("pc_rec", 2, ic_install_jmp, pc_rec, NULLF, "_pc_rec"),
	BLT2("pc_div", 3, ic_install_jmp, pc_div, NULLF, "_pc_div"),
	BLT2("prc_mul", 3, ic_install_jmp, prc_mul, NULLF, "_prc_mul"),
	BLT2("prc_mac", 4, ic_install_jmp, prc_mac, NULLF, "_prc_mac"),
	BLT2("make_vector", 2, ic_install_jmp, make_vector, NULLF, "_make_vector"),
	BLT2("vrr_dot", 3, ic_install_jmp, vrr_dot, NULLF, "_vrr_dot"),
	BLT2("vrr_add", 3, ic_install_jmp, vrr_add, NULLF, "_vrr_add"),
	BLT2("vrr_div", 3, ic_install_jmp, vrr_div, NULLF, "_vrr_div"),
	BLT2("vrr_mul", 3, ic_install_jmp, vrr_mul, NULLF, "_vrr_mul"),
	BLT2("vrr_sub", 3, ic_install_jmp, vrr_sub, NULLF, "_vrr_sub"),
	BLT2("varg", 3, ic_install_jmp, varg, NULLF, "_varg"),
	BLT2("varg_first", 3, ic_install_jmp, varg_first, NULLF, "_varg_first"),
	BLT2("varg_last", 3, ic_install_jmp, varg_last, NULLF, "_varg_last"),
	BLT2("vlength", 2, ic_install_jmp, vlength, NULLF, "_vlength"),
	BLT2("vr_sum", 2, ic_install_jmp, vr_sum, NULLF, "_vr_sum"),
	BLT2("vr_mag2", 2, ic_install_jmp, vr_mag2, NULLF, "_vr_mag2"),
	BLT2("vr_neg", 2, ic_install_jmp, vr_neg, NULLF, "_vr_neg"),
	BLT2("vrsr_mul", 3, ic_install_jmp, vrsr_mul, NULLF, "_vrsr_mul"),
	BLT2("vr_select", 4, ic_install_jmp, vr_select, NULLF, "_vr_select"),
	BLT2("vcc_add", 3, ic_install_jmp, vcc_add, NULLF, "_vcc_add"),
	BLT2("vcc_sub", 3, ic_install_jmp, vcc_sub, NULLF, "_vcc_sub"),
	BLT2("vcc_mul", 3, ic_install_jmp, vcc_mul, NULLF, "_vcc_mul"),
	BLT2("vcc_div", 3, ic_install_jmp, vcc_div, NULLF, "_vcc_div"),
	BLT2("vcc_dot", 3, ic_install_jmp, vcc_dot, NULLF, "_vcc_dot"),
	BLT2("vrc_mul", 3, ic_install_jmp, vrc_mul, NULLF, "_vrc_mul"),
	BLT2("vcjc_dot", 3, ic_install_jmp, vcjc_dot, NULLF, "_vcjc_dot"),
	BLT2("vc_conj", 2, ic_install_jmp, vc_conj, NULLF, "_vc_conj"),
	BLT2("vc_neg", 2, ic_install_jmp, vc_neg, NULLF, "_vc_neg"),
	BLT2("vc_sum", 2, ic_install_jmp, vc_sum, NULLF, "_vc_sum"),
	BLT2("vcsc_mul", 3, ic_install_jmp, vcsc_mul, NULLF, "_vcsc_mul"),
	BLT2("vcsr_mul", 3, ic_install_jmp, vcsr_mul, NULLF, "_vcsr_mul"),
	BLT2("vc_cmag2", 2, ic_install_jmp, vc_cmag2, NULLF, "_vc_cmag2"),
	BLT2("vrc_mul", 3, ic_install_jmp, vrc_mul, NULLF, "_vrc_mul"),
	BLT2("vc_select", 4, ic_install_jmp, vc_select, NULLF, "_vc_select"),
	BLT2("lrr_dot", 3, ic_install_jmp, lrr_dot, NULLF, "_lrr_dot"),
	BLT2("lrr_mul", 3, ic_install_jmp, lrr_mul, NULLF, "_lrr_mul"),
	BLT2("lcc_dot", 3, ic_install_jmp, lcc_dot, NULLF, "_lcc_dot"),
	BLT2("lcc_add", 3, ic_install_jmp, lcc_add, NULLF, "_lcc_add"),
	BLT2("lcc_sub", 3, ic_install_jmp, lcc_sub, NULLF, "_lcc_sub"),
	BLT2("lcc_mul", 3, ic_install_jmp, lcc_mul, NULLF, "_lcc_mul"),
	BLT2("lsplit", 4, ic_install_jmp, lsplit2, NULLF, "_lsplit2"),
#endif /* MotorolaMath */
#endif /* Portable */
};



void
builtin_init(void)
{
    /*//static long builtins_initialized=0;*/

    /*//if (!builtins_initialized) {*/
	register int i;
	register struct blt_struct *p;
	struct blt2_struct *p2;
	i = sizeof blt_tab / sizeof (struct blt_struct);
	p = blt_tab + i - 1;
	for (; i > 0; i--, p--) {
	    w_assert_builtin(p->name, p->arity, p->blt);
	}

	i = sizeof blt2_tab / sizeof (struct blt2_struct);
	p2 = blt2_tab + i - 1;
	for (; i > 0; i--, p2--) {
	    w_assert_built2(p2->name, p2->arity, p2->installer,
			    (long) p2->p1, (long) p2->p2);
	}

	builtins_initialized = 1;
	ss_register_global(&builtins_initialized);
    /*//}*/
}

/* #ifdef NewMath  */
#if defined(NewMath) && !defined(Portable)
Code *mth_is_addr;
Code *mth_lt_addr;
Code *mth_gt_addr;
Code *mth_le_addr;
Code *mth_ge_addr;
Code *mth_ne_addr;
Code *mth_eq_addr;
#endif /* NewMath */

int pbi_set_interrupt_vector(void)
{
    PWord v;
    int vt;
    char *s;
    PI_getan(&v, &vt, 1);
    if (vt == PI_SYM) s = PI_getsymname(NULL, v, 0);
    else if (vt == PI_UIA) s = PI_getuianame(NULL, v, 0);
    else PI_FAIL;
    
    wm_overcode = w_nameentry((PWord) MODULE_BUILTINS,
    			(PWord) find_token((UCHAR *)s), 3)->exec_entry;
    PI_SUCCEED;
}

int pbi_uncaught_interrupt(void)
{
  fprintf(stderr, "Uncaught Interrupt!\n");
  exit(1);
  PI_SUCCEED;
}

void
time_cut_interrupt_init(void)
{
    os_init_time();

    wm_cutaddr = w_nameprobe((PWord) MODULE_BUILTINS, (PWord) TK_CUT, 0)->call_entry;
    /* Set the default interrupt handler to be uncaught_interrupt */
    wm_overcode = w_nameentry((PWord) MODULE_BUILTINS,
			   (PWord) find_token((UCHAR *)"uncaught_interrupt"), 3)->exec_entry;

/* #ifdef NewMath  */
#if defined(NewMath) && !defined(Portable)
    mth_is_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_IS, 2)
	->call_entry;
    mth_lt_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_LESS, 2)
	->call_entry;
    mth_gt_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_GRT, 2)
	->call_entry;
    mth_le_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_LEQ, 2)
	->call_entry;
    mth_ge_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_GEQ, 2)
	->call_entry;
    mth_ne_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_ZEBRA2, 2)
	->call_entry;
    mth_eq_addr = w_nameentry((PWord) MODULE_BUILTINS, (PWord) TK_ZEBRA, 2)
	->call_entry;
#endif /* defined(NewMath) && !defined(Portable) */

}



#ifdef PACKAGE

/*
 * Size of the builtin address table must be greater than
 * total number of builtins in the system
 */
#define BLT_ADDR_TBL_SIZE 	4093


static struct blt_addr_tbl_entry {
    long  bltaddr;
    char *bltname;
} blt_addr_tbl[BLT_ADDR_TBL_SIZE];


builtin_addr_table_init(void)
{
    register int i;
    register struct blt_struct *p;
    register struct blt_addr_tbl_entry *q;
    struct blt2_struct *p2;

    /* Clear builtin address table */
    q = blt_addr_tbl;
    i = BLT_ADDR_TBL_SIZE;
    while (i--) {
	q->bltaddr = 0;
	q++;
    }

    /* Insert first set of builtins */
    i = sizeof blt_tab / sizeof (struct blt_struct);
    p = blt_tab + i - 1;
    for (; i > 0; i--, p--)
	if (p->bltname != (char *) -1)
#ifdef PCKG_NO_UNDERBAR
	    insert_builtin_addr((long) p->blt, ((char *) (p->bltname)) + 1);
#else  /* PCKG_NO_UNDERBAR */
	    insert_builtin_addr((long) p->blt, p->bltname);
#endif /* PCKG_NO_UNDERBAR */

    /* Insert second set of builtins */
    i = sizeof blt2_tab / sizeof (struct blt2_struct);
    p2 = blt2_tab + i - 1;
    for (; i > 0; i--, p2--)
	if (p2->bltname != (char *) -1)
#ifdef PCKG_NO_UNDERBAR
	    insert_builtin_addr((long) p2->p1, ((char *) (p2->bltname)) + 1);
#else  /* PCKG_NO_UNDERBAR */
	    insert_builtin_addr((long) p2->p1, p2->bltname);
#endif /* PCKG_NO_UNDERBAR */
}


insert_builtin_addr(blt, bname)
    long  blt;
    char *bname;
{
    register unsigned int i;
    register unsigned int start;

    i = blt % BLT_ADDR_TBL_SIZE;
    start = i;
    while (blt_addr_tbl[i].bltaddr) {
	i = (i + 1) % BLT_ADDR_TBL_SIZE;
	if (i == start)
	    fatal_error(FE_IN_BLTINIT, 0);
    }

    blt_addr_tbl[i].bltaddr = blt;
    blt_addr_tbl[i].bltname = bname;
}

char *
builtin_name(blt)
    long  blt;
{
    register unsigned int i;

    i = blt % BLT_ADDR_TBL_SIZE;
    while ((blt_addr_tbl[i].bltaddr != 0) && (blt_addr_tbl[i].bltaddr != blt))
	i = (i + 1) % BLT_ADDR_TBL_SIZE;

    if (blt_addr_tbl[i].bltaddr == blt)
	return (blt_addr_tbl[i].bltname);
    else {
	fprintf(stderr,
		"\nWarning: Builtin %x not in builtin address table.", blt);
	fflush(stderr);
	return ("UNKNOWN_BUILTIN_NAME");
    }
}


char *
is_builtin_name(blt)
    long  blt;
{
    register unsigned int i;

    i = blt % BLT_ADDR_TBL_SIZE;
    while ((blt_addr_tbl[i].bltaddr != 0) && (blt_addr_tbl[i].bltaddr != blt))
	i = (i + 1) % BLT_ADDR_TBL_SIZE;

    if (blt_addr_tbl[i].bltaddr == blt)
	return (blt_addr_tbl[i].bltname);

    return ((char *) -1);
}

#endif /* PACKAGE */
