/*=======================================================*
 |			built.h		
 |		Copyright (c) 1993-95 Applied Logic Systems, Inc.
 |			-- Include file for b*.c files
 *=======================================================*/
#define FAIL return(0) 
#define SUCCEED return(1)

/*
 * The following PERR_ macros cause a prolog error to be thrown
 * (eventually...via the interrupt mechanism).
 */

#define PERR_INSTANTIATION(namtok,arity)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_INSTANTIATION_ERROR,0, \
			     0, 0,0, 0,0); 			\
	    FAIL;						\
	} while (0)

#define PERR_TYPE(namtok,arity,typesym,cv,ct)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_TYPE_ERROR,2, 	\
			     typesym, cv,ct, 0,0);		\
	    FAIL;						\
	} while (0)

#define PERR_DOMAIN(namtok,arity,dsym,cv,ct)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_DOMAIN_ERROR,2,	\
			     dsym, cv,ct, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_EXISTENCE(namtok,arity,obsym,cv,ct)		\
	do {							\
	    set_prolog_error(namtok,arity, TK_EXISTENCE_ERROR,2, \
			     obsym, cv,ct, 0,0);		\
	    FAIL;						\
	} while (0)

#define PERR_PERMISSION(namtok,arity,opsym,typesym,cv,ct)	\
	do {							\
	    set_prolog_error(namtok,arity, TK_PERMISSION_ERROR,2,	\
			     opsym, typesym,WTP_SYMBOL, cv,ct); \
	    FAIL;						\
	} while (0)

#define PERR_REPRESENTATION(namtok,arity,flagsym)		\
	do {							\
	    set_prolog_error(namtok,arity, TK_REPRESENTATION_ERROR,1,	\
			     flagsym,0,0,0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_EVALUATION(namtok,arity,errsym)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_EVALUATION_ERROR,1, \
			     errsym, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_RESOURCE(namtok,arity,errsym)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_RESOURCE_ERROR,1, \
			     errsym, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_SYSTEM(namtok,arity)				\
	do {							\
	    set_prolog_error(namtok,arity, TK_SYSTEM_ERROR,0,	\
			     0, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

/* bparser.c */
extern	int	get_number	PARAMS( (PWord, int, double *) );

/* butil.c */
extern	void	heap_copy		PARAMS( (PWord *, int *, pword) );
extern	int	xform_uia			PARAMS( (PWord *, int *) );
extern	int	force_uia			PARAMS( (PWord *, int *) );
extern	void	string_to_list	PARAMS( (PWord *, int *, UCHAR *) );
extern	int	list_to_string		PARAMS( (UCHAR *, PWord, int) );
extern	int	getstring			PARAMS( (UCHAR **, PWord, int) );
extern	int	getlong				PARAMS( (long *, PWord, int) );
extern	int	get_gv_number		PARAMS( (UCHAR *) );
extern	void set_prolog_error	PARAMS( (PWord, int, PWord, int, PWord, PWord, int, PWord, int) );
extern	int	getdouble			PARAMS( (double *, PWord, int) );

/* wdisp.c */
extern	void	prolog_write	PARAMS( (PWord, int) );
extern	void	prolog_writeq	PARAMS( (PWord, int) );
extern	void	prolog_display	PARAMS( (PWord, int) );

/* built.c */
extern	void	builtin_init			PARAMS( (void) );
extern	void	time_cut_interrupt_init	PARAMS( (void) );
extern	int	pbi_set_interrupt_vector	PARAMS((void));
extern  int     pbi_uncaught_interrupt 	PARAMS( (void) );

/* bmisc.c */
#ifdef CMeta
extern	int	wm_identical	PARAMS(( PWord, int, PWord, int ));
#endif

/* arith.c */
extern	int	pbi_time			PARAMS(( void ));
extern	int	pbi_less			PARAMS(( void ));
extern	int	pbi_greater			PARAMS(( void ));
extern	int	pbi_equalorless		PARAMS(( void ));
extern	int	pbi_greaterorequal 	PARAMS(( void ));
extern	int	pbi_arithequal		PARAMS(( void ));
extern	int	pbi_arithnotequal 	PARAMS(( void ));
extern	int	pbi_is				PARAMS(( void ));
extern	int	pbi_srandom			PARAMS(( void ));
/* fpbasis.c */
extern	int pbi_fpconst_val			PARAMS( (void) );
extern	int pbi_uia_poke_fpconst	PARAMS( (void) );

/* from bcinter.c */
#ifdef DOS
extern	int	pbi_c_make_farptr PARAMS(( void ));
#endif
extern	int	pbi_c_malloc		PARAMS(( void ));
extern	int	pbi_c_free			PARAMS(( void ));
extern	int	pbi_c_set			PARAMS(( void ));
extern	int	pbi_c_examine		PARAMS(( void ));

/* bdb.c */
extern	int	pbi_abolish				PARAMS(( void ));
extern	int	pbi_abolish_clausegroup	PARAMS(( void ));
extern	int	pbi_asserta				PARAMS(( void ));
extern	int	pbi_assertz				PARAMS(( void ));
extern	int	pbi_addclause			PARAMS(( void ));
extern	int	pbi_execcommand			PARAMS(( void ));
extern	int	pbi_erase				PARAMS(( void ));
extern	int	pbi_dynamic				PARAMS(( void ));
extern	int	pbi_icode				PARAMS(( void ));
extern	int	pbi_index_proc			PARAMS(( void ));
extern	int	pbi_massively_abolish_clausegroup PARAMS(( void ));
extern	int	pbi_nextproc			PARAMS(( void ));
extern	int	pbi_procinfo			PARAMS(( void ));
extern	int	pbi_clauseinfo			PARAMS(( void ));
extern	int	pbi_firstargkey			PARAMS(( void ));
extern	int	pbi_resolve_module		PARAMS(( void ));
extern	int	pbi_exported_proc		PARAMS(( void ));
extern	int	pbi_next_module			PARAMS(( void ));
extern	int	pbi_cr_mod_close		PARAMS(( void ));
extern	int	pbi_libbreak			PARAMS(( void ));
extern	int	pbi_listasm_clause		PARAMS(( void ));
extern	int	pbi_listasm_ntblentry	PARAMS(( void ));
extern	int	pbi_push_clausegroup	PARAMS(( void ));
extern	int	pbi_pop_clausegroup		PARAMS(( void ));
extern	int	pbi_collectcode			PARAMS(( void ));

	/* freeze.c */
extern	int	pbi_cptx		PARAMS(( void ));
extern	int	disp_heap		PARAMS(( void ));
extern	int     disp_item		PARAMS((void));
extern	int	pbi_swp_tr		PARAMS(( void ));
extern	int	pbi_walk_cps		PARAMS(( void ));
extern	int	pbi_delay		PARAMS(( void ));
extern  int	pbi_is_delay_var	PARAMS(( void ));
extern  int	pbi_kill_freeze	PARAMS(( void ));
extern	int 	pbi_clct_tr 		PARAMS(( void ));
extern	int 	pbi_unset_2nd 		PARAMS(( void ));
extern	int 	pbi_del_tm_for		PARAMS(( void ));
extern	int	pbi_bind_vars		PARAMS(( void ));

#ifdef INTCONSTR

	/* intaux.c */
extern int	pbi_fuzz			PARAMS((void));

	/* int_net.c */
extern int  ilinknet			PARAMS((void));
extern int reset_cstr_ctrs		PARAMS (( void ));
extern int get_cstr_ctrs_vals	PARAMS (( void ));
extern int set_max_iters_val	PARAMS (( void ));
extern int run_grteq_cstrs		PARAMS (( void ));
extern int  x_int_op			PARAMS((void));

#endif /* INTCONSTR */

#ifdef CONSTRDEBUG
extern void	debugconstr		PARAMS((void));
#endif /* CONSTRDEBUG */

	/* bdbg.c */
extern	int	pbi_dbg_nospy	PARAMS(( void ));
extern	int	pbi_dbg_spy	PARAMS(( void ));
extern	int	pbi_dbg_spyon	PARAMS(( void ));
extern	int	pbi_dbg_spyoff	PARAMS(( void ));
extern	int	pbi_dbg_spying	PARAMS(( void ));

	/* bgv.c */
extern	int	pbi_gv_alloc	PARAMS(( void ));
extern	int	pbi_gv_free	PARAMS(( void ));
extern	int	pbi_gv_get	PARAMS(( void ));
extern	int	pbi_gv_set	PARAMS(( void ));
extern	int	pbi_gv_alloc_init	PARAMS(( void ));
extern	int	pbi_gv_isfree	PARAMS(( void ));
extern	int	pbi_gv_maxpossible	PARAMS(( void ));

	/* bio.c */
extern	int	pbi_display	PARAMS(( void ));
extern	int	pbi_get	PARAMS(( void ));
extern	int	pbi_get0	PARAMS(( void ));
extern	int	pbi_load	PARAMS(( void ));
extern	int	pbi_nl	PARAMS(( void ));
extern	int	pbi_put	PARAMS(( void ));
extern	int	pbi_read	PARAMS(( void ));
extern	int	pbi_see	PARAMS(( void ));
extern	int	pbi_seeing	PARAMS(( void ));
extern	int	pbi_seen	PARAMS(( void ));
extern	int	pbi_tell	PARAMS(( void ));
extern	int	pbi_telling	PARAMS(( void ));
extern	int	pbi_told	PARAMS(( void ));
extern	int	pbi_debug	PARAMS(( void ));
extern	int	pbi_ttyflush	PARAMS(( void ));
extern	int	pbi_write	PARAMS(( void ));
extern	int	pbi_writeq	PARAMS(( void ));
extern	int	pbi_obp_open	PARAMS(( void ));
extern	int	pbi_obp_close	PARAMS(( void ));
extern	int	pbi_obp_load	PARAMS(( void ));
extern	int	pbi_obp_push_stop	PARAMS(( void ));
extern	int	pbi_obp_pop	PARAMS(( void ));
extern	int	pbi_resource_load	PARAMS(( void ));
extern	int	pbi_old_consult	PARAMS(( void ));
extern	int	pbi_save_image_with_state_to_file	PARAMS(( void ));
extern	int	pbi_attach_state_to_file	PARAMS(( void ));
extern	int	pbi_save_state_to_file	PARAMS(( void ));
extern	int	pbi_get_current_image	PARAMS(( void ));
#ifdef MacOS
extern  int	pbi_save_app_with_obp	PARAMS(( void ));
#endif
#ifdef DynamicForeign
extern	int	pbi_load_foreign	PARAMS(( void ));
#endif /* DynamicForeign */

/* bmeta.c */
#ifdef	CMeta
extern	int	pbi_true	PARAMS(( void ));
extern	int	pbi_equal	PARAMS(( void ));
extern	int	pbi_arg		PARAMS(( void ));
extern	int	pbi_mangle	PARAMS(( void ));
#ifdef TRAILVALS
extern int pbi_trailed_mangle PARAMS(( void ));
#endif
extern	int	pbi_functor	PARAMS(( void ));
extern	int	pbi_identical	PARAMS(( void ));
extern	int	pbi_unidentical	PARAMS(( void ));
extern	int	pbi_eq		PARAMS(( void ));
extern	int	pbi_noneq	PARAMS(( void ));
extern	int	pbi_var		PARAMS(( void ));
extern	int	pbi_nonvar	PARAMS(( void ));
extern	int	pbi_integer	PARAMS(( void ));
extern	int	pbi_float	PARAMS(( void ));
extern	int	pbi_number	PARAMS(( void ));
extern	int	pbi_atom	PARAMS(( void ));
extern	int	pbi_atomic	PARAMS(( void ));
extern	int	pbi_compound	PARAMS(( void ));
#endif	/* CMeta */
extern	int	pbi_findterm	PARAMS(( void ));

/* bmisc.c */
#ifdef	CMeta
extern	int	pbi_compare	PARAMS(( void ));
#endif
extern	int	pbi_hashN	PARAMS(( void ));
extern	int	pbi_gensym	PARAMS(( void ));
extern	int	pbi_isgensym	PARAMS(( void ));
extern	int	pbi_ptermaddr	PARAMS(( void ));
extern	int	pbi_traildump	PARAMS(( void ));
extern	int	pbi_frame_info	PARAMS(( void ));

/* bos.c */
extern	int	pbi_access	PARAMS(( void ));
extern	int	pbi_chdir	PARAMS(( void ));
extern	int	pbi_getenv	PARAMS(( void ));
extern  int     pbi_get_user_home PARAMS(( void ));
extern	int	pbi_system	PARAMS(( void ));
extern	int	pbi_tmpnam	PARAMS(( void ));
extern	int	pbi_protect_bottom_stack_page	PARAMS(( void ));
extern	int	pbi_get_image_dir_and_name	PARAMS(( void ));
extern int   argcount;
extern char **argvector;
extern	int pbi_command_line	PARAMS((void));
#if defined(UNIX) /* _XOPEN_CRYPT */
extern	int pbi_crypt			PARAMS((void));
#endif
extern	int pbi_copy_file		PARAMS((void));

/* bparser.c */
extern	int	pbi_op		PARAMS(( void ));
extern	int	pbi_tokid	PARAMS(( void ));
extern	int	pbi_name	PARAMS(( void ));
extern	int	pbi_atom_chars	PARAMS(( void ));
extern	int	pbi_atom_codes	PARAMS(( void ));
extern	int	pbi_atom_length	PARAMS(( void ));
extern	int	pbi_sub_atom	PARAMS(( void ));
extern	int	pbi_char_code	PARAMS(( void ));
extern	int	pbi_uia_alloc	PARAMS(( void ));
extern	int	pbi_uia_clip	PARAMS(( void ));
extern	int	pbi_uia_size	PARAMS(( void ));
extern	int	pbi_uia_peek	PARAMS(( void ));
extern	int	pbi_uia_peekb	PARAMS(( void ));
extern	int	pbi_uia_peekw	PARAMS(( void ));
extern	int	pbi_uia_peekl	PARAMS(( void ));
extern	int	pbi_uia_peekd	PARAMS(( void ));
extern	int	pbi_uia_peeks	PARAMS(( void ));
extern	int	pbi_uia_peeks4	PARAMS(( void ));
extern	int	pbi_uia_poke	PARAMS(( void ));
extern	int	pbi_uia_pokeb	PARAMS(( void ));
extern	int	pbi_uia_pokew	PARAMS(( void ));
extern	int	pbi_uia_pokel	PARAMS(( void ));
extern	int	pbi_uia_poked	PARAMS(( void ));
extern	int	pbi_uia_pokes	PARAMS(( void ));
extern	int	pbi_strlen	PARAMS(( void ));
extern	int	pbi_atom_concat	PARAMS(( void ));

/* bsio.c */
extern	int	sio_mkstream	PARAMS(( void ));
extern	int	sio_errcode	PARAMS(( void ));
extern	int	sio_set_errcode	PARAMS(( void ));
extern	int	sio_errno	PARAMS(( void ));
extern	int	sio_aux	PARAMS(( void ));
extern	int	sio_fd	PARAMS(( void ));
extern	int	sio_cpos	PARAMS(( void ));
extern	int	sio_lpos	PARAMS(( void ));
extern	int	sio_buf_params	PARAMS(( void ));
extern	int	sio_increment_bufpos	PARAMS(( void ));
extern	int	sio_set_position	PARAMS(( void ));
extern	int	sio_set_eof	PARAMS(( void ));
extern	int	sio_reset_eof	PARAMS(( void ));
extern	int	sio_file_open	PARAMS(( void ));
extern	int	sio_console_open	PARAMS(( void ));
#ifdef SysVIPC
extern	int	sio_sysVq_open	PARAMS(( void ));
extern	int	pbi_ftok	PARAMS(( void ));
extern	int	pbi_msgctl	PARAMS(( void ));
#endif
#ifdef SSBQ
extern	int	sio_ssbq_open	PARAMS(( void ));
#endif
#ifdef HAVE_SOCKET
extern	int	sio_nsocket		PARAMS(( void ));
extern	int	sio_nsocket_connect	PARAMS(( void ));
extern	int	sio_nsocket_bind	PARAMS(( void ));
extern	int	sio_nsocket_listen	PARAMS(( void ));
extern	int	sio_nsocket_accept	PARAMS(( void ));
extern	int	sio_nsocket_close	PARAMS(( void ));
extern	int	sio_nsocket_select	PARAMS(( void ));
extern	int	sio_nsocketpair	        PARAMS(( void ));

extern	int	sio_nsocket_open	PARAMS(( void ));

extern int pbi_gethostbyname(void);
extern int pbi_gethostbyaddr(void);

extern	int	sio_gethostname	PARAMS(( void ));
extern	int	sio_socket_open	PARAMS(( void ));
extern	int	sio_is_server_socket	PARAMS(( void ));
extern	int	sio_accept_socket_connection	PARAMS(( void ));
extern	int	sio_poll		PARAMS(( void ));
extern	int	sio_simple_select		PARAMS(( void ));
#endif /* HAVE_SOCKET */
extern	int	sio_fork	PARAMS(( void ));
extern	int	sio_rexec	PARAMS(( void ));
extern	int	sio_generic_open	PARAMS(( void ));
extern	int	sio_close	PARAMS(( void ));
#ifndef SIO_ASM
extern	int	sio_get_byte	PARAMS(( void ));
extern	int	sio_put_byte	PARAMS(( void ));
#endif /* SIO_ASM */
extern	int	sio_unget_byte	PARAMS(( void ));
extern	int	sio_getpos	PARAMS(( void ));
extern	int	sio_seek	PARAMS(( void ));
extern	int	sio_readbuffer	PARAMS(( void ));
extern	int	sio_set_console_prompt	PARAMS(( void ));
extern	int	sio_set_history_file	PARAMS(( void ));
extern	int	sio_set_no_load_prev_history	PARAMS(( void ));
extern	int	sio_writebuffer	PARAMS(( void ));
extern	int	sio_bufshift	PARAMS(( void ));
extern	int	sio_next_token	PARAMS(( void ));
extern	int	sio_next_tokens	PARAMS(( void ));
extern	int	sio_skip_layout	PARAMS(( void ));
extern	int	sio_linenumber	PARAMS(( void ));
extern	int	sio_put_atom	PARAMS(( void ));
extern	int	sio_put_number	PARAMS(( void ));
extern	int	sio_get_number	PARAMS(( void ));
extern	int	sio_qatom	PARAMS(( void ));
extern	int	sio_var_to_atom	PARAMS(( void ));
extern	int	sio_lettervar	PARAMS(( void ));
extern	int	sio_sprintf	PARAMS(( void ));
extern	int sio_sprintf_number PARAMS((void));
extern	int	sio_isgraphicatom	PARAMS(( void ));
extern	int	sio_readln	PARAMS(( void ));
extern	int	sio_nl	PARAMS(( void ));
extern	int	sio_position_in_line	PARAMS(( void ));

/* bsystem.c */
extern	int	pbi_ouch		PARAMS(( void ));
extern	int	pbi_forceCtlC		PARAMS(( void ));
extern	int	pbi_forcePrologError	PARAMS(( void ));
extern	int	pbi_reset_wm_normal	PARAMS(( void ));
extern	int	pbi_showanswers		PARAMS(( void ));
extern	int	pbi_halt		PARAMS(( void ));
extern	int	pbi_statistics		PARAMS(( void ));
extern	int	pbi_stack_overflow	PARAMS(( void ));
extern	int	pbi_stack_info		PARAMS(( void ));
#ifdef MacOS
extern	int	pbi_debugger		PARAMS(( void ));
#endif
extern	int	pbi_printno		PARAMS(( void ));
extern	int	pbi_printwarning	PARAMS(( void ));
/* extern	int pbi_limits_info		PARAMS(( void )); */
#if	defined(Portable) && defined(IProfile)
extern	int	pbi_init_iprofile	PARAMS(( void ));
extern	int	pbi_dump_iprofile	PARAMS(( void ));
#endif	/* defined(Portable) && defined(IProfile) */

/* gc.c */
extern	int	gc		PARAMS(( void ));

/* sig.c */
extern	int	pbi_alarm	PARAMS(( void ));
extern	int	pbi_signal_name	PARAMS(( void ));

/* wam.c -- byte only */
#ifdef TRACEBWAM
extern	int 	toggle_bwam 	PARAMS( ( void ) );
#endif

/* lforeign.c */
extern int prolog_load_plugin(void);


#ifdef SUBTYPES
extern	int	pbi_less_sut_int	PARAMS(( void ));
extern	int	pbi_eq_sut_int	PARAMS(( void ));
extern	int	pbi_mk_sut_int	PARAMS(( void ));
extern	int	pbi_t_sut_int	PARAMS(( void ));
extern	int	pbi_pos_atom	PARAMS(( void ));
#endif /* SUBTYPES */
