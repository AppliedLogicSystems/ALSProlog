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
extern	int	get_number	(PWord, int, double *);

/* butil.c */
extern	void	heap_copy		(PWord *, int *, pword);
extern	int	xform_uia			(PWord *, int *);
extern	int	force_uia			(PWord *, int *);
extern	void	string_to_list	(PWord *, int *, UCHAR *);
extern	int	list_to_string		(UCHAR *, PWord, int);
extern	int	getstring			(UCHAR **, PWord, int);
extern	int	getlong				(long *, PWord, int);
extern	int	get_gv_number		(UCHAR *);
extern	void set_prolog_error	(PWord, int, PWord, int, PWord, PWord, int, PWord, int);
extern	int	getdouble			(double *, PWord, int);

/* wdisp.c */
extern	void	prolog_write	(PWord, int);
extern	void	prolog_writeq	(PWord, int);
extern	void	prolog_display	(PWord, int);

/* built.c */
extern	void	builtin_init			(void);
extern	void	time_cut_interrupt_init	(void);
extern	int	pbi_set_interrupt_vector	(void);
extern  int     pbi_uncaught_interrupt 	(void);

/* bmisc.c */
#ifdef CMeta
extern	int	wm_identical	( PWord, int, PWord, int );
#endif

/* arith.c */
extern	int	pbi_time			( void );
extern	int	pbi_less			( void );
extern	int	pbi_greater			( void );
extern	int	pbi_equalorless		( void );
extern	int	pbi_greaterorequal 	( void );
extern	int	pbi_arithequal		( void );
extern	int	pbi_arithnotequal 	( void );
extern	int	pbi_is				( void );
extern	int	pbi_srandom			( void );
/* fpbasis.c */
extern	int pbi_fpconst_val			(void);
extern	int pbi_uia_poke_fpconst	(void);

/* from bcinter.c */
#ifdef DOS
extern	int	pbi_c_make_farptr ( void );
#endif
extern	int	pbi_c_malloc		( void );
extern	int	pbi_c_free			( void );
extern	int	pbi_c_set			( void );
extern	int	pbi_c_examine		( void );

/* bdb.c */
extern	int	pbi_abolish				( void );
extern	int	pbi_abolish_clausegroup	( void );
extern	int	pbi_asserta				( void );
extern	int	pbi_assertz				( void );
extern	int	pbi_addclause			( void );
extern	int	pbi_execcommand			( void );
extern	int	pbi_erase				( void );
extern	int	pbi_dynamic				( void );
extern	int	pbi_icode				( void );
extern	int	pbi_index_proc			( void );
extern	int	pbi_massively_abolish_clausegroup ( void );
extern	int	pbi_nextproc			( void );
extern	int	pbi_procinfo			( void );
extern	int	pbi_clauseinfo			( void );
extern	int	pbi_firstargkey			( void );
extern	int	pbi_resolve_module		( void );
extern	int	pbi_exported_proc		( void );
extern	int	pbi_next_module			( void );
extern	int	pbi_cr_mod_close		( void );
extern	int	pbi_libbreak			( void );
extern	int	pbi_listasm_clause		( void );
extern	int	pbi_listasm_ntblentry	( void );
extern	int	pbi_push_clausegroup	( void );
extern	int	pbi_pop_clausegroup		( void );
extern	int	pbi_collectcode			( void );

	/* freeze.c */
extern	int	pbi_cptx		( void );
extern	int	disp_heap		( void );
extern	int     disp_item		(void);
extern	int	pbi_swp_tr		( void );
extern	int	pbi_walk_cps		( void );
extern	int	pbi_delay		( void );
extern  int	pbi_is_delay_var	( void );
extern  int	pbi_kill_freeze	( void );
extern	int 	pbi_clct_tr 		( void );
extern	int 	pbi_unset_2nd 		( void );
extern	int 	pbi_del_tm_for		( void );
extern	int	pbi_bind_vars		( void );
extern	int	pbi_cptz		( void );
extern	int	x_disp_heap		( void );
extern	int	pbi_x_swp_tr		( void );

#ifdef INTCONSTR

	/* intaux.c */
extern int	pbi_fuzz			(void);

	/* int_net.c */
extern int  ilinknet			(void);
extern int reset_cstr_ctrs		( void );
extern int get_cstr_ctrs_vals	( void );
extern int set_max_iters_val	( void );
extern int run_grteq_cstrs		( void );
extern int  x_int_op			(void);

#endif /* INTCONSTR */

#ifdef CONSTRDEBUG
extern void	debugconstr		(void);
#endif /* CONSTRDEBUG */

	/* bdbg.c */
extern	int	pbi_dbg_nospy	( void );
extern	int	pbi_dbg_spy	( void );
extern	int	pbi_dbg_spyon	( void );
extern	int	pbi_dbg_spyoff	( void );
extern	int	pbi_dbg_spying	( void );

	/* bgv.c */
extern	int	pbi_gv_alloc	( void );
extern	int	pbi_gv_free	( void );
extern	int	pbi_gv_get	( void );
extern	int	pbi_gv_set	( void );
extern	int	pbi_gv_alloc_init	( void );
extern	int	pbi_gv_isfree	( void );
extern	int	pbi_gv_maxpossible	( void );

	/* bio.c */
extern	int	pbi_display	( void );
extern	int	pbi_get	( void );
extern	int	pbi_get0	( void );
extern	int	pbi_load	( void );
extern	int	pbi_nl	( void );
extern	int	pbi_put	( void );
extern	int	pbi_read	( void );
extern	int	pbi_see	( void );
extern	int	pbi_seeing	( void );
extern	int	pbi_seen	( void );
extern	int	pbi_tell	( void );
extern	int	pbi_telling	( void );
extern	int	pbi_told	( void );
extern	int	pbi_debug	( void );
extern	int	pbi_ttyflush	( void );
extern	int	pbi_write	( void );
extern	int	pbi_writeq	( void );
extern	int	pbi_obp_open	( void );
extern	int	pbi_obp_close	( void );
extern	int	pbi_obp_load	( void );
extern	int	pbi_obp_push_stop	( void );
extern	int	pbi_obp_pop	( void );
extern	int	pbi_resource_load	( void );
extern	int	pbi_old_consult	( void );
extern	int	pbi_save_image_with_state_to_file	( void );
extern	int	pbi_attach_state_to_file	( void );
extern	int	pbi_save_state_to_file	( void );
extern	int	pbi_get_current_image	( void );
#ifdef MacOS
extern  int	pbi_save_app_with_obp	( void );
#endif
#ifdef DynamicForeign
extern	int	pbi_load_foreign	( void );
#endif /* DynamicForeign */

/* bmeta.c */
#ifdef	CMeta
extern	int	pbi_true	( void );
extern	int	pbi_equal	( void );
extern	int	pbi_arg		( void );
extern	int	pbi_mangle	( void );
#ifdef TRAILVALS
extern int pbi_trailed_mangle ( void );
#endif
extern	int	pbi_functor	( void );
extern	int	pbi_identical	( void );
extern	int	pbi_unidentical	( void );
extern	int	pbi_eq		( void );
extern	int	pbi_noneq	( void );
extern	int	pbi_var		( void );
extern	int	pbi_nonvar	( void );
extern	int	pbi_integer	( void );
extern	int	pbi_float	( void );
extern	int	pbi_number	( void );
extern	int	pbi_atom	( void );
extern	int	pbi_atomic	( void );
extern	int	pbi_compound	( void );
#endif	/* CMeta */
extern	int	pbi_findterm	( void );

/* bmisc.c */
#ifdef	CMeta
extern	int	pbi_compare	( void );
#endif
extern	int	pbi_hashN	( void );
extern	int	pbi_gensym	( void );
extern	int	pbi_isgensym	( void );
extern	int	pbi_ptermaddr	( void );
extern	int	pbi_traildump	( void );
extern	int	pbi_frame_info	( void );

/* bos.c */
extern	int	pbi_access	( void );
extern	int	pbi_chdir	( void );
extern	int	pbi_getenv	( void );
extern  int     pbi_get_user_home ( void );
extern	int	pbi_system	( void );
extern	int	pbi_tmpnam	( void );
extern	int	pbi_protect_bottom_stack_page	( void );
extern	int	pbi_get_image_dir_and_name	( void );
extern int   argcount;
extern char **argvector;
extern	int pbi_command_line	(void);
#if defined(UNIX) /* _XOPEN_CRYPT */
extern	int pbi_crypt			(void);
#endif
extern	int pbi_copy_file		(void);

/* bparser.c */
extern	int	pbi_op		( void );
extern	int	pbi_tokid	( void );
extern	int	pbi_name	( void );
extern	int	pbi_atom_chars	( void );
extern	int	pbi_atom_codes	( void );
extern	int	pbi_atom_length	( void );
extern	int	pbi_sub_atom	( void );
extern	int	pbi_char_code	( void );
extern	int	pbi_uia_alloc	( void );
extern	int	pbi_uia_clip	( void );
extern	int	pbi_uia_size	( void );
extern	int	pbi_uia_peek	( void );
extern	int	pbi_uia_peekb	( void );
extern	int	pbi_uia_peekw	( void );
extern	int	pbi_uia_peekl	( void );
extern	int	pbi_uia_peekd	( void );
extern	int	pbi_uia_peeks	( void );
extern	int	pbi_uia_peeks4	( void );
extern	int	pbi_uia_poke	( void );
extern	int	pbi_uia_pokeb	( void );
extern	int	pbi_uia_pokew	( void );
extern	int	pbi_uia_pokel	( void );
extern	int	pbi_uia_poked	( void );
extern	int	pbi_uia_pokes	( void );
extern	int	pbi_atom_concat	( void );

/* bsio.c */
extern	int	sio_mkstream	( void );
extern	int	sio_errcode	( void );
extern	int	sio_set_errcode	( void );
extern	int	sio_errno	( void );
extern	int	sio_aux	( void );
extern	int	sio_fd	( void );
extern	int	sio_cpos	( void );
extern	int	sio_lpos	( void );
extern	int	sio_buf_params	( void );
extern	int	sio_increment_bufpos	( void );
extern	int	sio_set_position	( void );
extern	int	sio_set_eof	( void );
extern	int	sio_reset_eof	( void );
extern	int	sio_file_open	( void );
extern	int	sio_console_open	( void );
#ifdef SysVIPC
extern	int	sio_sysVq_open	( void );
extern	int	pbi_ftok	( void );
extern	int	pbi_msgctl	( void );
#endif
#ifdef SSBQ
extern	int	sio_ssbq_open	( void );
#endif
#ifdef HAVE_SOCKET
extern	int	sio_nsocket		( void );
extern	int	sio_nsocket_connect	( void );
extern	int	sio_nsocket_bind	( void );
extern	int	sio_nsocket_listen	( void );
extern	int	sio_nsocket_accept	( void );
extern	int	sio_nsocket_close	( void );
extern	int	sio_nsocket_select	( void );
extern	int	sio_nsocketpair	        ( void );

extern	int	sio_nsocket_open	( void );

extern int pbi_gethostbyname(void);
extern int pbi_gethostbyaddr(void);

extern	int	sio_gethostname	( void );
extern	int	sio_socket_open	( void );
extern	int	sio_is_server_socket	( void );
extern	int	sio_accept_socket_connection	( void );
extern	int	sio_poll		( void );
extern	int	sio_simple_select		( void );
#endif /* HAVE_SOCKET */
extern	int	sio_fork	( void );
extern	int	sio_rexec	( void );
extern	int	sio_generic_open	( void );
extern	int	sio_close	( void );
#ifndef SIO_ASM
extern	int	sio_get_byte	( void );
extern	int	sio_put_byte	( void );
#endif /* SIO_ASM */
extern	int	sio_unget_byte	( void );
extern	int	sio_getpos	( void );
extern	int	sio_seek	( void );
extern	int	sio_readbuffer	( void );
extern  int     sio_set_do_lineedit ( void );
extern  int     sio_set_lineedit_prompt  ( void );
extern  int     sio_set_history_file    ( void );
extern  int     sio_set_no_load_prev_history    ( void );
extern	int	sio_writebuffer	( void );
extern	int	sio_bufshift	( void );
extern	int	sio_next_token	( void );
extern	int	sio_next_tokens	( void );
extern	int	sio_skip_layout	( void );
extern	int	sio_linenumber	( void );
extern	int	sio_put_atom	( void );
extern	int	sio_put_number	( void );
extern	int	sio_get_number	( void );
extern	int	sio_qatom	( void );
extern	int	sio_var_to_atom	( void );
extern	int	sio_lettervar	( void );
extern	int	sio_sprintf	( void );
extern	int sio_sprintf_number (void);
extern	int	sio_isgraphicatom	( void );
extern	int	sio_readln	( void );
extern	int	sio_nl	( void );
extern	int	sio_position_in_line	( void );

/* bsystem.c */
extern	int	pbi_ouch		( void );
extern	int	pbi_forceCtlC		( void );
extern	int	pbi_forcePrologError	( void );
extern	int	pbi_reset_wm_normal	( void );
extern	int	pbi_showanswers		( void );
extern	int	pbi_halt		( void );
extern	int	pbi_statistics		( void );
extern	int	pbi_stack_overflow	( void );
extern	int	pbi_stack_info		( void );
#ifdef MacOS
extern	int	pbi_debugger		( void );
#endif
extern	int	pbi_printno		( void );
extern	int	pbi_printwarning	( void );
/* extern	int pbi_limits_info		( void ); */
#if	defined(Portable) && defined(IProfile)
extern	int	pbi_init_iprofile	( void );
extern	int	pbi_dump_iprofile	( void );
#endif	/* defined(Portable) && defined(IProfile) */

/* gc.c */
extern	int	gc		( void );

/* sig.c */
extern	int	pbi_alarm	( void );
extern	int	pbi_signal_name	( void );

/* wam.c -- byte only */
#ifdef TRACEBWAM
extern	int 	toggle_bwam 	( void );
#endif

/* lforeign.c */
extern int prolog_load_plugin(void);


#ifdef SUBTYPES
extern	int	pbi_less_sut_int	( void );
extern	int	pbi_eq_sut_int	( void );
extern	int	pbi_mk_sut_int	( void );
extern	int	pbi_t_sut_int	( void );
extern	int	pbi_pos_atom	( void );
#endif /* SUBTYPES */

extern	int	curl_c_builtin	( void );
extern	int	lookup_opt_info	( void );
