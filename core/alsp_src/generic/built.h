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
	    set_prolog_error(hpe, namtok,arity, TK_INSTANTIATION_ERROR,0, \
			     0, 0,0, 0,0); 			\
	    FAIL;						\
	} while (0)

#define PERR_TYPE(namtok,arity,typesym,cv,ct)			\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_TYPE_ERROR,2, 	\
			     typesym, cv,ct, 0,0);		\
	    FAIL;						\
	} while (0)

#define PERR_DOMAIN(namtok,arity,dsym,cv,ct)			\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_DOMAIN_ERROR,2,	\
			     dsym, cv,ct, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_EXISTENCE(namtok,arity,obsym,cv,ct)		\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_EXISTENCE_ERROR,2, \
			     obsym, cv,ct, 0,0);		\
	    FAIL;						\
	} while (0)

#define PERR_PERMISSION(namtok,arity,opsym,typesym,cv,ct)	\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_PERMISSION_ERROR,2,	\
			     opsym, typesym,WTP_SYMBOL, cv,ct); \
	    FAIL;						\
	} while (0)

#define PERR_REPRESENTATION(namtok,arity,flagsym)		\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_REPRESENTATION_ERROR,1,	\
			     flagsym,0,0,0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_EVALUATION(namtok,arity,errsym)			\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_EVALUATION_ERROR,1, \
			     errsym, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_RESOURCE(namtok,arity,errsym)			\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_RESOURCE_ERROR,1, \
			     errsym, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

#define PERR_SYSTEM(namtok,arity)				\
	do {							\
	    set_prolog_error(hpe, namtok,arity, TK_SYSTEM_ERROR,0,	\
			     0, 0,0, 0,0);			\
	    FAIL;						\
	} while (0)

/* bparser.c */
extern	int	get_number	 (PE,PWord, int, double *);

/* butil.c */
extern	void	heap_copy		 (PE, PWord *, int *, pword);
extern	int	xform_uia			 (PE, PWord *, int *);
extern	int	force_uia			 (PE, PWord *, int *);
//extern	void	string_to_list	 (PWord *, int *, UCHAR *);
extern	void	string_to_list	 (PE, PWord *, int *, char *);
extern	int	list_to_string		 (PE, UCHAR *, PWord, int);
extern	int	getstring_pe			 (PE, UCHAR **, PWord, int);
#define getstring(a,b,c)	getstring_pe(hpe,a,b,c)
extern	int	getlong				 (PE, long *, PWord, int);
extern	int	get_gv_number		 (PE, UCHAR *);
extern	void set_prolog_error	(PE, PWord, int, PWord, int, PWord, PWord, int, PWord, int);
extern	int	getdouble_pe			 (PE,double *, PWord, int);
#define getdouble(a,b,c)	getdouble_pe(hpe,a,b,c)

/* wdisp.c */
extern	void	prolog_write	 (PE, PWord, int);
extern	void	prolog_writeq	 (PE, PWord, int);
extern	void	prolog_display	 (PE, PWord, int);

/* built.c */
extern	void	builtin_init			 (PE);
extern	void	time_cut_interrupt_init	 (PE);
extern	int	pbi_set_interrupt_vector	(PE);
extern  int     pbi_uncaught_interrupt 	 (PE);

/* bmisc.c */
#ifdef CMeta
extern	int	wm_identical	(PE, PWord, int, PWord, int );
#endif

/* arith.c */
extern	int	pbi_time			( PE );
extern	int	pbi_less			( PE );
extern	int	pbi_greater			( PE );
extern	int	pbi_equalorless		( PE );
extern	int	pbi_greaterorequal 	( PE );
extern	int	pbi_arithequal		( PE );
extern	int	pbi_arithnotequal 	( PE );
extern	int	pbi_is				( PE );
extern	int	pbi_srandom			( PE );
/* fpbasis.c */
extern	int pbi_fpconst_val			 (PE);
extern	int pbi_uia_poke_fpconst	 (PE);

/* from bcinter.c */
#ifdef DOS
extern	int	pbi_c_make_farptr ( PE );
#endif
extern	int	pbi_c_malloc		( PE );
extern	int	pbi_c_free			( PE );
extern	int	pbi_c_set			( PE );
extern	int	pbi_c_examine		( PE );

/* bdb.c */
extern	int	pbi_abolish				( PE );
extern	int	pbi_abolish_clausegroup	( PE );
extern	int	pbi_asserta				( PE );
extern	int	pbi_assertz				( PE );
extern	int	pbi_addclause			( PE );
extern	int	pbi_execcommand			( PE );
extern	int	pbi_erase				( PE );
extern	int	pbi_dynamic				( PE );
extern	int	pbi_icode				( PE );
extern	int	pbi_index_proc			( PE );
extern	int	pbi_massively_abolish_clausegroup ( PE );
extern	int	pbi_nextproc			( PE );
extern	int	pbi_procinfo			( PE );
extern	int	pbi_clauseinfo			( PE );
extern	int	pbi_firstargkey			( PE );
extern	int	pbi_resolve_module		( PE );
extern	int	pbi_exported_proc		( PE );
extern	int	pbi_next_module			( PE );
extern	int	pbi_cr_mod_close		( PE );
extern	int	pbi_libbreak			( PE );
extern	int	pbi_listasm_clause		( PE );
extern	int	pbi_listasm_ntblentry	( PE );
extern	int	pbi_push_clausegroup	( PE );
extern	int	pbi_pop_clausegroup		( PE );
extern	int	pbi_collectcode			( PE );

	/* freeze.c */
extern	int	pbi_cptx		( PE );
extern	int	disp_heap		( PE );
extern	int     disp_item		(PE);
extern	int	pbi_swp_tr		( PE );
extern	int	pbi_walk_cps		( PE );
extern	int	pbi_delay		( PE );
extern  int	pbi_is_delay_var	( PE );
extern  int	pbi_kill_freeze	( PE );
extern	int 	pbi_clct_tr 		( PE );
extern	int 	pbi_unset_2nd 		( PE );
extern	int 	pbi_del_tm_for		( PE );
extern	int	pbi_bind_vars		( PE );

#ifdef INTCONSTR

	/* intaux.c */
extern int	pbi_fuzz			(PE);

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
extern	int	pbi_dbg_nospy	( PE );
extern	int	pbi_dbg_spy	( PE );
extern	int	pbi_dbg_spyon	( PE );
extern	int	pbi_dbg_spyoff	( PE );
extern	int	pbi_dbg_spying	( PE );

	/* bgv.c */
extern	int	pbi_gv_alloc	( PE );
extern	int	pbi_gv_free	( PE );
extern	int	pbi_gv_get	( PE );
extern	int	pbi_gv_set	( PE );
extern	int	pbi_gv_alloc_init	( PE );
extern	int	pbi_gv_isfree	( PE );
extern	int	pbi_gv_maxpossible	( PE );

	/* bio.c */
extern	int	pbi_display	( PE );
extern	int	pbi_get	( PE );
extern	int	pbi_get0	( PE );
extern	int	pbi_load	( PE );
extern	int	pbi_nl	( PE );
extern	int	pbi_put	( PE );
extern	int	pbi_read	( PE );
extern	int	pbi_see	( PE );
extern	int	pbi_seeing	( PE );
extern	int	pbi_seen	( PE );
extern	int	pbi_tell	( PE );
extern	int	pbi_telling	( PE );
extern	int	pbi_told	( PE );
extern	int	pbi_debug	( PE );
extern	int	pbi_ttyflush	( PE );
extern	int	pbi_write	( PE );
extern	int	pbi_writeq	( PE );
extern	int	pbi_obp_open	( PE );
extern	int	pbi_obp_close	( PE );
extern	int	pbi_obp_load	( PE );
extern	int	pbi_obp_push_stop	( PE );
extern	int	pbi_obp_pop	( PE );
extern	int	pbi_resource_load	( PE );
extern	int	pbi_old_consult	( PE );
extern	int	pbi_save_image_with_state_to_file	( PE );
extern	int	pbi_attach_state_to_file	( PE );
extern	int	pbi_save_state_to_file	( PE );
extern	int	pbi_get_current_image	( PE );
#ifdef MacOS
extern  int	pbi_save_app_with_obp	( PE );
#endif
#ifdef DynamicForeign
extern	int	pbi_load_foreign	( PE );
#endif /* DynamicForeign */

/* bmeta.c */
#ifdef	CMeta
extern	int	pbi_true	( PE );
extern	int	pbi_equal	( PE );
extern	int	pbi_arg		( PE );
extern	int	pbi_mangle	( PE );
#ifdef TRAILVALS
extern int pbi_trailed_mangle ( PE );
#endif
extern	int	pbi_functor	( PE );
extern	int	pbi_identical	( PE );
extern	int	pbi_unidentical	( PE );
extern	int	pbi_eq		( PE );
extern	int	pbi_noneq	( PE );
extern	int	pbi_var		( PE );
extern	int	pbi_nonvar	( PE );
extern	int	pbi_integer	( PE );
extern	int	pbi_float	( PE );
extern	int	pbi_number	( PE );
extern	int	pbi_atom	( PE );
extern	int	pbi_atomic	( PE );
extern	int	pbi_compound	( PE );
#endif	/* CMeta */
extern	int	pbi_findterm	( PE );

/* bmisc.c */
#ifdef	CMeta
extern	int	pbi_compare	( PE );
#endif
extern	int	pbi_hashN	( PE );
extern	int	pbi_gensym	( PE );
extern	int	pbi_isgensym	( PE );
extern	int	pbi_ptermaddr	( PE );
extern	int	pbi_traildump	( PE );
extern	int	pbi_frame_info	( PE );

/* bos.c */
extern	int	pbi_access	( PE );
extern	int	pbi_chdir	( PE );
extern	int	pbi_getenv	( PE );
extern  int     pbi_get_user_home ( PE );
extern	int	pbi_system	( PE );
extern	int	pbi_tmpnam	( PE );
extern	int	pbi_protect_bottom_stack_page	( PE );
extern	int	pbi_get_image_dir_and_name	( PE );
extern int   argcount;
extern char **argvector;
extern	int pbi_command_line	(PE);
#ifdef UNIX
extern	int pbi_crypt			(PE);
#endif
extern	int pbi_copy_file		(PE);

extern int pbi_new_thread(PE);
extern int pbi_send(PE);
extern int pbi_receive(PE);
extern void thread_init(void);

/* bparser.c */
extern	int	pbi_op		( PE );
extern	int	pbi_tokid	( PE );
extern	int	pbi_name	( PE );
extern	int	pbi_atom_chars	( PE );
extern	int	pbi_atom_codes	( PE );
extern	int	pbi_atom_length	( PE );
extern	int	pbi_sub_atom	( PE );
extern	int	pbi_char_code	( PE );
extern	int	pbi_uia_alloc	( PE );
extern	int	pbi_uia_clip	( PE );
extern	int	pbi_uia_size	( PE );
extern	int	pbi_uia_peek	( PE );
extern	int	pbi_uia_peekb	( PE );
extern	int	pbi_uia_peekw	( PE );
extern	int	pbi_uia_peekl	( PE );
extern	int	pbi_uia_peekd	( PE );
extern	int	pbi_uia_peeks	( PE );
extern	int	pbi_uia_peeks4	( PE );
extern	int	pbi_uia_poke	( PE );
extern	int	pbi_uia_pokeb	( PE );
extern	int	pbi_uia_pokew	( PE );
extern	int	pbi_uia_pokel	( PE );
extern	int	pbi_uia_poked	( PE );
extern	int	pbi_uia_pokes	( PE );
extern	int	pbi_strlen	( PE );
extern	int	pbi_atom_concat	( PE );

/* bsio.c */
extern	int	sio_mkstream	( PE );
extern	int	sio_errcode	( PE );
extern	int	sio_set_errcode	( PE );
extern	int	sio_errno	( PE );
extern	int	sio_aux	( PE );
extern	int	sio_fd	( PE );
extern	int	sio_cpos	( PE );
extern	int	sio_lpos	( PE );
extern	int	sio_buf_params	( PE );
extern	int	sio_increment_bufpos	( PE );
extern	int	sio_set_position	( PE );
extern	int	sio_set_eof	( PE );
extern	int	sio_reset_eof	( PE );
extern	int	sio_file_open	( PE );
extern	int	sio_console_open	( PE );
#ifdef SysVIPC
extern	int	sio_sysVq_open	( PE );
extern	int	pbi_ftok	( PE );
extern	int	pbi_msgctl	( PE );
#endif
#ifdef SSBQ
extern	int	sio_ssbq_open	( PE );
#endif
#ifdef HAVE_SOCKET
extern	int	sio_nsocket		( PE );
extern	int	sio_nsocket_connect	( PE );
extern	int	sio_nsocket_bind	( PE );
extern	int	sio_nsocket_listen	( PE );
extern	int	sio_nsocket_accept	( PE );
extern	int	sio_nsocket_close	( PE );
extern	int	sio_nsocket_select	( PE );
extern	int	sio_nsocketpair	    ( PE );

extern	int	sio_nsocket_open	( PE );

extern int pbi_gethostbyname(PE);
extern int pbi_gethostbyaddr(PE);

extern	int	sio_gethostname	( PE );
extern	int	sio_socket_open	( PE );
extern	int	sio_is_server_socket	( PE );
extern	int	sio_accept_socket_connection	( PE );
extern	int	sio_poll		( PE );
extern	int	sio_simple_select		( PE );
#endif /* HAVE_SOCKET */
extern	int	sio_fork	( PE );
extern	int	sio_rexec	( PE );
extern	int	sio_generic_open	( PE );
extern	int	sio_close	( PE );
#ifndef SIO_ASM
extern	int	sio_get_byte	( PE );
extern	int	sio_put_byte	( PE );
#endif /* SIO_ASM */
extern	int	sio_unget_byte	( PE );
extern	int	sio_getpos	( PE );
extern	int	sio_seek	( PE );
extern	int	sio_readbuffer	( PE );
extern	int	sio_writebuffer	( PE );
extern	int	sio_bufshift	( PE );
extern	int	sio_next_token	( PE );
extern	int	sio_next_tokens	( PE );
extern	int	sio_skip_layout	( PE );
extern	int	sio_linenumber	( PE );
extern	int	sio_put_atom	( PE );
extern	int	sio_put_number	( PE );
extern	int	sio_get_number	( PE );
extern	int	sio_qatom	( PE );
extern	int	sio_var_to_atom	( PE );
extern	int	sio_lettervar	( PE );
extern	int	sio_sprintf	( PE );
extern	int sio_sprintf_number (PE);
extern	int	sio_isgraphicatom	( PE );
extern	int	sio_readln	( PE );
extern	int	sio_nl	( PE );
extern	int	sio_position_in_line	( PE );

/* bsystem.c */
extern	int	pbi_ouch		( PE );
extern	int	pbi_forceCtlC		( PE );
extern	int	pbi_forcePrologError	( PE );
extern	int	pbi_reset_wm_normal	( PE );
extern	int	pbi_showanswers		( PE );
extern	int	pbi_halt		( PE );
extern	int	pbi_statistics		( PE );
extern	int	pbi_stack_overflow	( PE );
extern	int	pbi_stack_info		( PE );
#ifdef MacOS
extern	int	pbi_debugger		( PE );
#endif
extern	int	pbi_printno		( PE );
extern	int	pbi_printwarning	( PE );
/* extern	int pbi_limits_info		( PE ); */
#if	defined(Portable) && defined(IProfile)
extern	int	pbi_init_iprofile	( PE );
extern	int	pbi_dump_iprofile	( PE );
#endif	/* defined(Portable) && defined(IProfile) */

/* gc.c */
extern	int	gc		( PE );

/* sig.c */
extern	int	pbi_alarm	( PE );
extern	int	pbi_signal_name	( PE );

/* wam.c -- byte only */
#ifdef TRACEBWAM
extern	int 	toggle_bwam 	 ( PE );
#endif

/* lforeign.c */
extern int prolog_load_plugin(PE);


#ifdef SUBTYPES
extern	int	pbi_less_sut_int	( PE );
extern	int	pbi_eq_sut_int	( PE );
extern	int	pbi_mk_sut_int	( PE );
extern	int	pbi_t_sut_int	( PE );
extern	int	pbi_pos_atom	( PE );
#endif /* SUBTYPES */
