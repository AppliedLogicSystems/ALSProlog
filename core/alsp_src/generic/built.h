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

#define PERR_CALCULATION(namtok,arity,errsym)			\
	do {							\
	    set_prolog_error(namtok,arity, TK_CALCULATION_ERROR,1, \
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

/* arith.c */
extern	void	init_time	PARAMS( (void) );
/* bparser.c */
extern	int	get_number	PARAMS( (PWord, int, double *) );

/* butil.c */
extern	void	heap_copy	PARAMS( (PWord *, int *, pword) );
extern	int	xform_uia	PARAMS( (PWord *, int *) );
extern	int	force_uia	PARAMS( (PWord *, int *) );
extern	void	string_to_list	PARAMS( (PWord *, int *, UCHAR *) );
extern	int	list_to_string	PARAMS( (UCHAR *, PWord, int) );
extern	int	getstring	PARAMS( (UCHAR **, PWord, int) );
extern	int	getlong		PARAMS( (long *, PWord, int) );
extern	int	get_gv_number	PARAMS( (UCHAR *) );
extern	void	set_prolog_error PARAMS( (PWord, int, PWord, int, PWord, PWord, int, PWord, int) );
extern	int	getdouble	PARAMS( (double *, PWord, int) );

/* wdisp.c */
extern	void	prolog_write	PARAMS( (PWord, int) );
extern	void	prolog_writeq	PARAMS( (PWord, int) );
extern	void	prolog_display	PARAMS( (PWord, int) );

/* built.c */
extern	void	builtin_init	PARAMS( (void) );
extern	void	time_cut_interrupt_init PARAMS( (void) );

/* bmisc.c */
#ifdef CMeta
extern	int	wm_identical	PARAMS(( PWord, int, PWord, int ));
#endif

/* arith.c */
extern	int	pbi_time	PARAMS(( void ));
extern	int	pbi_less	PARAMS(( void ));
extern	int	pbi_greater	PARAMS(( void ));
extern	int	pbi_equalorless	PARAMS(( void ));
extern	int	pbi_greaterorequal PARAMS(( void ));
extern	int	pbi_arithequal	PARAMS(( void ));
extern	int	pbi_arithnotequal PARAMS(( void ));
extern	int	pbi_is		PARAMS(( void ));
extern	int	pbi_srandom	PARAMS(( void ));

/* from bcinter.c */
#ifdef DOS
extern	int	pbi_c_make_farptr PARAMS(( void ));
#endif
extern	int	pbi_c_malloc	PARAMS(( void ));
extern	int	pbi_c_free	PARAMS(( void ));
extern	int	pbi_c_set	PARAMS(( void ));
extern	int	pbi_c_examine	PARAMS(( void ));

/* bdb.c */
extern	int	pbi_abolish	PARAMS(( void ));
extern	int	pbi_abolish_clausegroup	PARAMS(( void ));
extern	int	pbi_asserta	PARAMS(( void ));
extern	int	pbi_assertz	PARAMS(( void ));
extern	int	pbi_addclause	PARAMS(( void ));
extern	int	pbi_execcommand	PARAMS(( void ));
extern	int	pbi_erase	PARAMS(( void ));
extern	int	pbi_dynamic	PARAMS(( void ));
extern	int	pbi_icode	PARAMS(( void ));
extern	int	pbi_index_proc	PARAMS(( void ));
extern	int	pbi_massively_abolish_clausegroup PARAMS(( void ));
extern	int	pbi_nextproc	PARAMS(( void ));
extern	int	pbi_procinfo	PARAMS(( void ));
extern	int	pbi_clauseinfo	PARAMS(( void ));
extern	int	pbi_firstargkey	PARAMS(( void ));
extern	int	pbi_resolve_module	PARAMS(( void ));
extern	int	pbi_exported_proc	PARAMS(( void ));
extern	int	pbi_next_module	PARAMS(( void ));
extern	int	pbi_libbreak	PARAMS(( void ));
extern	int	pbi_listasm_clause	PARAMS(( void ));
extern	int	pbi_listasm_ntblentry	PARAMS(( void ));
extern	int	pbi_push_clausegroup	PARAMS(( void ));
extern	int	pbi_pop_clausegroup	PARAMS(( void ));
extern	int	pbi_collectcode	PARAMS(( void ));

	/* freeze.c */
extern	void	pbi_cptx	PARAMS(( void ));
extern	int		disp_heap	PARAMS(( void ));
extern	void	pbi_swp_tr	PARAMS(( void ));
extern	void	pbi_walk_cps	PARAMS(( void ));
extern	void	pbi_delay	PARAMS(( void ));
extern	void 	pbi_clct_tr PARAMS(( void ));
extern	void	pbi_collect_thawed	PARAMS(( void ));

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
extern	int	pbi_ttyflush	PARAMS(( void ));
extern	int	pbi_write	PARAMS(( void ));
extern	int	pbi_writeq	PARAMS(( void ));
extern	int	pbi_obp_open	PARAMS(( void ));
extern	int	pbi_obp_close	PARAMS(( void ));
extern	int	pbi_obp_load	PARAMS(( void ));
extern	int	pbi_obp_push_stop	PARAMS(( void ));
extern	int	pbi_obp_pop	PARAMS(( void ));
extern	int	pbi_old_consult	PARAMS(( void ));
extern	int	pbi_save_state_to_file	PARAMS(( void ));
#ifdef DynamicForeign
extern	int	pbi_load_foreign	PARAMS(( void ));
#endif /* DynamicForeign */

/* bmeta.c */
#ifdef	CMeta
extern	int	pbi_true	PARAMS(( void ));
extern	int	pbi_equal	PARAMS(( void ));
extern	int	pbi_arg		PARAMS(( void ));
extern	int	pbi_mangle	PARAMS(( void ));
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
extern	int	pbi_system	PARAMS(( void ));
extern	int	pbi_tmpnam	PARAMS(( void ));
extern	int	pbi_protect_bottom_stack_page	PARAMS(( void ));
extern	int	pbi_get_image_dir_and_name	PARAMS(( void ));

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
#ifdef SysVIPC
extern	int	sio_sysVq_open	PARAMS(( void ));
extern	int	pbi_ftok	PARAMS(( void ));
extern	int	pbi_msgctl	PARAMS(( void ));
#endif
#ifdef SSBQ
extern	int	sio_ssbq_open	PARAMS(( void ));
#endif
#ifdef HAVE_SOCKET
extern	int	sio_socket_open	PARAMS(( void ));
extern	int	sio_is_server_socket	PARAMS(( void ));
extern	int	sio_accept_socket_connection	PARAMS(( void ));
extern	int	sio_poll	PARAMS(( void ));
#endif /* HAVE_SOCKET */
extern	int	window_insert_pos	PARAMS(( void ));
extern	int	set_window_insert_pos	PARAMS(( void ));
extern	int	sio_window_open		PARAMS(( void ));
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
extern	int	sio_isgraphicatom	PARAMS(( void ));
extern	int	sio_readln	PARAMS(( void ));
extern	int	sio_position_in_line	PARAMS(( void ));

/* bsystem.c */
extern	int	pbi_ouch		PARAMS(( void ));
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
#if	defined(Portable) && defined(IProfile)
extern	int	pbi_init_iprofile	PARAMS(( void ));
extern	int	pbi_dump_iprofile	PARAMS(( void ));
#endif	/* defined(Portable) && defined(IProfile) */

/* gc.c */
extern	int	gc		PARAMS(( void ));

/* sig.c */
extern	int	pbi_alarm	PARAMS(( void ));

/* wam.c -- byte only */
#ifdef TRACEBWAM
extern	void 	toggle_bwam 	PARAMS( ( void ) );
#endif

