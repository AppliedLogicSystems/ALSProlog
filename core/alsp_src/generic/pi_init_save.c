/*=====================================================================*
 |		pi_init.c
 | Copyright (c) 1988-1994, Applied Logic Systems, Inc.
 |
 |	a segment of the old pimain.c
 *=====================================================================*/


#ifdef EXP_DATE
    if ((unsigned long) time(0) >= EXP_DATE) {
	PI_app_printf(PI_app_printf_error, "System validity date passed!\n");
	exit(1);
	}
#endif

    if ((exit_status = PI_prolog_init(WIN_STR, argc, argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(1);
    }

#if MOTIFWINS || OLWINS || DECWINS || XWINS
    x_init();
    xaux_init();
#endif

#if MOTIFWINS || OLWINS || DECWINS
    xtaux_init();
#endif

#ifdef OLWINS
    ol_init();
    olaux_init();
#endif

#ifdef MOTIFWINS
    motif_init();
    motifaux_init();
#endif

#ifdef DECWINS
    decw_init();
    decwaux_init();
#endif

#ifdef NEXTWINS
    next_init();
    nextaux_init();
#endif

#ifdef PROLOGDBI
    prologdbi_init();
#endif /* PROLOGDBI */
