/*=============================================================*
 | security.h
 | Copyright (c) 1996 Applied Logic Systems
 |
 | Hardware/software copy-protection security for timed-demos
 | and hardware-key protected versions of ALS Prolog.
 *=============================================================*/

extern void	init_security(void);
extern void	shutdown_security(void);
extern int pbi_enable_security(void);
extern void enable_security(void);
extern void	check_security(void);
