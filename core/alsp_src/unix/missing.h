/*=======================================================================*
 *		missing.h
 |	Copyright (c) 1989-95 Applied Logic Systems, Inc.
 |
 |		-- things which should be in system include files but which
 |		   sometimes aren't
 |
 | Author: Kevin A. Buettner with the help of man pages and include files
 | 	on systems which have these things defined.
 |
 | Our strategy here is to define MISSING_EXTERN_foo in mconfig.h if foo is
 | a C library function which ought to have been declared in a system
 | include file, but which for some inexplicable reason isn't.
 |
 | Why don't we just declare everything ourselves and forget all this
 | MISSING_ crap?  The reason is that if we do, we will often be in conflict
 | with the declarations on systems which _do_ declare the things that they
 | are supposed to.  We can make our declarations match one system, but they
 | may not match all others.  Therefore, it is only safe to declare those
 | things which are actually missing from a system include file.
 |
 | With the growing use of ANSI C, only really old systems should have to
 | make very many MISSING_ definitions in their mconfig.h file.  And
 | eventually (soon, hopefully) we will have a configuration mechanism which
 | will determine all of this stuff for us.
 *=======================================================================*/

#ifndef _MISSING_H_INCLUDED_
#define _MISSING_H_INCLUDED_ 1

/*
 * Some of the following functions are not explicitly called from anywhere
 * in the sources to alspro.  Include files on certain systems may call
 * some of the following (such as bzero or _filbuf) from a macro which
 * if left undeclared will give warnings from certain compilers.
 */

#if MISSING_EXTERN__FILBUF
extern	int	_filbuf		PARAMS(( FILE * ));
#endif

#if MISSING_EXTERN__FLSBUF
extern	int	_flsbuf		PARAMS(( int, FILE * ));
#endif

#ifdef MISSING_EXTERN_ACCESS
extern	int	access		PARAMS(( const char *, int ));
#endif

#ifdef MISSING_EXTERN_ATOI
extern	int	atoi		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_BRK
extern	int	brk		PARAMS(( void * ));
#endif

#ifdef MISSING_EXTERN_BCOPY
extern	void	bcopy		PARAMS(( const void *, void *, int ));
#endif

#ifdef MISSING_EXTERN_BZERO
extern	void	bzero		PARAMS(( char *, int ));
#endif

#ifdef MISSING_EXTERN_CHDIR
extern	int	chdir		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_CLOSE
extern	int	close		PARAMS(( int ));
#endif

#ifdef MISSING_EXTERN_DUP2
extern	int	dup2		PARAMS(( int, int ));
#endif

#ifdef MISSING_EXTERN_EXECL
extern	int	execl		PARAMS(( const char *, ... ));
#endif

#ifdef MISSING_EXTERN_FCLOSE
extern	int	fclose		PARAMS(( FILE * ));
#endif

#ifdef MISSING_EXTERN_FFLUSH
extern	int	fflush		PARAMS(( FILE * ));
#endif

#ifdef MISSING_EXTERN_SSCANF
extern	int	sscanf		PARAMS(( char *, const char *format, ... ));
#endif


#ifdef MISSING_EXTERN_FORK
extern	pid_t	fork		PARAMS(( void ));
#endif

#if MISSING_EXTERN_FPRINTF
extern	int	fprintf		PARAMS(( FILE *, const char *, ... ));
#endif

#ifdef MISSING_EXTERN_FPUTC
extern	int	fputc		PARAMS(( int, FILE * ));
#endif

#ifdef MISSING_EXTERN_FGETC
extern	int	fgetc		PARAMS(( FILE * ));
#endif

#ifdef MISSING_EXTERN_FREAD
extern	int	fread		PARAMS(( void *, size_t, int, FILE * ));
#endif

#ifdef MISSING_EXTERN_FREE
extern	void	free		PARAMS(( void * ));
#endif

#ifdef MISSING_EXTERN_FSEEK
extern	int	fseek		PARAMS(( FILE *, long, int ));
#endif

#ifdef MISSING_EXTERN_FWRITE
extern	int	fwrite		PARAMS(( const void *, size_t, size_t, FILE * ));
#endif

#ifdef MISSING_EXTERN_GETCWD
extern	char *	getcwd		PARAMS(( char *, size_t ));
#endif

#ifdef MISSING_EXTERN_GETENV
extern	char *	getenv		PARAMS(( char * ));
#endif

#ifdef MISSING_EXTERN_GETHOSTNAME
extern	int	gethostname 	PARAMS(( char *, int ));
#endif

#ifdef MISSING_EXTERN_GETPAGESIZE
extern	int	getpagesize	PARAMS(( void ));
#endif

#ifdef MISSING_EXTERN_GETPID
extern	pid_t	getpid		PARAMS(( void ));
#endif

#ifdef MISSING_EXTERN_GETWD
extern	char *	getwd		PARAMS(( char * ));
#endif

#ifdef MISSING_EXTERN_HTONS
extern	unsigned short htons	PARAMS(( unsigned short ));
#endif

#ifdef MISSING_EXTERN_LSEEK
extern	off_t	lseek		PARAMS(( int, off_t, int ));
#endif

#ifdef MISSING_EXTERN_OPEN
extern	int	open		PARAMS(( const char *, int, ... ));
#endif

#ifdef MISSING_EXTERN_MALLOC
extern	void *	malloc		PARAMS(( size_t ));
#endif

#ifdef MISSING_EXTERN_MEMSET
extern	void *	memset		PARAMS(( void *, int c, size_t n ));
#endif

#ifdef MISSING_EXTERN_MUNMAP
extern	int	munmap		PARAMS(( caddr_t addr, int len ));
#endif

#ifdef MISSING_EXTERN_MKDIR
extern	int	mkdir		PARAMS(( const char *, mode_t ));
#endif

#ifdef MISSING_EXTERN_PERROR
extern	void	perror		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_PIPE
extern	int	pipe		PARAMS(( int [2] ));
#endif

#ifdef MISSING_EXTERN_PRINTF
extern	int	printf		PARAMS(( const char *, ... ));
#endif

#ifdef MISSING_EXTERN_READLINK
extern	int	readlink	PARAMS(( const char *, void *, int ));
#endif

#ifdef MISSING_EXTERN_REALLOC
extern	void *	realloc		PARAMS(( void *, size_t ));
#endif

#ifdef MISSING_EXTERN_REGCMP
extern	char *	regcmp		PARAMS(( const char *, ... ));
#endif

#ifdef MISSING_EXTERN_REGEX
extern	char *	regex		PARAMS(( const char *, const char *, ... ));
#endif

#ifdef MISSING_EXTERN_RE_COMP
extern	char *	re_comp		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_RE_EXEC
extern int	re_exec		PARAMS(( char * ));
#endif

#ifdef MISSING_EXTERN_READ
extern	int	read		PARAMS(( int, void *, unsigned int ));
#endif

#ifdef MISSING_EXTERN_REWIND
extern	int	rewind		PARAMS(( FILE * ));
#endif

#ifdef MISSING_EXTERN_REXEC
extern	int	rexec		PARAMS(( char **, int, char *, char *, char *, int * ));
#endif

#ifdef MISSING_EXTERN_RMDIR
extern	int	rmdir		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_SBRK
extern	void *	sbrk		PARAMS(( int ));
#endif

#ifdef MISSING_EXTERN_SELECT
extern	int	select		PARAMS(( int, fd_set *, fd_set *, fd_set *, struct timeval * ));
#endif

#ifdef MISSING_EXTERN_SETITIMER
extern	int	setitimer	PARAMS(( int, struct itimerval *, struct itimerval * ));
#endif

#ifdef MISSING_EXTERN_SETPGRP
#ifdef HAVE_BSD_SETPGRP
extern	int	setpgrp		PARAMS(( int, int ));
#else
extern	int	setpgrp		PARAMS(( void ));
#endif
#endif

#ifdef MISSING_EXTERN_SETVBUF
#ifdef SETVBUF_REVERSED
extern	int	setvbuf 	PARAMS(( FILE *, int, char *, size_t ));
#else
extern	int	setvbuf		PARAMS(( FILE *, char *, int, size_t ));
#endif
#endif

#ifdef MISSING_EXTERN_SIGVEC
extern	int	sigvec		PARAMS(( int, struct sigvec *, struct sigvec * ));
#endif

#ifdef MISSING_EXTERN_SIGSTACK
extern	int	sigstack	PARAMS(( struct sigstack *, struct sigstack * ));
#endif

#ifdef MISSING_EXTERN_STAT
extern	int	stat		PARAMS(( const char *, struct stat * ));
#endif

#ifdef MISSING_EXTERN_LSTAT
extern	int	lstat		PARAMS(( const char *, struct stat * ));
#endif

#ifdef MISSING_EXTERN_SYMLINK
extern	int	symlink		PARAMS(( const char *, const char * ));
#endif

#ifdef MISSING_EXTERN_SYSTEM
extern	int	system		PARAMS(( const char * ));
#endif

#if MISSING_EXTERN_TIME
extern	long	time		PARAMS(( long * ));
#endif

#ifdef MISSING_EXTERN_TIMES
extern	int	times		PARAMS(( struct tms * ));
#endif

#ifdef MISSING_EXTERN_UNLINK
extern	int	unlink		PARAMS(( const char * ));
#endif

#ifdef MISSING_EXTERN_VFPRINTF
extern	int	vfprintf	PARAMS(( FILE *, const char *, va_list ));
#endif

#ifdef MISSING_EXTERN_VSPRINTF
extern	int	vsprintf	PARAMS(( char *, const char *, va_list ));
#endif

#ifdef MISSING_EXTERN_WAIT
extern	pid_t	wait		PARAMS(( int * ));
#endif

#ifdef MISSING_EXTERN_WRITE
extern	int	write		PARAMS(( int, const void *, unsigned int ));
#endif

#ifdef MISSING_EXTERN_CRYPT
extern char    *crypt           PARAMS((char *, char *));
#endif

#ifdef MISSING_EXTERN_STRTOUL
extern unsigned long int strtoul(const char *, char **, int base);
#endif

#ifdef MISSING_EXTERN_REALPATH
extern char *realpath(const char *path, char resolved_path[]);
#endif

#ifdef MISSING_EXTERN_GETRUSAGE
extern int getrusage(int, struct rusage *);
#endif

#ifdef MISSING_EXTERN_GETIMEOFDAY
extern int gettimeofday(struct timeval *tp, void *tzp);
#endif

#ifdef MISSING_EXTERN_H_ERRNO
extern int h_errno;
#endif

#ifdef MISSING_EXTERN_TEMPNAM
extern char *tempnam(const char *dir, const char *pfx);
#endif

#ifdef MISSING_EXTERN_SETSOCKOPT
extern int setsockopt(int s, int level, int optname, char *optval, int optlen);
#endif

#ifdef MISSING_EXTERN_STRERROR
char *strerror(int errnum);
#endif

#ifdef MISSING_EXTERN_MPROTECT
int mprotect(void *addr, size_t len, int prot);
#endif

#endif /* _MISSING_H_INCLUDED_ */
