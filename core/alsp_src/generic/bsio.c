/*=======================================================================*
 |			bsio.c
 |		Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |			-- stream I/O primitives
 |
 | Author: Kevin A. Buettner
 | Creation: 10/8/91
 | Revision History:
 |      Some additions by Ken Bowen
 |      Socket additions by Kevin Jiang (February-April, 1993)
 |		Socket rewrite by Kevin Buettner (Dec, 1993)
 |		Buffer management rewrite by Kevin Buettner (Dec, 1993)
 | 10/26/94 - C. Houpt -- Put inclusion of <fcntl.h> under the control of
 |			  HAVE_FCNTL_H. Put sio_poll() under the control of HAVE_SOCKET. 
 |			  Added char* casts for various calls.
 |			  -- Put in corrected version of open() and read() to fix
 |				 various bugs in Unix emulation libraries.
 | ??/??/94,	C. Houpt -- NEED TO ADD SOCKET SUPPORT FOR MAC!
 |
 | Configuration parameters (defines, in aconfig.h or mconfig.h):
 |
 |  SysVIPC             -- Unix System V IPC message queues
 |  SSBQ                -- Software Services Backplane Queues
 |  HAVE_SOCKET         -- sockets (should include DOS/FTP & Mac)
 |    Subsidiary params for sockets:
 |      UNIX  -- various flavors of unix; if your brand of unix doesn't
 |		 		 support sockets, then don't define HAVE_SOCKET.
 |      DOS   -- stuff for PC-FTP TCP/IP sockets
 |	    MacOS -- sockets on the mac
 *=======================================================================*/
#include "defs.h"

#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if !defined(MPW_TOOL) && defined(__MWERKS__)
#include <unix.h>
/* MetroWerks does not define the EINTR error code. */
#define EINTR           4
#endif

#ifdef DOS
#include <types.h>
#include <stat.h>
#include <io.h>
#define open    _open
#define close   _close
#define lseek   _lseek
#define read    _read
#define write   _write
#endif /* DOS */

#ifdef SysVIPC		/* For System V Message Queues: */
#include <sys/ipc.h>
#include <sys/msg.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

/* These should be in sys/ipc.h or sys/msg.h, but some systems have really
 * lame include files.
 */
extern	key_t	ftok		PARAMS(( CONST char *, int ));
extern	int	msgget		PARAMS(( key_t, int ));
extern	int	msgsnd		PARAMS(( int, CONST void *, size_t, int ));
extern	int	msgrcv		PARAMS(( int, void *, size_t, long, int ));
extern	int	msgctl		PARAMS(( int, int, ... ));

#endif /* SysVIPC */

#ifdef SSBQ		/* For Software Services Backplane Queues: */
/* #include "ipc_interface.h"  */
#endif /* SSBQ */

#ifdef HAVE_SOCKET		/* For sockets: */

#ifdef UNIX
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/param.h>
#ifndef SCO_UNIX
#include <sys/un.h>
#endif /* SCO_UNIX */
#endif /* UNIX */

#ifdef DOS			/* use PC/TCP Developement Kit for DOS */
#include <sys/types.h>		/* C compiler header file */
#include <netinet/in.h>		/* PC/TCP header files with */
#include <netdb.h>		/* same names as BSDUNIX ones */
#include <pctcp/types.h>
#include <pctcp/pctcp.h>
#include <pctcp/sockets.h>
#include <pctcp/options.h>
#include <pctcp/error.h>
#endif /* DOS */

#ifdef MacOS

#include <GUSI.h>

#endif /* MacOS */

#endif /* HAVE_SOCKET */

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef SEEK_SET
#define SEEK_SET 0		/* for lseek call */
#define SEEK_CUR 1
#define SEEK_END 2
#endif /* SEEK_SET */

#ifndef STDIN_FILENO		/* these are defined in unistd.h */
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif /* STDIN_FILENO */

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
	/* this should be in <netdb.h>, but sometimes isn't */
#endif	/* MAXHOSTNAMELEN */

/* Declare socket related functions */
#if defined(HAVE_SOCKET)
#ifdef MISSING_EXTERN_SOCKET
extern	int	socket		PARAMS(( int, int, int ));
#endif
#ifdef MISSING_EXTERN_BIND
extern	int	bind		PARAMS(( int, struct sockaddr *, int ));
#endif
#ifdef MISSING_EXTERN_CONNECT
extern	int	connect		PARAMS(( int, struct sockaddr *, int ));
#endif
#ifdef MISSING_EXTERN_LISTEN
extern	int	listen		PARAMS(( int, int ));
#endif
#ifdef MISSING_EXTERN_GETHOSTNAME
extern	int	gethostname	PARAMS(( char *, int ));
#endif
#ifdef MISSING_EXTERN_GETSOCKNAME
extern	int	getsockname	PARAMS(( int, struct sockaddr *, int * ));
#endif
#ifdef MISSING_EXTERN_SELECT
extern	int	select		PARAMS(( int, fd_set *, fd_set *, fd_set *, struct timeval * ));
#endif
#ifdef MISSING_EXTERN_ACCEPT
extern	int	accept		PARAMS(( int, struct sockaddr *, int * ));
#endif
#ifdef MISSING_EXTERN_REXEC
extern	int	rexec		PARAMS(( char **, int, char *, char *, char *, int * ));
#endif
#ifdef MISSING_EXTERN_SENDTO
extern	int	sendto		PARAMS(( int, void *, int, int, struct sockaddr *, int ));
#endif
#ifdef MISSING_EXTERN_RECVFROM
extern	int	recvfrom	PARAMS(( int, void *, int, int, struct sockaddr *, int * ));
#endif
#ifdef MISSING_EXTERN_SHUTDOWN
extern	int	shutdown	PARAMS(( int, int) );
#endif
#endif /* HAVE_SOCKET */

#include "bsio.h"
#include "siolex.h"

static	UCHAR *	get_stream_buffer PARAMS(( PWord, int ));
static	void	incr_fdrefcnt	PARAMS(( int ));
static	int	decr_fdrefcnt	PARAMS(( int ));
static	int	compute_flags	PARAMS(( char *, int , int ));
static	void	delete_stream_name PARAMS(( PWord ));
static	int	accept_connection PARAMS(( PWord, char * ));
static	int	stream_is_ready	PARAMS(( char *, long ));
static	void	shift_buffer	PARAMS(( UCHAR * ));
static	int	write_buf	PARAMS(( PWord, UCHAR * ));
static	long	stream_end	PARAMS(( UCHAR * ));
static	int	skip_layout	PARAMS(( UCHAR * ));
static	int	octal		PARAMS(( UCHAR ** ));
static	int	hexadecimal	PARAMS(( UCHAR ** ));
static	double	decimal		PARAMS(( UCHAR **, int *));
static	int	escaped_char	PARAMS(( UCHAR ** ));
static	void	quoted_atom	PARAMS(( PWord *, PWord *, int *, UCHAR **, UCHAR *lim, UCHAR *buf));
static	int	quoted_string	PARAMS(( PWord *, PWord *, int *, UCHAR **, UCHAR *, UCHAR * ));
static	int	char_constant	PARAMS(( UCHAR **, UCHAR *, int, int ));
static	int	next_token0	PARAMS(( UCHAR *, PWord *, int *, PWord *, int * ));
static	int	format_type	PARAMS(( UCHAR * ));
static  int int_or_float    PARAMS((int, PWord));

/*
 * sio_mkstream(BufSize,Stream)
 *
 * sio_mkstream is called from prolog with a variable (hopefully not an already
 * constructed term).   A stream descriptor and buffer are allocated,
 * initialized, and returned.  Only the buffer slot of the stream descriptor
 * is filled in.  The other slots are initialized with zeros and must be
 * filled in by the Prolog code at the time of the open.
 */

int
sio_mkstream()
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    UCHAR *buf;
    int   i;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || v1 < 8 || v1 > wm_normal - 256)
	FAIL;

    w_uia_alloc(&v4, &t4, (size_t) (size_t)SIO_SDSIZE(v1));

    buf = (UCHAR *) M_FIRSTUIAWORD(v4);

    SIO_CPOS(buf) = 0;
    SIO_LPOS(buf) = 0;
    SIO_BUFPOS(buf) = 0;
    SIO_LINENUM(buf) = 1;
    SIO_ERRNO(buf) = 0;
    SIO_FLAGS(buf) = 0;
    SIO_FD(buf) = 0;
    SIO_COMMENT(buf) = 0;
    SIO_ELINENUM(buf) = 0;
    SIO_AUX(buf) = 0;
    SIO_AUX2(buf) = 0;
    SIO_BFSIZE(buf) = v1;
    SIO_COLUMN(buf) = 0;

    w_mk_term(&v3, &t3, SIO_SD_FUNCTOR, SIO_SD_ARITY);
    w_install_argn(v3, 1, v4, t4);
    for (i = 2; i <= SIO_SD_ARITY; i++)
	w_install_argn(v3, i, 0, WTP_INTEGER);

    if (w_unify(v2, t2, v3, t3))
	SUCCEED;
    else
	FAIL;
}


/*
 * get_stream_buffer is called with a value and type argument representing
 * a stream descriptor (not just a stream buffer).
 *
 * The buffer is returned as a result of this function if successful;
 * if unsuccessful, 0 is returned.
 */

static UCHAR *
get_stream_buffer(v, t)
    PWord v;
    int   t;
{
    PWord functor;
    int   arity;

    if (t != WTP_STRUCTURE)
	return 0;

    w_get_functor(&functor, v);
    w_get_arity(&arity, v);

    if (functor != SIO_SD_FUNCTOR || arity != SIO_SD_ARITY)
	return 0;

    w_get_argn(&v, &t, v, 1);

    if (t != WTP_UIA)
	return 0;

    return (UCHAR *) M_FIRSTUIAWORD(v);
}


/*
 * sio_errcode(Stream,ErrCode)
 *
 * sio_errcode is called from Prolog to retrieve the SIO_ERRCODE value from
 * the buffer associated with a stream descriptor.  The value is unified with
 * ErrCode
 */


int
sio_errcode()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, (PWord) SIO_ERRCODE(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

/*
 * sio_set_errcode(Stream,ErrCode)
 *
 *      sio_set_errcode is called from Prolog to set the SIO_ERRCODE value in
 *      the buffer associated with a stream descriptor to ErrCode; used by
 *      stream routines such as windows streams.
 */


int
sio_set_errcode()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (t2 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    SIO_ERRCODE(buf) = (int) v2;

    SUCCEED;
}


/*
 * sio_buf_params(Stream,BufStart,BufSize)
 *
 * sio_buf_params is called from Prolog to retrieve location information
 * about the buffer associated with a stream descriptor; installed for
 * use by the buffer read/write routines for atom/string type streams.
 * BufStart = byte offset from beginning of the buf associated with
 * Stream of the actual buffer;
 */

int
sio_buf_params()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   bufstart;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    bufstart = SIO_BUFFER(buf)-buf;

    if (w_unify(v2, t2, (PWord) bufstart, WTP_INTEGER) &&
	w_unify(v3, t3, (PWord) SIO_BFSIZE(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;

}


/*
 * sio_errno(Stream,ErrNo)
 *
 * sio_errno is called from Prolog to retrieve the SIO_ERRNO value from
 * the buffer associated with a stream descriptor.
 */


int
sio_errno()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, (PWord) SIO_ERRNO(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;

}


/*
 * sio_aux(Stream,Aux)
 *
 * sio_aux is called from Prolog to retrieve the SIO_AUX value from
 * the buffer associated with a stream descriptor.
 */


int
sio_aux()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, (PWord) SIO_AUX(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;

}

/*
 * sio_fd(Stream,Aux)
 *
 * sio_fd is called from Prolog to retrieve the SIO_FD value from
 * the buffer associated with a stream descriptor.
 */

int
sio_fd()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;
	PWord rval; int rtype;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

/* printf("sio_fd: SIO_FD(buf)=%d\n",SIO_FD(buf)); */

/*
    if (w_unify(v2, t2, (PWord) SIO_FD(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
*/

	PI_makedouble(&rval,&rtype,(double) SIO_FD(buf));
	if (PI_unify(v2,t2,rval,rtype))
		PI_SUCCEED;
		PI_FAIL;


}


/*
 * sio_increment_bufpos(Stream)
 *
 * sio_increment_bufpos is called from Prolog to increment SIO_BUFPOS(buf)
 * by SIO_LPOS(buf)
 */

int
sio_increment_bufpos()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    SIO_BUFPOS(buf) += SIO_LPOS(buf);

    SUCCEED;

}

/*
 * sio_set_position(Stream,CPOS,LPOS)
 *
 * sio_set_position is called from Prolog to set the SIO_CPOS(buf)
 * and SIO_LPOS(buf) values of the buf associated with Stream
 */

int
sio_set_position()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (t2 != WTP_INTEGER || t3 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    SIO_CPOS(buf) = v2;
    SIO_LPOS(buf) = v3;

    SUCCEED;

}

/*
 * sio_set_eof(Stream)
 *
 * sio_set_eof is called from Prolog to set the end-of-file indicators.
 */

int
sio_set_eof()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    SIO_ERRCODE(buf) = SIOE_EOF;
    SIO_FLAGS(buf) |= SIOF_EOF;

    SUCCEED;

}


/*
 * sio_reset_eof(Stream)
 *
 * sio_reset_eof is called from Prolog to increment SIO_BUFPOS(buf)
 * by SIO_LPOS(buf)
 */

int
sio_reset_eof()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SIO_FLAGS(buf) &= ~SIOF_EOF;

    SUCCEED;

}


/*
 * sio_cpos(Stream, CPos)
 *
 * siof_cpos is called from Prolog to retrieve the current CPos
 */

int
sio_cpos()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, (PWord) SIO_CPOS(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


/*
 * sio_lpos(Stream, CPos)
 *
 * sio_lpos is called from Prolog to retrieve the current LPos
 */

int
sio_lpos()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, (PWord) SIO_LPOS(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


/*
 * incr_fdrefcnt, decr_fdrefcnt
 *
 * Multiple prolog streams may reference the same file descriptor.  This is
 * useful in situations where the file descriptor is full duplex (such as
 * with sockets) or in situations where it is desirable to have multiple
 * buffers associated with the same descriptor (terminals or terminal-like
 * streams).
 *
 * When a stream opens a descriptor, it should increment the reference count.
 * When a stream closes a descriptor, it should decrement the reference count.
 * Should the reference count reach zero, the stream may be closed down. We
 * do not explicitly put the call to close in decr_fdrefcnt because there may
 * be different ways to close different types of descriptors for different
 * platforms. Even on unix, for example, it may be desirable to call shutdown
 * rather than close for certain types of sockets.  In addition, there may be
 * other actions to take in order to properly shut down the stream.  Unix
 * domain sockets, for example, need to have the path associated with the
 * socket unlinked.
 *
 * incr_fdrefcnt will allocate and initialize the reference count array if
 * necessary and will then increment the reference count associated with
 * the descriptor fd.
 *
 * decr_fdrefcnt will decrement the reference count associated with fd and
 * return 1 if that file descriptor should be actually closed. 0 will be
 * returned if it should be left open.
 */

static unsigned short *fdrefcnts = NULL;
static int openmax = 0;
#define OPEN_MAX_GUESS 256

static void
incr_fdrefcnt(fd)
    int fd;
{
    if (fdrefcnts == NULL) {
#if defined(_SC_OPEN_MAX)
	if ( (openmax = sysconf(_SC_OPEN_MAX)) < 0)
	    openmax = OPEN_MAX_GUESS;
#elif defined(OPEN_MAX)
	openmax = OPEN_MAX;
#elif defined(FOPEN_MAX)
	openmax = FOPEN_MAX;
#else
	openmax = OPEN_MAX_GUESS;
#endif /* OPEN_MAX */

	if ( (fdrefcnts = (unsigned short *) 
			  malloc(openmax * sizeof *fdrefcnts)) == NULL)
	    fatal_error(FE_FDREFCNTS, 0);
	memset(fdrefcnts, 0, openmax * sizeof *fdrefcnts);
    }

    fdrefcnts[fd]++;
}

static int
decr_fdrefcnt(fd)
    int fd;
{
    if (--fdrefcnts[fd] <= 0)
	return 1;
    else
	return 0;
}

/*
 * compute_flags examines the file modes and buffering value and sets the
 * SIO_FLAGS field appropriately.  It will return 0 if everything is
 * hunky-dory, and -1 if there is an invalid argument.  SIO_ERRCODE will
 * be set set to SIOE_INARG if there is an invalid argument.
 */

static int
compute_flags(buf, m, b)
    char *buf;
    int m;
    int b;
{
    long flags;

    flags = (b & SIOB_DONTBLOCK) ? SIOF_DONTBLOCK : 0;

    switch (b & ~SIOB_DONTBLOCK) {
	case SIOB_BYTE :
	    flags |= SIOF_BBYTE;
	    break;
	case SIOB_LINE :
	    flags |= SIOF_BLINE;
	    break;
	case SIOB_BLOCK :
	    break;
	default :
	    SIO_ERRCODE(buf) = SIOE_INARG;
	    return -1;
    }

    switch (m) {
	case SIOM_READ :
	    flags |= SIOF_READ;
	    break;
	case SIOM_READ_WRITE :
	    flags |= SIOF_READ | SIOF_WRITE;
	    break;
	case SIOM_WRITE :
	case SIOM_APPEND :
	    flags |= SIOF_WRITE;
	    break;
	default :
	    SIO_ERRCODE(buf) = SIOE_INARG;
	    return -1;
    }

    SIO_FLAGS(buf) = flags;
    return 0;
}

/*
 * sio_file_open(FileName,SD,Mode,Buffering)
 *
 * sio_file_open is called from Prolog to open a file or other device available
 * through the file system.
 */


int
sio_file_open()
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    UCHAR *filename;
    UCHAR *buf;
    int   flags = 0;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (!getstring(&filename, v1, t1) || t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }
    SIO_TYPE(buf) = SIO_TYPE_FILE;

    switch (v3) {
	case SIOM_READ:
	    flags = O_RDONLY;
	    break;
	case SIOM_WRITE:
	    flags = O_WRONLY | O_CREAT | O_TRUNC;
	    break;
	case SIOM_APPEND:
	    flags = O_WRONLY | O_APPEND | O_CREAT;
	    break;
	case SIOM_READ_WRITE:
	    flags = O_RDWR | O_CREAT;
	    break;
    }

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;

    if (strcmp((char *)filename, "$stdin") == 0)
	SIO_FD(buf) = STDIN_FILENO;
    else if (strcmp((char *)filename, "$stdout") == 0)
	SIO_FD(buf) = STDOUT_FILENO;
    else if (strcmp((char *)filename, "$stderr") == 0)
	SIO_FD(buf) = STDERR_FILENO;
    else {
#if	defined(DOS)
	if ((SIO_FD(buf) = open(filename, flags, (S_IWRITE | S_IREAD))) == -1) {
	    /* } */
#elif	defined(MacOS)
#ifdef THINK_C
	    /* Open files as text files to insure CR/NL conversion. */
	    if ((SIO_FD(buf) = open((char *)filename, flags | O_TEXT)) == -1) {	/* } */
#elif defined(__MWERKS__) && !__POWERPC__
	    extern int metrowerks_open_patch(const char *filename, int mode);
	    if ((SIO_FD(buf) = metrowerks_open_patch((char *)filename, flags)) == -1) {/* } */
#else
	    if ((SIO_FD(buf) = open((char *)filename, flags)) == -1) {	/* } */
#endif
#elif defined(__GO32__) || defined(OS2)
	if ((SIO_FD(buf) = open(filename, flags|O_BINARY, 0777)) == -1) {
#else  /* default code */
	if ((SIO_FD(buf) = open(filename, flags, 0777)) == -1) {
#endif
	    if (errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = errno;
	    }
	    FAIL;
	}
    }

    incr_fdrefcnt(SIO_FD(buf));
    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}


#ifdef SysVIPC

/*
 * ftok(Path,ID)
 *
 * calls the library(3) function ftok(path,id) to generate a key
 */

int
pbi_ftok()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *path;
    UCHAR *idname;
    UCHAR  id;
    key_t FTok;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (!getstring(&path, v1, t1) ||
	!getstring(&idname, v2, t2)) {
	FAIL;
    }

    id = idname[0];

    FTok = ftok(path, id);

    if (w_unify(v3, t3, (PWord) FTok, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


/*
 * sio_sysVq_open(IPC_Key,Stream,NMode,NBuffering,PermsFlag,MType,MWait),
 *
 * sio_sysVq_open is called from Prolog to open a System V IPC
 * message queue.
 *
 *      ASSIGNMENTS:
 *              SIO_FD          --      message queue id
 *              SIO_AUX2        --      message type
 *              SIO_AUX3        --      message flag
 */

int
sio_sysVq_open()
{
    PWord v1, v2, v3, v4, v5, v6, v7;
    int   t1, t2, t3, t4, t5, t6, t7;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);
    w_get_An(&v7, &t7, 7);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;


    if (t1 != WTP_INTEGER || t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER || t5 != WTP_INTEGER ||
	t6 != WTP_INTEGER || t7 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    SIO_TYPE(buf) = SIO_TYPE_SYSVQ;

    if (compute_flags(buf,v3,v4) < 0)
	FAIL;

    if ((SIO_FD(buf) = msgget((key_t) v1, ((int) v5 | (int) IPC_CREAT))) == -1) {
	if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	FAIL;
    }

    /* install the message type: */
    SIO_AUX2(buf) = (int) v6;
    SIO_HEADER(buf, 8) = (long) v6;

    /* install message wait flag */
    SIO_AUX3(buf) = (int) v7;

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}

#endif /* SysVIPC */

#ifdef SSBQ
/*
 * sio_ssbq_initialize()
 *
 * sio_ssbq_initialize is called from Prolog to initialize
 * the SSB-message-queue system.
 */

int
sio_ssbq_initialize()
{

    if (ipc_init(30) == -1) {
	FAIL;
    }
    else
	SUCCEED;
}



/*
 * sio_ssbq_open(Q_Name,Stream,NMode,NBuffering,TargetNode,MessWait)
 *
 * sio_ssbq_open is called from Prolog to open an SSB-message-queue-based
 * stream.
 */

int
sio_ssbq_open()
{
    PWord v1, v2, v3, v4, v5, v6;
    int   t1, t2, t3, t4, t5, t6;
    UCHAR *queuename, *targetnode;
    UCHAR *buf;
    int   qc;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (!getstring(&queuename, v1, t1) || t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER || t6 != WTP_INTEGER ||
	!getstring(&targetnode, v5, t5)) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    SIO_TYPE(buf) = SIO_TYPE_SSBQ;

    if (compute_flags(buf,v3,v4) < 0)
	FAIL;

    if (v3 == SIOM_READ) {	/* read mode: create the queue */
	if ((qc = create_queue(queuename)) != 0) {
	    if (errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = errno;
	    }
	    FAIL;
	}
    }

    /* queuename acts as the file descriptor: */
    SIO_FD(buf) = (int) strcpy(malloc(strlen(queuename) + 1), queuename);

    /* install the target node */
    SIO_SSBQ_TARGETNODE(buf) =
	(int) strcpy((UCHAR *) malloc(strlen(targetnode) + 1), targetnode);

    /* install message wait flag */
    SIO_SSBQ_WAIT(buf) = (int) v6;
    (UCHAR *) SIO_SSBQ_INCB(buf) = (UCHAR *) 0;
    SIO_SSBQ_USEDCS(buf) = 0;
    SIO_SSBQ_REMCS(buf) = 0;
    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}

#endif /* SSBQ */



#ifdef HAVE_SOCKET

/*
 * sio_socket_open(HostOrPath,Port,Dom,Typ,NMode,NBuffering,QLen,SD,Stream)
 *
 * sio_socket_open is called from Prolog to open a socket-based stream
 *
 * The socket descriptor is stored in SIO_FD(buf).
 *
 * For datagram streams the address in which the return address is stored
 * is in an additional malloc'd structure whose pointer is available via
 * SIO_SOCKET_ADDRESS.
 */

int
sio_socket_open()
{
    PWord v1, v2, v3, v4, v5, v6, v7, v8, v9;
    int   t1, t2, t3, t4, t5, t6, t7, t8, t9;
    UCHAR *buf, *host_or_path, *clone_buf;
    int   portnum, domain, socktype;
    char myhostname[MAXHOSTNAMELEN];
    int status = 0;
    int isserver = 0;

#ifdef UNIX
    struct sockaddr_in sockname_in;	/* Internet socket addresses */
    struct hostent *hp;

#ifdef AF_UNIX
    struct sockaddr_un sockname_un;	/* Unix socket addresses */
#endif /* AF_UNIX */
#endif /* UNIX */

#ifdef DOS
    struct addr sockname;
#endif /* DOS */

#ifdef MacOS
    struct sockaddr_in sockname_in; /* Internet socket addresses */
    struct hostent *hp;
#endif /* MacOS */

    w_get_An(&v1, &t1, 1);	/* Host name or path name (may be var) */
    w_get_An(&v2, &t2, 2);	/* Port to connect to (may be var) */
    w_get_An(&v3, &t3, 3);	/* Domain / protocol number */
    w_get_An(&v4, &t4, 4);	/* Socket type */
    w_get_An(&v5, &t5, 5);	/* Stream mode number (read or write) */
    w_get_An(&v6, &t6, 6);	/* Buffering number */
    w_get_An(&v7, &t7, 7);	/* Queue length for listen */
    w_get_An(&v8, &t8, 8);	/* Socket Descriptor to clone */
    w_get_An(&v9, &t9, 9);	/* Stream Descriptor */

    if ((buf = get_stream_buffer(v9, t9)) == (UCHAR *) 0)
	FAIL;
    
    SIO_ERRCODE(buf) = SIOE_INARG;

    if (!getstring(&host_or_path, v1, t1))
	if (t1 == WTP_UNBOUND)
	    host_or_path = NULL;
	else
	    FAIL;
    
    if (t2 == WTP_UNBOUND)
	portnum = 0;
    else if (t2 == WTP_INTEGER)
	portnum = v2;
    else
	FAIL;
    
    if (t3 == WTP_INTEGER && (
#ifdef AF_UNIX
    				v3 == AF_UNIX ||
#endif
				v3 == AF_INET ))
	domain = v3;
    else
	FAIL;

    if (t4 == WTP_INTEGER) {
	switch (v4) {
	    case ALS_STREAM :
		socktype = SOCK_STREAM;
		break;
	    case ALS_DGRAM :
		socktype = SOCK_DGRAM;
		break;
	    default :
		FAIL;
	}
    }
    else
	FAIL;
    

    if (t5 != WTP_INTEGER || t6 != WTP_INTEGER || t7 != WTP_INTEGER)
	FAIL;

    if (compute_flags(buf,v5,v6) < 0)
	FAIL;

    SIO_SOCKET_ADDRESS(buf) = 0;
    SIO_SOCKET_ADDRESS_LEN(buf) = 0;
    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if ( (clone_buf = get_stream_buffer(v8, t8)) ) {

	/* clone the file descriptor and increment reference count */
	SIO_FD(buf) = SIO_FD(clone_buf);
	incr_fdrefcnt(SIO_FD(buf));

	/* clone certain of the flags... */
	SIO_FLAGS(buf) &= ~SIOF_CLONE;
	SIO_FLAGS(buf) |= (SIO_FLAGS(clone_buf) & SIOF_CLONE);

	/* clone the type field */
	SIO_TYPE(buf) = SIO_TYPE(clone_buf);

	/* clone the address buffer */
	SIO_SOCKET_ADDRESS(buf) = SIO_SOCKET_ADDRESS(clone_buf);
	SIO_SOCKET_ADDRESS_LEN(buf) = SIO_SOCKET_ADDRESS_LEN(clone_buf);

	/* all done with cloning */
	SUCCEED;
    }

#ifdef UNIX

    if ((SIO_FD(buf) = socket(domain, socktype, 0)) == -1) {
	SIO_ERRCODE(buf) = SIOE_SYSCALL;
	SIO_ERRNO(buf) = errno;
	FAIL;
    }

    switch (domain) {
#ifdef AF_UNIX
	case AF_UNIX :
	    sockname_un.sun_family = AF_UNIX;
	    strcpy(sockname_un.sun_path, (UCHAR *) host_or_path);

	    if (bind(SIO_FD(buf), (struct sockaddr *) &sockname_un,
		     (int) (sizeof (sockname_un.sun_family)
		          + strlen(sockname_un.sun_path))) == 0) {
		isserver = 1;
	    }
	    else {
		status = connect(SIO_FD(buf),
				 (struct sockaddr *) &sockname_un,
				 (int) (sizeof (sockname_un.sun_family)
				      + strlen(sockname_un.sun_path)));
	    }
	    break;
#endif /* AF_UNIX */
	case AF_INET :
	    gethostname(myhostname, MAXHOSTNAMELEN);
	    memset(&sockname_in, 0, sizeof sockname_in);
	    sockname_in.sin_family = AF_INET;
	    sockname_in.sin_addr.s_addr = INADDR_ANY;
	    sockname_in.sin_port = htons((u_short)portnum);

	    if ( (host_or_path == NULL || strcmp(myhostname,host_or_path) == 0)
		 && bind(SIO_FD(buf), (struct sockaddr *) &sockname_in,
		         sizeof (struct sockaddr_in)) == 0 ) {
		isserver = 1;
	    }
	    else {
		if (host_or_path == NULL)
		    host_or_path = myhostname;

		if ( (hp = gethostbyname((UCHAR *) host_or_path)) )
		    memmove((UCHAR *) &sockname_in.sin_addr,
			    (UCHAR *) hp->h_addr,
			    (size_t) hp->h_length);
#ifdef DGUX
		else if ((sockname_in.sin_addr.s_addr = inet_addr(host_or_path).s_addr)
#else
		else if ((sockname_in.sin_addr.s_addr = inet_addr(host_or_path))
#endif
				== (unsigned long) -1) {
		    status = -1;
		    break;	/* break early */
		}

		sockname_in.sin_port = htons((u_short)portnum);

		status = connect(SIO_FD(buf),
				 (struct sockaddr *) &sockname_in,
				 sizeof (struct sockaddr_in));
	    }
	    break;
    }

    switch (socktype) {
	case SOCK_STREAM :
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_STREAM;
	    if (isserver) {
		status = listen(SIO_FD(buf), v7);
		SIO_FLAGS(buf) |= SIOF_NEEDACCEPT;
	    }
	    break;
	case SOCK_DGRAM :
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_DGRAM;
	    if (isserver && domain == AF_INET) {
		struct sockaddr_in *sa;
		SIO_SOCKET_ADDRESS(buf) = malloc(sizeof (struct sockaddr_in));
		SIO_SOCKET_ADDRESS_LEN(buf) = sizeof (struct sockaddr_in);
		sa = (struct sockaddr_in *) SIO_SOCKET_ADDRESS(buf);
		memset(sa, 0, sizeof (struct sockaddr_in *));
		sa->sin_family = AF_INET;
		sa->sin_addr.s_addr = INADDR_ANY;
		/* any initial writes will go to the discard service (port 9) */
		sa->sin_port = 9;
	    }
	    break;
    }

#endif /* UNIX */

/* FIXME: DOS and MacOS need to be updated to match the unix rewrite */
#ifdef DOS
    switch (socktype) {
	case ALS_STREAM:
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_STREAM;
	    if (v4 == SIOM_READ) {	/* accept connection and can write */
		sockname.lsocket = portnum;
		sockname.protocol = STREAM;

		SIO_FD(buf) = net_listen(-1, STREAM, &sockname);
		if (SIO_FD(buf) == -1) {
		    pneterror("net_listen");
		    exit(1);
		}
	    }
	    else {		/* (v4 == SIOM_WRITE); request connection and
				 * can read
				 */
		sockname.fhost = nm_res_name(host_or_path, (UCHAR *) NULL, 0);
		sockname.fsocket = portnum;
		sockname.lsocket = 0;
		sockname.protocol = STREAM;

		SIO_FD(buf) = net_connect(-1, STREAM, &sockname);
		if (SIO_FD(buf) == -1) {
		    pneterror("net_connect");
		    exit(1);
		}
	    }
	    break;		/* case ALS_STREAM */

	case ALS_DGRAM:
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_DGRAM;
	    sockname.lsocket = portnum;
	    sockname.protocol = DGRAM;

	    SIO_FD(buf) = net_connect(-1, DGRAM, &sockname);
	    if (SIO_FD(buf) == -1) {
		pneterror("net_connect");
		exit(1);
	    }
	    /* FIXME:  This has not been tested */
	    sockname.fhost = nm_res_name(host_or_path, (UCHAR *) NULL, 0);
	    sockname.fsocket = portnum;
	    sockname.lsocket = portnum;
	    sockname.protocol = DGRAM;
	    SIO_SOCKET_ADDRESS(buf) = malloc(sizeof sockname);
	    memmove(SIO_SOCKET_ADDRESS(buf), &sockname, sizeof sockname);
	    SIO_SOCKET_ADDRESS_LEN(buf) = sizeof sockname;

	    break;		/* case ALS_DGRAM */

	    /* OTHER TYPES OF SOCKETS GO HERE */

	default:
	    FAIL;
    }
#endif /* DOS */

#ifdef MacOS

    if ((SIO_FD(buf) = socket(domain, socktype, 0)) == -1) {
    SIO_ERRCODE(buf) = SIOE_SYSCALL;
    SIO_ERRNO(buf) = errno;
    FAIL;
    }

    switch (domain) {
    case AF_INET :
        gethostname(myhostname, MAXHOSTNAMELEN);
        memset(&sockname_in, 0, sizeof sockname_in);
        sockname_in.sin_family = AF_INET;
        sockname_in.sin_addr.s_addr = INADDR_ANY;
        sockname_in.sin_port = htons((u_short)portnum);

        if ( (host_or_path == NULL || strcmp(myhostname,host_or_path) == 0)
         && bind(SIO_FD(buf), (struct sockaddr *) &sockname_in,
                 sizeof (struct sockaddr_in)) == 0 ) {
        isserver = 1;
        }
        else {
        if (host_or_path == NULL)
            host_or_path = myhostname;

        if ( (hp = gethostbyname((UCHAR *) host_or_path)) )
            memmove((UCHAR *) &sockname_in.sin_addr,
                (UCHAR *) hp->h_addr,
                (size_t) hp->h_length);
        else if ((sockname_in.sin_addr.s_addr = inet_addr(host_or_path).s_addr)
                == (unsigned long) -1) {
            status = -1;
            break;  /* break early */
        }

        sockname_in.sin_port = htons((u_short)portnum);

        status = connect(SIO_FD(buf),
                 (struct sockaddr *) &sockname_in,
                 sizeof (struct sockaddr_in));
        }
        break;
    }

    switch (socktype) {
    case SOCK_STREAM :
        SIO_TYPE(buf) = SIO_TYPE_SOCKET_STREAM;
        if (isserver) {
        status = listen(SIO_FD(buf), v7);
        SIO_FLAGS(buf) |= SIOF_NEEDACCEPT;
        }
        break;
    case SOCK_DGRAM :
        SIO_TYPE(buf) = SIO_TYPE_SOCKET_DGRAM;
        if (isserver && domain == AF_INET) {
        struct sockaddr_in *sa;
        SIO_SOCKET_ADDRESS(buf) = (long) malloc(sizeof (struct sockaddr_in));
        SIO_SOCKET_ADDRESS_LEN(buf) = sizeof (struct sockaddr_in);
        sa = (struct sockaddr_in *) SIO_SOCKET_ADDRESS(buf);
        memset(sa, 0, sizeof (struct sockaddr_in *));
        sa->sin_family = AF_INET;
        sa->sin_addr.s_addr = INADDR_ANY;
        /* any initial writes will go to the discard service (port 9) */
        sa->sin_port = 9;
        }
        break;
    }
#endif /* MacOS */

    if (status == -1) {
	if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (t1 == WTP_UNBOUND && domain == AF_INET) {
	PWord vh;
	int th;
	w_mk_uia(&vh,&th,myhostname);
	(void) w_unify(v1,t1, vh,th);	/* cannot fail */
    }

    if (t2 == WTP_UNBOUND && domain == AF_INET) {
	int size = sizeof sockname_in;
	if (getsockname(SIO_FD(buf),
		        (struct sockaddr *) &sockname_in,
			&size) == 0) {
	    (void) w_unify(v2,t2,sockname_in.sin_port,WTP_INTEGER);
	    /* 
	     * The above call to w_unify will fail only if the programmer
	     * writing the code to open a socket is a total moron. He would
	     * have had to specify both the host and port as the same variable.
	     *
	     * We will succeed anyway.  If somebody changes this code so
	     * that it fails instead, you should close the socket and
	     * deallocate any other resources allocated above.
	     */
	}
    }

    incr_fdrefcnt(SIO_FD(buf));
    SUCCEED;
}

static void
delete_stream_name(vsd)
    PWord vsd;
{
#ifdef AF_UNIX
    PWord vns,vn,vd;
    int   tns,tn,td;
    UCHAR  *pathname, *domainname;
    w_get_argn(&vns,&tns, vsd, SIO_SD_STREAM_NAME);
    if (tns == WTP_STRUCTURE) {
	/* FIXME: Hardcoded constants */
	w_get_argn(&vn,&tn, vns, 1); /* Host or path name */
	w_get_argn(&vd,&td, vns, 3); /* domain name */
	if (   getstring(&pathname,vn,tn)
	    && getstring(&domainname,vd,td)
	    && strcmp(domainname,"unix") == 0 )
	    unlink(pathname);
    }
#endif /* AF_UNIX */
}


/*
 * accept_connection will accept a connection on a stream socket.  If the
 * accept succeeds or if no accept is necessary, 0 will be returned.  If
 * accept fails, accept_connection will return -1.
 */

static int
accept_connection(vsd, buf)
    PWord vsd;
    char  *buf;
{
    if (SIO_FLAGS(buf) & SIOF_NEEDACCEPT) {
	int newfd = accept(SIO_FD(buf),
			   (struct sockaddr *) 0, (int *) 0);
	if (newfd < 0) {
	    return -1;
	}
	else {
	    SIO_FLAGS(buf) &= ~SIOF_NEEDACCEPT;
	    if (decr_fdrefcnt(SIO_FD(buf))) {
		delete_stream_name(vsd);
		/* close connection descriptor */
		if (close(SIO_FD(buf)) < 0)
		    perror("accept_connection");
	    }
	    SIO_FD(buf) = newfd;
	    incr_fdrefcnt(newfd);
	}
    }
    return 0;
}

/*
 * sio_is_server_socket(Stream) will succeed if Stream is bound to a
 * socket stream which requires an accept prior to a buffer read or 
 * write operation.
 */

int
sio_is_server_socket()
{
    PWord v1;
    int t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;
    
    if ( SIO_TYPE(buf) == SIO_TYPE_SOCKET_STREAM 
	&& (SIO_FLAGS(buf) & SIOF_NEEDACCEPT) )
	SUCCEED;
    else if ( SIO_TYPE(buf) == SIO_TYPE_SOCKET_DGRAM
	     && SIO_SOCKET_ADDRESS(buf) )
	SUCCEED;
    else
	FAIL;
}

/*
 * sio_accept_socket_connection(Stream) will accept a connection on a socket.
 * This may be useful for writable server sockets.
 */

int
sio_accept_socket_connection()
{
    PWord v1;
    int t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;
    
    if (accept_connection(v1, buf) == 0)
	SUCCEED;
    else {
	SIO_ERRCODE(buf) = SIOE_SYSCALL;
	SIO_ERRNO(buf) = errno;
	FAIL;
    }
}

#endif /* HAVE_SOCKET */

#ifdef REXEC

/*
 * sio_rexec(Host, Command, User, Password, RS, WS, ES)
 */

int
sio_rexec()
{
#ifndef __GO32__
    PWord v1, v2, v3, v4, v5, v6, v7;
    int   t1, t2, t3, t4, t5, t6, t7;
    char *rbuf, *wbuf, *ebuf;
    int rfd, wfd, efd, issock;
    UCHAR *command, *hostname, *username, *password;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);
    w_get_An(&v7, &t7, 7);

    if (!getstring(&command, v2, t2))
	FAIL;
    
    if (!getstring(&hostname, v1, t1))
	hostname = NULL;

    if (!getstring(&username, v3, t3))
	username = NULL;
    
    if (!getstring(&password, v4,t4))
	password = NULL;
    
    rbuf = (char *)get_stream_buffer(v5,t5);
    wbuf = (char *)get_stream_buffer(v6,t6);
    ebuf = (char *)get_stream_buffer(v7,t7);

    rfd = wfd = efd = -1;
    
    if (hostname != NULL || username != NULL || password != NULL) {
#ifdef	HAVE_REXEC
					/* execution on remote machine */
	char myhostname[MAXHOSTNAMELEN];
	struct servent *se;

	se = getservbyname("exec", "tcp");

	if (se == NULL) {
	    perror("getservbyname");
	    FAIL;
	}

	if (hostname == NULL) {
	    gethostname(myhostname,MAXHOSTNAMELEN);
	    hostname = myhostname;
	}

	if ((rfd = rexec((char **) &hostname, se->s_port, username, password, command,
			 (ebuf ? &efd : NULL))) < 0) {
	    perror("rexec");
	    FAIL;
	}
	wfd = rfd;
	issock = 1;
#else	/* HAVE_REXEC */
	FAIL;				/* No remote execution facilities */
#endif	/* !HAVE_REXEC */
    }
    else {				/* execution on local machine */
#if defined(UNIX)
	int pid;
	int rfdpair[2], wfdpair[2], efdpair[2];
	
	if (rbuf == NULL)
	    rfdpair[0] = rfdpair[1] = -1;
	else
	    pipe(rfdpair);
	
	if (wbuf == NULL)
	    wfdpair[0] = wfdpair[1] = -1;
	else
	    pipe(wfdpair);
	
	if (ebuf == NULL) 
	    efdpair[0] = efdpair[1] = STDERR_FILENO;
	else
	    pipe(efdpair);

	if ( (pid = fork()) < 0) {
	    FAIL;			/* Can't fork anymore */
	}
	else if (pid == 0) {		/* The child process */
	    int i;

	    /* Disassociate child from controlling terminal */
#ifdef	HAVE_SETSID 
	    setsid();
#elif	defined(HAVE_BSD_SETPGRP)
	    /* FIXME:  There is probably more to be done here */
	    (void)setpgrp(0,0);
#else	/* probably SysV without setsid */
	    (void)setpgrp();
#endif	/* HAVE_SETSID */

	    /* Close unneeded file descriptors */
	    if (!openmax) {
		incr_fdrefcnt(0);
		decr_fdrefcnt(0);
	    }
	    for (i=0; i<openmax; i++)
		if (i != rfdpair[1] && i != wfdpair[0] && i != efdpair[1])
		    close(i);

	    /* Use /dev/null for input or output as needed */
	    if (rfdpair[1] == -1) {
		if ((rfdpair[1] = open("/dev/null",O_WRONLY)) < 0)
		    _exit(1);
	    }
	    if (wfdpair[0] == -1) {
		if ((wfdpair[0] = open("/dev/null",O_RDONLY)) < 0)
		    _exit(1);
	    }

	    /* Setup stdin, stdout, and stderr */
	    if (wfdpair[0] != STDIN_FILENO) {
		dup2(wfdpair[0], STDIN_FILENO);
		close(wfdpair[0]);
	    }
	    if (rfdpair[1] != STDOUT_FILENO) {
		dup2(rfdpair[1], STDOUT_FILENO);
		close(wfdpair[1]);
	    }

	    if (efdpair[0] == -1)
		dup2(STDOUT_FILENO, STDERR_FILENO);
	    else if (efdpair[1] != STDERR_FILENO) {
		dup2(efdpair[1], STDERR_FILENO);
		close(efdpair[1]);
	    }

	    /* Run the command */
	    execl("/bin/sh", "sh", "-c", command, (char *) 0);
	    _exit(1);	/* should not get here  */
	}
	else {				/* The parent process */
	    deathwatch();		/* Bury children when they die */
	    close(rfdpair[1]);
	    rfd = rfdpair[0];
	    close(wfdpair[0]);
	    wfd = wfdpair[1];
	    if (efdpair[0] == -1) {
		close(efdpair[1]);
		efd = efdpair[0];
	    }
	    else
		efd = rfd;
	}

	issock = 0;
#else /* UNIX */
	FAIL;
#endif /* !UNIX */
    }

    if (rbuf) {
	SIO_TYPE(rbuf) = issock ? SIO_TYPE_SOCKET_STREAM : SIO_TYPE_FILE;
	SIO_FD(rbuf) = rfd;
	incr_fdrefcnt(rfd);
    }
    else {
#ifdef HAVE_SOCKET
	if (issock)
	    shutdown(rfd, 0);
#endif /* HAVE_SOCKET */
    }

    if (wbuf) {
	SIO_TYPE(wbuf) = issock ? SIO_TYPE_SOCKET_STREAM : SIO_TYPE_FILE;
	SIO_FD(wbuf) = wfd;
	incr_fdrefcnt(wfd);
    }
    else {
#ifdef HAVE_SOCKET
	if (issock)
	    shutdown(wfd, 1);
#endif /* HAVE_SOCKET */
    }
    
    if (ebuf) {
	SIO_TYPE(ebuf) = issock ? SIO_TYPE_SOCKET_STREAM : SIO_TYPE_FILE;
	SIO_FD(ebuf) = efd;
	incr_fdrefcnt(efd);
    }
#endif /* GO32 */
    SUCCEED;
}
#endif /* REXEC */


/*
 * stream_is_ready(buf,waittime)
 *
 */

static int
stream_is_ready(buf, usec_to_wait)
    char *buf;
    long usec_to_wait;
{
    switch (SIO_TYPE(buf)) {
	case SIO_TYPE_FILE :
	case SIO_TYPE_SOCKET_DGRAM :
	case SIO_TYPE_SOCKET_STREAM : {
#ifdef HAVE_SELECT
	    fd_set rfds, wfds, efds;
	    struct timeval wait_time;
	    FD_ZERO(&rfds);
	    FD_ZERO(&wfds);
	    FD_ZERO(&efds);
	    if (SIO_FLAGS(buf) & (SIOF_READ | SIOF_NEEDACCEPT))
		FD_SET(SIO_FD(buf), &rfds);
	    else 
		FD_SET(SIO_FD(buf), &wfds);

	    wait_time.tv_sec = usec_to_wait / 1000000;
	    wait_time.tv_usec = usec_to_wait % 1000000;

	    if (select(SIO_FD(buf)+1, &rfds, &wfds, &efds, &wait_time)  > 0)
		return 1;
	    else
		return 0;
#else
	    return 1;
#endif
	}
	default :
	    return 1;
    }
}

#if HAVE_SOCKET
/*
 * sio_poll will check if an I/O operation is ready on a stream.  It will
 * succeed if an I/O operation on the stream will not block. Otherwise it will
 * fail.  A time value in microseconds may be given as the time to wait
 * for the stream to become ready.
 *
 * sio_poll(Stream, WaitTime)
 */

int
sio_poll()
{
    PWord v1, v2;
    int t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (t2 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (stream_is_ready(buf,v2))
	SUCCEED;
    else
	FAIL;
}
#endif /* HAVE_SOCKET */

#ifdef WINIOBASIS
/*
 *	window_insert_pos(WinID, WinPos)
 *
 *	window_insert_pos is called from Prolog to get the
 *	current insertion point for the window entity associated with
 *	WinID (e.g., a Motif Text Widget); this gets the value which is
 *	currently stored in the stream descriptor; it does not go to
 *	the window & windowing system for it; that is done from
 *	winsGetTextInsertionPostion/2 which is conditionally defined
 *	to use the correct call to the underlying window system.
 */


int
win_insert_pos()
{
  PWord v1,v2;
  int t1,t2;
  UCHAR *buf;
  
  w_get_An(&v1,&t1,1);
  w_get_An(&v2,&t2,2);
  
  if ((buf=get_stream_buffer(v1,t1)) == (UCHAR *) 0)
	FAIL;
  
  if (w_unify(v2,t2,(PWord)WINS_INSERT_POS(buf),WTP_INTEGER))
	SUCCEED;
  else
	FAIL;
}

int
set_win_insert_pos()
{
  PWord v1,v2;
  int t1,t2;
  UCHAR *buf;
  
  w_get_An(&v1,&t1,1);
  w_get_An(&v2,&t2,2);

/* printf("t1=%d  t2=%d\n",t1,t2); */

  if ((buf=get_stream_buffer(v1,t1)) == (UCHAR *) 0 || t2 != WTP_INTEGER)
	FAIL;
  
/* printf("Buf & t2 ok\n"); */

  WINS_INSERT_POS(buf) = (int)v2;
  SUCCEED;
}

/* 
   int_or_float(t,v)
 */
static int
int_or_float(int t,PWord v)
{
#ifdef DoubleType
    if ( (t == WTP_INTEGER) || (t == WTP_DOUBLE))
		return(1);
	else
		return(0);
#else  /* not-DoubleType */
	PWord functor;
	int   arity;

    if (t == WTP_INTEGER)
		return(1);
	else if (t != WTP_STRUCTURE)
		return(0);
	else
		w_get_arity(&arity, v);
		w_get_functor(&functor, v);

		if (arity != 4 || functor != TK_DDOUBLE)
			return(0);
		else
			return(1);
#endif /* DoubleType */
}



/*
 * sio_window_open(ID, Stream, Mode, Buffering,WinInsertPos,WinPosGV)
 *
 * sio_window_open is called from Prolog to open a stream which is 
 * associated with a window, and largely managed from Prolog.
 *
 * Note: WINS_INSERT_POS is manipulated by the two functions above.
 * WINS_GV_POS is set with an integer at the time the stream is
 * opened; this integer is the number of a Prolog global variable
 * (allocated by open_window_stream), and is never changed.  This
 * variable contains the cursor position in the text widget. Currently,
 * this is only used by the Prolog side.  However, the plan is to
 * change the update callback (on each keystroke) to run in C, so
 * that it will use this Prolog global variable at that time. [Prolog
 * will still need to get at that value at times too, hence this
 * device.]
 *
 */
int
sio_window_open()
{
    PWord v1, v2, v3, v4, v5, v6;
    int   t1, t2, t3, t4, t5, t6;
    unsigned char *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);

/* printf("sio_w_o: types: t1=%d t2=%d t3=%d t4=%d t5=%d t6=%d\n",
			t1, t2, t3, t4, t5, t6);  */


    if ((buf = get_stream_buffer(v2, t2)) == (unsigned char *) 0)
	FAIL;


    if ((t3 != WTP_INTEGER) || (t4 != WTP_INTEGER) || (t6 != WTP_INTEGER)
	) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

	if (int_or_float(t1,v1) == 0)
	{ SIO_ERRCODE(buf) = SIOE_INARG;
	  FAIL; 
	}
	if (int_or_float(t5,v5) == 0)
	{ SIO_ERRCODE(buf) = SIOE_INARG;
	  FAIL; 
	}
/*
printf("t5 ok\n");
printf("types ok\n");
*/

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;
/* printf("flags ok\n"); */

    SIO_TYPE(buf) = SIO_TYPE_PROLOG_MANAGED;

#ifdef DoubleType
    	SIO_FD(buf) = (unsigned long) v1;
#else
	if (t1 == WTP_INTEGER)
	{
    	SIO_FD(buf) = (unsigned long) v1;
		}
	else /* v1 is a structure representing a float:
    	    t1 == WTP_STRUCTURE)   */
	{
		PWord functor, vv;
		int   arity, tt;
		double uu=0;
		int i;

		w_get_arity(&arity, v1);
		w_get_functor(&functor, v1);
			/* Must have: arity == 4 && functor == TK_DDOUBLE */

		for (i = 0; i < 4; i++) {
		    w_get_argn(&vv, &tt, v1, i + 1);
			*(((short *) &uu)+ i) = (short) vv;

/*  printf("i=%d vv=%hd uu=%d\n",i,(short)vv,(unsigned long)uu);   */
		}
    	SIO_FD(buf) = (unsigned long) uu;

	}
#endif	/* DoubleType */

/* printf("SIO_FD(buf)=%d\n",SIO_FD(buf)); */


    SIO_ERRCODE(buf) = SIOE_NORMAL;

    WINS_INSERT_POS(buf) = (int)v5;
    WINS_POS_GV(buf)	 = (int)v6;

    SUCCEED;
}

#endif /* WINIOBASIS */


/*
 * sio_generic_open(ID, Stream, Mode, Buffering)
 *
 * sio_generic_open is called from Prolog to open a stream which is largely
 * managed from prolog.  Since the stream is largely managed from prolog,
 * there is not much to do -- a small amount of initialization will suffice.
 *
 * This function is used (at present) to open atom, code list, and window
 * streams. 
 */

int
sio_generic_open()
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (t1 != WTP_INTEGER || t3 != WTP_INTEGER || t4 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;

    SIO_TYPE(buf) = SIO_TYPE_PROLOG_MANAGED;
    SIO_FD(buf) = (int) v1;
    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}


/*
 * shift_buffer is called by sio_bufshift and internally to shift the contents
 * of the buffer and to update the related positional values.
 */

static void
shift_buffer(buf)
    UCHAR *buf;
{
    int count = SIO_LPOS(buf) - SIO_CPOS(buf);
    if (count <= 0) {
	SIO_BUFPOS(buf) += SIO_LPOS(buf);
	SIO_CPOS(buf) = SIO_LPOS(buf) = 0;
    }
    else {
	SIO_BUFPOS(buf) += SIO_CPOS(buf);
	memmove(SIO_BUFFER(buf), SIO_BUFFER(buf)+SIO_CPOS(buf), (size_t)count);
	SIO_CPOS(buf) = 0;
	SIO_LPOS(buf) = count;
    }
}

/*
 * sio_bufshift(Stream)
 *
 *      Shifts (or empties) the buffer so that new data may be read in.  It
 *	is only necessary to call this function for streams which implement
 *	read_buffer in prolog.  Streams implemented by code in this file
 *	call shift_buffer to perform the same actions but without the overhead.
 */

int
sio_bufshift()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (SIO_FLAGS(buf) & SIOF_DIRTY) {
	SIO_ERRCODE(buf) = SIOE_WRITE;
	FAIL;
    }

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    shift_buffer(buf);

    SUCCEED;

}

/*
 * write_buf will write out SIO_CPOS characters starting at the
 * beginning of the buffer.  No updates of SIO_LPOS or SIO_CPOS are
 * performed. It is up to the caller to adjust these values.
 */

static int
write_buf(vsd,buf)
    PWord vsd;
    UCHAR *buf;
{
    int   writeflg = 0;

#ifdef SysVIPC
    struct msgbuf *msgp;
#endif /* SysVIPC */

    if (SIO_FLAGS(buf) & SIOF_READ) {
	if (lseek((int) SIO_FD(buf), (long) SIO_BUFPOS(buf), SEEK_SET) == -1) {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	    return 0;
	}
    }

    if ((SIO_FLAGS(buf) & SIOF_DONTBLOCK) && !stream_is_ready((char *)buf,0)) {
	SIO_ERRCODE(buf) = SIOE_NOTREADY;
	return 0;
    }

    switch (SIO_TYPE(buf)) {
	case SIO_TYPE_FILE:
	    writeflg = write(SIO_FD(buf), (char *)SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
	    break;

#ifdef SysVIPC
	case SIO_TYPE_SYSVQ:
	    msgp = (struct msgbuf *) ((long *) SIO_BUFFER(buf) - 1);
	    msgp->mtype = SIO_SysVIPC_MTYPE(buf);
	    writeflg = msgsnd(SIO_FD(buf), msgp, SIO_LPOS(buf),
			      SIO_SysVIPC_WAIT(buf));
	    break;
#endif /* SysVIPC */

#ifdef SSBQ
	case SIO_TYPE_SSBQ:
	    writeflg = send_msg(SIO_SSBQ_TARGETNODE(buf),
				SIO_FD(buf),
				(ushort) SIO_LPOS(buf),
				SIO_BUFFER(buf),
				(ushort) 0,
				"DUMMY",
				(UCHAR *) 0);
	    break;
#endif /* SSBQ */

#ifdef HAVE_SOCKET
	case SIO_TYPE_SOCKET_STREAM:
	    if (accept_connection(vsd, buf)) {
		writeflg = -1;
		break;		/* break early */
	    }

#ifdef UNIX
	    writeflg = write(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#elif defined(DOS)
	    writeflg = net_write(SIO_FD(buf), SIO_BUFFER(buf),
				 SIO_LPOS(buf), 0);
#elif defined(MacOS)
            writeflg = write(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf))
;
#endif /* UNIX */
	    break;

	case SIO_TYPE_SOCKET_DGRAM:
#ifdef UNIX
	    if (SIO_SOCKET_ADDRESS(buf))
		writeflg = sendto(SIO_FD(buf),
				  SIO_BUFFER(buf),
				  SIO_LPOS(buf), 0,
				  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
				  SIO_SOCKET_ADDRESS_LEN(buf));
	    else
		writeflg = write(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#elif defined(DOS)
	    /* FIXME: SIO_SOCKET_ADDRESS(buf) == 0 */
	    writeflg = net_writeto(SIO_FD(buf),
				   SIO_BUFFER(buf),
				   SIO_LPOS(buf),
				   SIO_SOCKET_ADDRESS(buf),
				   0);
#elif defined(MacOS)
            if (SIO_SOCKET_ADDRESS(buf))
                writeflg = sendto(SIO_FD(buf),
                                  SIO_BUFFER(buf),
                                  SIO_LPOS(buf), 0,
                                  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
                                  SIO_SOCKET_ADDRESS_LEN(buf));
            else
                writeflg = write(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#endif /* UNIX, DOS, MacOS */
	    break;
#endif /* HAVE_SOCKET */

	default:
	    SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	    return 0;
	    break;
    }

    if (writeflg == -1) {
	if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	return 0;
    }

    SIO_FLAGS(buf) &= ~SIOF_DIRTY;
    return 1;
}


/*
 * sio_close(Stream)
 *
 *      Called from Prolog to close streams.
 */

int
sio_close()
{
    PWord v1;
    int   t1;
    UCHAR *buf;
    int   closeflg = 0;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (SIO_FLAGS(buf) & SIOF_DIRTY) {
	if (!write_buf(v1,buf)) {
	    FAIL;
	}
    }

    switch (SIO_TYPE(buf)) {
	case SIO_TYPE_FILE:
	    if (decr_fdrefcnt(SIO_FD(buf)))
		closeflg = close(SIO_FD(buf));
	    break;

#ifdef SysVIPC
	case SIO_TYPE_SYSVQ:
	    /* Surely we must have to do something here! */
	    break;
#endif /* SysVIPC */

#ifdef SSBQ
	case SIO_TYPE_SSBQ:
	    if (SIO_FLAGS(buf) == SIOF_READ) {
		/* read mode: destroy the queue */
		closeflg = delete_queue((UCHAR *) SIO_FD(buf));
		if (closeflg == -1) {
		    SIO_ERRCODE(buf) = SIOE_SYSCALL;
		    SIO_ERRNO(buf) = errno;
		    return 0;
		}
	    }
	    /* free the queuename and targetnode storage: */
	    free((char *) SIO_FD(buf));
	    free((char *) SIO_SSBQ_TARGETNODE(buf));
	    break;
#endif /* SSBQ */

#ifdef HAVE_SOCKET
	case SIO_TYPE_SOCKET_STREAM:
	    if (SIO_FLAGS(buf) & SIOF_WRITE) {
		if (decr_fdrefcnt(SIO_FD(buf)))
		    goto close_socket;
		else
		    closeflg = shutdown(SIO_FD(buf),1);
		break;
	    }
	case SIO_TYPE_SOCKET_DGRAM:

	    if (decr_fdrefcnt(SIO_FD(buf))) {
close_socket:
		/* If we are closing the final reference to a unix domain
		 * socket on which we are waiting for a connection, we
		 * need to unlink the pathname.
		 */

		if (SIO_FLAGS(buf) & SIOF_NEEDACCEPT)
		    delete_stream_name(v1);

#ifdef UNIX
		if (SIO_TYPE(buf) == SIO_TYPE_SOCKET_DGRAM)
		    closeflg = close(SIO_FD(buf));
		else {
/* FIXME: Figure out how to check for broken shutdown */
#if defined(SysVR3)
		    /* shutdown seems to be broken on svr3 */
		    closeflg = close(SIO_FD(buf));
#else /* SysVR3 */
		    closeflg = shutdown(SIO_FD(buf),2);
#endif /* SysVR3 */
		}
#elif defined(DOS)
		closeflg = net_release(SIO_FD(buf));
#elif defined(MacOS)
                if (SIO_TYPE(buf) == SIO_TYPE_SOCKET_DGRAM)
                    closeflg = close(SIO_FD(buf));
                else {
                    closeflg = shutdown(SIO_FD(buf),2);
                }
#endif /* MacOS */
		if (SIO_SOCKET_ADDRESS(buf))
		    free((char *) SIO_SOCKET_ADDRESS(buf));
	    }
	    break;
#endif /* HAVE_SOCKET */

	default:
	    SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	    FAIL;
	    break;
    }

    if (closeflg == -1) {
	if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
} /* sio_close */


#ifndef SIO_ASM

/*
 * sio_get_byte(SD,B)
 *
 *      Called from Prolog to get a byte out of a buffer
 */

int
sio_get_byte()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;
    int   pos;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    if (t2 != WTP_UNBOUND && (t2 != WTP_INTEGER || v2 < -1 || v2 > 255)) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    pos = SIO_CPOS(buf);
    if (pos >= SIO_LPOS(buf)) {
	SIO_ERRCODE(buf) = SIOE_READ;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SIO_CPOS(buf) = pos + 1;

    pos = SIO_BUFFER(buf)[pos];	/* get out byte and put in pos */

    if (pos == '\n') {
	SIO_LINENUM(buf)++;
	SIO_COLUMN(buf) = 0;
    }
    else
	SIO_COLUMN(buf)++;

    if (!w_unify(v2, t2, pos, WTP_INTEGER))
	FAIL;
    else
	SUCCEED;
}

/*
 * sio_put_byte(SD,Byte)
 */

int
sio_put_byte()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;
    int   pos;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_WRITE)) {
	SIO_ERRCODE(buf) = SIOE_ILLWRITE;
	FAIL;
    }

    if ((SIO_FLAGS(buf) & (SIOF_READ | SIOF_EOF)) == SIOF_READ &&
	SIO_LPOS(buf) == 0) {
	SIO_ERRCODE(buf) = SIOE_READ;
	FAIL;
    }

    if (t2 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    pos = SIO_CPOS(buf);
    SIO_BUFFER(buf)[pos] = v2;
    pos++;
    SIO_CPOS(buf) = pos;
    if (pos > SIO_LPOS(buf))
	SIO_LPOS(buf) = pos;
    SIO_FLAGS(buf) |= SIOF_DIRTY;

    if ((SIO_FLAGS(buf) & SIOF_BBYTE) ||
	((SIO_FLAGS(buf) & SIOF_BLINE) && v2 == '\n') ||
	(SIO_LPOS(buf) == SIO_BFSIZE(buf))) {
	SIO_ERRCODE(buf) = SIOE_WRITE;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}
#endif /* SIO_ASM */

/*
 * sio_unget_byte(SD)
 *
 *      Backs CPOS up one byte for the given stream.   This function is used
 *      to implement peek_char/1.
 */

int
sio_unget_byte()
{
    PWord v;
    int   t;
    UCHAR *buf;
    int   pos;

    w_get_An(&v, &t, 1);

    if ((buf = get_stream_buffer(v, t)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    pos = SIO_CPOS(buf);
    if (pos == 0) {
	SIO_ERRCODE(buf) = SIOE_UNGET;
	FAIL;
    }

    SIO_CPOS(buf) = pos - 1;
    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}

#ifdef SSBQ
/*
 * ssbq_get_msg(QName)
 */
int
ssbq_get_msg()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *queuename;
    UCHAR *buf;
    UCHAR *incoming_buf;
    int   get_msg_status, num_cs, cmpres;

    w_get_An(&v1, &t1, 1);

    &*&*&*&^*^*+_ + $$$
	buf has not been set ! !!!-raman
	Whoever uses this portion of code should fix this
	* &*&*&*&***&*&*&$$$

	if (!getstring(&queuename, v1, t1)) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    get_msg_status = get_msg(queuename, &incoming_buf,
			     &num_cs, -1, (UCHAR **) 0);

    if (get_msg_status == 0)
	SUCCEED;
    else
	FAIL;

}
#endif /* SSBQ */

/* Correct various bugs in different Unix emulation libraries. */
#if defined(THINK_C)

/* Think C's setvbuf() is broken, so stdin cannot do line buffering,
   This function works around it by using fgets().
*/
static int corrected_read(int fn, char *buffer, int count)
{
    int result;

    if (fn == 0) {
    	char *line;
	line = fgets(buffer, count, stdin);
	result = ferror(stdin) ? EOF : ( line == NULL ? 0 : strlen(line));
    } else {
	result = read(fn, buffer, count);
    }
    	
    return result;
}
#elif defined(__MWERKS__)

static int corrected_read(int fn, char *buffer, int count)
{
    int result;
    
    result = read(fn, buffer, count);
    
    if (fn == 0 && result > 0) {
	int i;
	for (i = 0; i < result; i++) if (buffer[i] == 5) result = 0;
    }
	
    return result;
}
#endif  /* defined(THINK_C) */

/*
 * sio_readbuffer(SD)
 */

int
sio_readbuffer()
{
    PWord v1;
    int   t1;
    int   nchars;
    UCHAR *buf, *buffer;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    if (SIO_FLAGS(buf) & SIOF_EOF) {
	SIO_ERRCODE(buf) = SIOE_EOF;
	FAIL;
    }

    if (SIO_FLAGS(buf) & SIOF_DIRTY)
	write_buf(v1,buf);

    if ((SIO_FLAGS(buf) & SIOF_DONTBLOCK) && !stream_is_ready((char *)buf,0)) {
	SIO_ERRCODE(buf) = SIOE_NOTREADY;
	return 0;
    }

    shift_buffer(buf);
    nchars = SIO_BFSIZE(buf) - SIO_LPOS(buf);
    buffer = SIO_BUFFER(buf) + SIO_LPOS(buf);

    switch (SIO_TYPE(buf)) {
	case SIO_TYPE_FILE:
#if defined(THINK_C) || defined(__MWERKS__)
		nchars = corrected_read(SIO_FD(buf), (char *)buffer, nchars);
#else
	    nchars = read(SIO_FD(buf), (char *)buffer, (size_t)nchars);
#endif
	    break;
#ifdef SysVIPC
	case SIO_TYPE_SYSVQ: {
	    struct msgbuf *msgp;
	    long mtype_save;
	    msgp = (struct msgbuf *) (buffer - sizeof msgp->mtype);
	    mtype_save = msgp->mtype;	/* save original data */
	    msgp->mtype = SIO_AUX2(buf);/* install the message type */
	    nchars = msgrcv(SIO_FD(buf), msgp, nchars,
			    SIO_AUX2(buf), SIO_AUX3(buf));
	    msgp->mtype = mtype_save;	/* restore original data */
	    break;		/* SIO_TYPE_SYSVQ */
	}
#endif /* SysVIPC */

#ifdef SSBQ
	case SIO_TYPE_SSBQ:
	    if (SIO_SSBQ_REMCS(buf) == 0) {
		UCHAR *incoming_buf;
		int num_cs;
		if (0 != get_msg(SIO_FD(buf), &incoming_buf,
			         &num_cs, SIO_SSBQ_WAIT(buf),
			         (UCHAR **) 0)) {
		    nchars = -1;
		    break;
		}
		SIO_SSBQ_USEDCS(buf) = (long) 0;
		SIO_SSBQ_REMCS(buf) = num_cs;
		(UCHAR *) SIO_SSBQ_INCB(buf) = incoming_buf;
		if (nchars >= num_cs)
		    nchars = num_cs;
	    }

	    memcpy(buffer,
		   (SIO_SSBQ_INCB(buf) + SIO_SSBQ_USEDCS(buf)),
		   nchars);
	    SIO_SSBQ_USEDCS(buf) += nchars;
	    SIO_SSBQ_REMCS(buf) -= nchars;

	    if ((SIO_SSBQ_REMCS(buf) == 0) &&
		((UCHAR *) SIO_SSBQ_INCB(buf) != (UCHAR *) 0))
		free((char *) SIO_SSBQ_INCB(buf));
	    break;
#endif /* SSBQ */

#ifdef HAVE_SOCKET
	case SIO_TYPE_SOCKET_STREAM:
	    if (accept_connection(v1,buf) < 0) {
		nchars = -1;
		break;			/* break early */
	    }

	    if ((SIO_FLAGS(buf) & SIOF_DONTBLOCK) && !stream_is_ready(buf,0)) {
		SIO_ERRCODE(buf) = SIOE_NOTREADY;
		return 0;
	    }
		
#if defined(UNIX)
	    nchars = read(SIO_FD(buf), buffer, (size_t)nchars);
#elif defined(DOS)
	    /* FIXME! */
	    nchars = net_read(SIO_FD(buf), buffer, nchars, &a, 0);
#elif defined(MacOS)
            nchars = read(SIO_FD(buf), buffer, (size_t)nchars);
#endif
	    break;

	case SIO_TYPE_SOCKET_DGRAM:
#if defined(UNIX)
	    if (SIO_SOCKET_ADDRESS(buf)) {
		int len = SIO_SOCKET_ADDRESS_LEN(buf);
		nchars = recvfrom(SIO_FD(buf), buffer,nchars, 0,
				  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
				  &len);
	    }
	    else
		nchars = read(SIO_FD(buf), buffer, (size_t)nchars);
#elif defined(DOS)
	    /* FIXME! */
	    nchars = net_read(SIO_FD(buf), buffer, nchars, &a, 0);
#elif defined(MacOS)
            if (SIO_SOCKET_ADDRESS(buf)) {
                int len = SIO_SOCKET_ADDRESS_LEN(buf);
                nchars = recvfrom(SIO_FD(buf), buffer,nchars, 0,
                                  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
                                  &len);
            }
            else
                nchars = read(SIO_FD(buf), buffer, (size_t)nchars);
#endif
	    SIO_FLAGS(buf) |= SIOF_EOF;	/* each packet is self contained */
	    break;
#endif /* HAVE_SOCKET */

	default:
	    SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	    FAIL;
	    break;
    }

    if (nchars < 0) {
	if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	FAIL;
    }
    else {
	SIO_ERRCODE(buf) = SIOE_NORMAL;
	SIO_LPOS(buf) += nchars;
	if (nchars == 0)
	    SIO_FLAGS(buf) |= SIOF_EOF;
	SUCCEED;
    }
}



/*
 * sio_writebuffer(SD)
 */

int
sio_writebuffer()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_WRITE)) {
	SIO_ERRCODE(buf) = SIOE_ILLWRITE;
	FAIL;
    }

    if (write_buf(v1,buf)) {
	shift_buffer(buf);
	SIO_ERRCODE(buf) = SIOE_NORMAL;
	SUCCEED;
    }
    else
	FAIL;
}

/*
 * sio_getpos(Stream,Pos)
 *
 *      sio_getpos is used to get the current file position.  This could be
 *      implemented from Prolog with sio_seek (see below), but is much faster
 *      and will work on non-seekable streams.
 */

int
sio_getpos()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;
    int   curpos;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    curpos = SIO_BUFPOS(buf) + SIO_CPOS(buf);

    if (w_unify(v2, t2, (PWord) curpos, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

static long
stream_end(buf)
    UCHAR *buf;
{
    long  curpos;
    long  endpos;

    curpos = lseek(SIO_FD(buf), 0, SEEK_CUR);
    endpos = lseek(SIO_FD(buf), 0, SEEK_END);
    (void) lseek(SIO_FD(buf), curpos, SEEK_SET);
    return endpos;
}

/*
 * sio_seek(Stream,OldPos,NewPos,Whence)
 *
 *      Stream  is a file stream to get/set the position of
 *      OldPos  will be unified with the current stream position
 *      NewPos  is the position to be set
 *      Whence  is the value used by lseek (0,1,2) to either
 *              set the absolute position, set from current position, or
 *              to set from end-of-file
 */

int
sio_seek()
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    UCHAR *buf;
    long  pos;
    long  curpos;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (NONSEEKABLE(SIO_TYPE(buf))) {
	SIO_ERRCODE(buf) = SIOE_ILLSEEK;
	FAIL;
    }

    if ((t2 != WTP_INTEGER && t2 != WTP_UNBOUND) || t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    curpos = SIO_BUFPOS(buf) + SIO_CPOS(buf);

    switch (v4) {
	case SEEK_SET:		/* 0 -- set from beginning */
	    pos = v3;
	    break;
	case SEEK_CUR:		/* 1 -- set from current position */
	    pos = curpos + v3;
	    break;
	case SEEK_END:		/* 2 -- set from end */
	    pos = stream_end(buf) + v3;
	    break;
	default:
	    SIO_ERRCODE(buf) = SIOE_INARG;
	    FAIL;
	    break;
    }

    if (SIO_BUFPOS(buf) <= pos && pos < SIO_BUFPOS(buf) + SIO_LPOS(buf))
	SIO_CPOS(buf) = pos - SIO_BUFPOS(buf);
    else {
	if (SIO_FLAGS(buf) & SIOF_DIRTY) {
	    if (!write_buf(v1,buf))
		FAIL;
	}
	if ((pos = lseek(SIO_FD(buf), v3, v4)) < 0) {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	    FAIL;
	}
	if (SIO_BUFPOS(buf) <= pos && pos < SIO_BUFPOS(buf) + SIO_LPOS(buf))
	    SIO_CPOS(buf) = pos - SIO_BUFPOS(buf);
	else {
	    SIO_BUFPOS(buf) = pos;
	    SIO_CPOS(buf) = 0;
	    SIO_LPOS(buf) = 0;
	}
	SIO_FLAGS(buf) &= ~SIOF_EOF;	/* clear eof flag */
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (w_unify(curpos, WTP_INTEGER, v2, t2))
	SUCCEED;
    else
	FAIL;

}

/*
 * skip_layout will skip over the so called layout text for the lexical
 * analyzer.  Layout text is white space, comments and newlines.
 *
 * skip_layout will return 1 if layout text (possibly none) was successfully
 * skipped and SIO_CPOS could be set to the start of a token within the
 * buffer.  If the buffer is empty or more needs to be read, 0 is returned.
 */

static int
skip_layout(buf)
    UCHAR *buf;
{
    register UCHAR *p, *lim;
    UCHAR *startpos;	/* used for determining value of SIO_COLUMN */
    int   nesting, inlinecomment;

    startpos = p = SIO_BUFFER(buf) + SIO_CPOS(buf);
    lim = SIO_BUFFER(buf) + SIO_LPOS(buf);

    if (!(SIO_FLAGS(buf) & (SIOF_INATOM | SIOF_INSTRING | SIOF_INQATOM))) {

	nesting = SIO_COMMENT(buf);
	inlinecomment = SIO_FLAGS(buf) & SIOF_LCOMMENT;

	while (p < lim) {
	    if (inlinecomment) {
		while (p < lim && *p != '\n')
		    p++;
		if (p < lim) {
		    p++;
		    inlinecomment = 0;
		    SIO_LINENUM(buf)++;
		    startpos = p;
		    SIO_COLUMN(buf) = 0;
		}
	    }
	    else if (nesting > 0) {
		lim--;		/* looking ahead two, so decrement lim */
		while (p < lim && (*p != '*' || *(p + 1) != '/')) {
		    if (*p == '/' && *(p + 1) == '*') {
			p++;
			nesting++;
		    }
		    else if (*p == '\n') {
			SIO_LINENUM(buf)++;
			startpos = p + 1;
			SIO_COLUMN(buf) = 0;
		    }
		    p++;
		}
		if (p == lim) {
		    if (*p != '*' && *p != '/') {
			if (*p == '\n') {
			    SIO_LINENUM(buf)++;
			    startpos = p + 1;
			    SIO_COLUMN(buf) = 0;
			}
			lim++;	/* increment lim if last char  */
			/* not star or slash */
			p++;	/* advance p in this case also */
		    }
		}
		else {
		    nesting--;
		    p += 2;
		    lim++;
		}
	    }
	    else {
		while (p < lim && (sio_chtb[*p] & SIOC_WHITESPACE)) {
		    if (*p == '\n') {
			SIO_LINENUM(buf)++;
			startpos = p + 1;
			SIO_COLUMN(buf) = 0;
		    }
		    p++;
		}
		if (p < lim) {
		    if (*p == '%') {
			inlinecomment = 1;
			p++;
		    }
		    else if (*p == '/') {
			if (p + 1 < lim) {
			    if (*(p + 1) == '*') {
				nesting++;
				p += 2;
			    }
			    else
				break;
			}
			else
			    break;
		    }
		    else
			break;
		}
	    }
	}
	SIO_COLUMN(buf) += (p - startpos);
	SIO_CPOS(buf) = p - SIO_BUFFER(buf);
	if (inlinecomment)
	    SIO_FLAGS(buf) |= SIOF_LCOMMENT;
	else
	    SIO_FLAGS(buf) &= ~SIOF_LCOMMENT;
	SIO_COMMENT(buf) = nesting;
    }

    if (p >= lim) {
	SIO_ERRCODE(buf) = SIOE_READ;
	return 0;
    }
    else
	return 1;
}

static int
octal(pp)
    UCHAR **pp;
{
    register UCHAR *p;
    register int val;

    p = *pp;
    val = *p - '0';
    p++;
    while (sio_chtb[*p] & SIOC_OCTAL) {
	val = (val << 3) + (*p - '0');
	p++;
    }
    *pp = p;
    return val;
}

static int
hexadecimal(pp)
    UCHAR **pp;
{
    register UCHAR *p;
    register int val;
    register int ctype;

    p = *pp;
    val = 0;
    while ( (ctype = sio_chtb[*p] & SIOC_HEXADECIMAL) ) {
	switch (ctype) {
	    case SIOC_DECIMAL:
		val = (val << 4) + (*p - '0');
		break;
	    case SIOC_HEXLOWER:
		val = (val << 4) + (*p - ('a' - 10));
		break;
	    case SIOC_HEXUPPER:
		val = (val << 4) + (*p - ('A' - 10));
		break;
	}
	p++;
    }
    *pp = p;
    return val;
}

static double
decimal(pp, ty)
    UCHAR **pp;
	int *ty;
{
    register UCHAR *p;
    double d, frac;

	*ty = WTP_INTEGER;
    p = *pp;
    d = *p++ - '0';
    frac = 0;

    while (sio_chtb[*p] & SIOC_DECIMAL)
	d = 10.0 * d + (*p++) - '0';

    if (*p == '.' && (sio_chtb[*(p + 1)] & SIOC_DECIMAL)) {
	UCHAR *s;
	
	*ty = WTP_DOUBLE;

	p++;
	while (sio_chtb[*p] & SIOC_DECIMAL)
	    p++;
	s = p - 1;

	frac = *s-- - '0';
	while (*s != '.')
	    frac = frac / 10.0 + *s-- - '0';

	d += frac / 10;
    }

    if ((sio_chtb[*p] & SIOC_E) &&
	(((sio_chtb[*(p + 1)] & SIOC_PLUSMINUS)
	  && (sio_chtb[*(p + 2)] & SIOC_DECIMAL))
	 || (sio_chtb[*(p + 1)] & SIOC_DECIMAL))) {
	int   exp = 0;
	int   neg = 0;
	double m, z;

	if (*++p == '-') {
	    neg = 1;
	    p++;
	}
	else if (*p == '+')
	    p++;

	while (sio_chtb[*p] & SIOC_DECIMAL)
	    exp = 10 * exp + (*p++) - '0';

	m = 1.0;
	z = 10.0;
	while (exp != 0) {
	    if (exp & 1) {
		m *= z;
		exp -= 1;
	    }
	    else {
		z *= z;
		exp >>= 1;
	    }
	}
	if (neg)
	    d /= m;
	else
	    d *= m;
    }

    *pp = p;
    return d;
}

static int
escaped_char(pp)
    UCHAR **pp;
{
    register UCHAR *p = *pp + 1;
    				/* set p and skip over the backslash */

    switch (*p) {
	case 'a':
	    *pp = p + 1;
	    return 7;
	case 'b':
	    *pp = p + 1;
	    return '\b';
	case 'f':
	    *pp = p + 1;
	    return '\f';
	case 'n':
	    *pp = p + 1;
	    return '\n';
	case 'r':
	    *pp = p + 1;
	    return '\r';
	case 't':
	    *pp = p + 1;
	    return '\t';
	case 'v':
	    *pp = p + 1;
	    return '\v';
	case 'x':
	    *pp = p + 1;
	    return hexadecimal(pp);
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	    *pp = p;
	    return octal(pp);
	default:
	    *pp = p + 1;
	    return *p;
    }
}

#define DOUBLEQUOTING 1

static void
quoted_atom(vpTokType, vpTokVal, tpTokVal, pp, lim, buf)
    PWord *vpTokType, *vpTokVal;
    int  *tpTokVal;
    UCHAR **pp;
    UCHAR *lim;
    UCHAR *buf;
{
    register UCHAR *p;
    register int count;
    UCHAR *pmem, *atomstart;
    int   escapefound, eossave;

    p = atomstart = *pp + 1;	/* advance over initial quote */

    count = 0;
    escapefound = 0;

    while (p < lim
#ifndef DOUBLEQUOTING
	   && *p != '\''
#endif /* DOUBLEQUOTING */
	) {
	if (*p == '\\') {
	    if (p + 1 >= lim) {
		count--;
		lim = p;
	    }
	    else if (*(p + 1) == '\n') {
		p += 2;
		count--;
		escapefound = 1;
	    }
	    else {
		pmem = p;
		(void) escaped_char(&pmem);
		p = pmem;
		escapefound = 1;
	    }
	}
#ifdef DOUBLEQUOTING
	else if (*p == '\'') {
	    if (p + 1 == lim) {
		count--;
		lim = p;
	    }
	    else if (*(p + 1) == '\'') {
		p += 2;
		escapefound = 1;
	    }
	    else
		break;		/* break out of the while loop */
	}
#endif /* DOUBLEQUOTING */
	else if (*p == '\n') {
	    (*pp)++;		/* advance over initial quote */
	    *vpTokType = TK_LEXERR;
	    *vpTokVal = SIOL_UNTERM_SYMBOL;
	    *tpTokVal = WTP_INTEGER;
	    return;		/* error: newline in atom */
	}
	else
	    p++;
	count++;
    }

    /*
     * escapefound will be set to 1 if an escaped character was found in
     * the quoted atom.
     *
     * count will have the number of characters to allocate
     */

    eossave = *p;
    *p = 0;

    if (escapefound) {
	register UCHAR *q;

	w_uia_alloc(vpTokVal, tpTokVal, (size_t) count);
	p = atomstart;
	q = (UCHAR *) M_FIRSTUIAWORD(*vpTokVal);
	while (*p) {
	    if (*p == '\\') {
		if (*(p + 1) == '\n') {
		    SIO_LINENUM(buf)++;
		    p += 2;
		}
		else {
		    pmem = p;
		    *q++ = escaped_char(&pmem);
		    p = pmem;
		}
	    }
#ifdef DOUBLEQUOTING
	    else if (*p == '\'') {
		*q++ = *p++;
		p++;		/* skip over second quote */
	    }
#endif /* DOUBLEQUOTING */
	    else
		*q++ = *p++;
	}
	*q = 0;
    }
    else if ( (*vpTokVal = probe_token(atomstart)) )
	*tpTokVal = WTP_SYMBOL;
    else
	w_mk_uia(vpTokVal, tpTokVal, atomstart);

    *p = eossave;
    if (p >= lim) {
	SIO_FLAGS(buf) |= SIOF_INQATOM;
	*vpTokType = TK_LONG_QSYMBOL;
	*pp = p;
    }
    else {
	SIO_FLAGS(buf) &= ~SIOF_INQATOM;
	if (*(p + 1) == '(' /* ) fake out vi */ )
	    *vpTokType = TK_FUNCTOR;
	else
	    *vpTokType = TK_SYMBOL;
	*pp = p + 1;		/* advance over final quote */
    }
}				/* quoted_atom */

static int
quoted_string(vpTokType, vpTokVal, tpTokVal, pp, lim, buf)
    PWord *vpTokType, *vpTokVal;
    int  *tpTokVal;
    UCHAR **pp;
    UCHAR *lim;
    UCHAR *buf;
{
    register UCHAR *p;
    register int c;
    UCHAR *pmem;
    PWord v1 = 0; 		/* = 0 to appease -Wall */
    PWord v2;
    int   t1, t2;
    int ccount = 0;

    p = *pp + 1;		/* advance over initial quote */

    SIO_FLAGS(buf) &= ~SIOF_INSTRING;

    for (;;) {
	/* skip over continuation lines */
	while (p+1 < lim && *p == '\\' && *(p + 1) == '\n') {
	    p += 2;
	    SIO_LINENUM(buf)++;
	}

	if (p >= lim)
	    break;	/* broken string */

	c = *p;		/* fetch next character */

	if (c == '\n') {
	    *pp = p+1;
	    return 0;	/* error: newline in string */
	}
	else if (c == '"') {
#ifdef DOUBLEQUOTING
	    if (p+1 >= lim)
		break;
	    if (*(p+1) == '"')
		p += 2;
#endif /* DOUBLEQUOTING */
	    else {
		*vpTokType = TK_STRING;
		*pp = p + 1;
		if (ccount) 
		    w_install_cdr(v1, TK_NIL, WTP_SYMBOL);
		else {
		    *vpTokVal = TK_NIL;
		    *tpTokVal = WTP_SYMBOL;
		}
		return 1;
	    }
	}
	else if (c == '\\') {
	    pmem = p;
	    c = escaped_char(&pmem);
	    if (pmem >= lim)
		break;
	    p = pmem;
	}
	else
	    p++;
	w_mk_list(&v2, &t2);
	if (ccount)
	    w_install_cdr(v1, v2, t2);
	else {
	    *vpTokVal = v2;
	    *tpTokVal = t2;
	}
	v1 = v2;
	t1 = t2;
	w_install_car(v1, c, WTP_INTEGER);
	ccount++;
    }

    /* We will only get here if we have a long (broken) string */
    SIO_FLAGS(buf) |= SIOF_INSTRING;
    *vpTokType = TK_LONG_STRING;
    *pp = p;
    if (ccount) 
	w_install_cdr(v1, TK_NIL, WTP_SYMBOL);
    else {
	*vpTokVal = TK_NIL;
	*tpTokVal = WTP_SYMBOL;
    }
    return 1;
}

static int
char_constant(pp, lim, inc, endc)
    UCHAR **pp;
    UCHAR *lim;
    int   inc;
    int   endc;
{
    int   val;

    *pp += inc;			/* skip over initial segment */
    if (*pp >= lim)
	return -1;

    val = **pp;
    if (val == '\\')
	val = escaped_char(pp);
    else if (val == '\n')
	return -1;
#ifdef SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS
    else if (val == endc) {
	if (*(*pp + 1) != endc) {
	    (*pp)++;
	    return -1;
	}
	else
	    (*pp) += 2;
    }
#endif /* SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS */
    else
	(*pp)++;

#ifdef	SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS
    if (**pp != endc)
	return -1;
    else {
	(*pp)++;
	return val;
    }
#else
    return val;
#endif /* SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS */
}

static long nt_tokstart;
static long nt_tokend;

/*
 * next_token0: primitive called by pbi_next_token
 */

#define CHECK_FOR_POSSIBLE_SPLIT(p)			\
do {							\
    if ((p) >= lim) {					\
	SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);	\
	SIO_ERRCODE(buf) = SIOE_READ;			\
	return 0;					\
    }							\
} while (0)

static int
next_token0(buf, vpTokType, tpTokType, vpTokVal, tpTokVal)
    UCHAR *buf;
    PWord *vpTokType;
    int  *tpTokType;
    PWord *vpTokVal;
    int  *tpTokVal;
{
    UCHAR *p, *lim, *tokstart;
    int   eossave;
	int ty;
	double dec_val;

    *tpTokType = WTP_SYMBOL;
    if (!skip_layout(buf)) {
	if (SIO_FLAGS(buf) & SIOF_EOF) {
	    SIO_ERRCODE(buf) = SIOE_NORMAL;
	    if (SIO_COMMENT(buf) != 0) {
		*vpTokType = (PWord) TK_LEXERR;
		*vpTokVal = (PWord) SIOL_BADCOMMENT;
		*tpTokVal = WTP_INTEGER;
		SIO_COMMENT(buf) = 0;
	    }
	    else {
		*vpTokType = (PWord) TK_EOF;
		*vpTokVal = (PWord) TK_EOF;
		*tpTokVal = WTP_SYMBOL;
	    }
	    nt_tokstart = nt_tokend = SIO_BUFPOS(buf) + SIO_CPOS(buf);
	    return 1;
	}
	else
	    return 0;
    }

    p = SIO_BUFFER(buf) + SIO_CPOS(buf);
    tokstart = p;
    nt_tokstart = SIO_BUFPOS(buf) + SIO_CPOS(buf);
    lim = SIO_BUFFER(buf) + SIO_LPOS(buf);
    *lim = 0;

    if (SIO_FLAGS(buf) & SIOF_EOF)
	lim++;
    

    if (SIO_FLAGS(buf) & (SIOF_INATOM | SIOF_INSTRING | SIOF_INQATOM)) {
	CHECK_FOR_POSSIBLE_SPLIT(p+1);
	if (SIO_FLAGS(buf) & SIOF_INQATOM) {
	    p--;		/* fake out single quote */
	    quoted_atom(vpTokType, vpTokVal, tpTokVal, &p, lim, buf);
	}
	else if (SIO_FLAGS(buf) & SIOF_INSTRING) {
	    p--;		/* fake out double quote */
	    if (!quoted_string(vpTokType, vpTokVal, tpTokVal, &p, lim, buf)) {
		*vpTokType = TK_LEXERR;
		*vpTokVal = SIOL_UNTERM_STRING;
		*tpTokVal = WTP_INTEGER;
	    }
	    if ((SIO_FLAGS(buf) & SIOF_INSTRING)
	     && *tpTokVal == WTP_SYMBOL
	     && *vpTokVal == TK_NIL) {
		SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);
		SIO_ERRCODE(buf) = SIOE_READ;
		return 0;
	    }
	}
    }
    else
	switch (sio_chtb[*p] & SIOC_TOKENSTART) {
	    case SIOC_SINGLE:
		*tpTokVal = WTP_SYMBOL;
		switch (*p++) {
		    case '(':
			*vpTokType = *vpTokVal = TK_LPAREN;
			break;
		    case ')':
			*vpTokType = *vpTokVal = TK_RPAREN;
			break;
		    case ',':
			*vpTokType = *vpTokVal = TK_COMMA;
			break;
		    case '[':
			*vpTokType = *vpTokVal = TK_LBRAC;
			break;
		    case ']':
			*vpTokType = *vpTokVal = TK_RBRAC;
			break;
		    case '|':
			*vpTokType = *vpTokVal = TK_VBAR;
			break;
		    case '!':
			*vpTokType = *vpTokVal = TK_CUT;
			break;
		    case ';':
			*vpTokType = *vpTokVal = TK_SCOLON;
			break;
		    case '{':
			*vpTokType = *vpTokVal = TK_LCURLY;
			break;
		    case '}':
			*vpTokType = *vpTokVal = TK_RCURLY;
			break;
		    default:	/* This is a error */
			*vpTokType = TK_LEXERR;
			*vpTokVal = SIOL_INTERNAL_1;
			*tpTokVal = WTP_INTEGER;
			break;

		}
		break;
	    case SIOC_GRAPHIC:
		while (sio_chtb[*++p] & SIOC_GRAPHIC) ;
		goto makesym;
	    case SIOC_LOWERCASE:
		while (sio_chtb[*++p] & SIOC_ALPHANUMERIC) ;
makesym:
		CHECK_FOR_POSSIBLE_SPLIT(p);
		eossave = *p;
		*p = 0;
		*vpTokVal = find_token(tokstart);
		*tpTokVal = WTP_SYMBOL;
		*p = eossave;
		if (*vpTokVal == TK_DOT &&
		    ((sio_chtb[eossave] & SIOC_WHITESPACE) ||
		     eossave == '%' ||
		     (eossave == '/' && *(p + 1) == '*')))
		    *vpTokType = TK_FULLSTOP;
		else if (eossave == '(')
		    *vpTokType = TK_FUNCTOR;
		else
		    *vpTokType = TK_SYMBOL;
		break;
	    case SIOC_UPPERCASE:
		while (sio_chtb[*++p] & SIOC_ALPHANUMERIC) ;
		CHECK_FOR_POSSIBLE_SPLIT(p);
		eossave = *p;
		*p = 0;
		if ( (*vpTokVal = probe_token(tokstart)) )
		    *tpTokVal = WTP_SYMBOL;
		else
		    w_mk_uia(vpTokVal, tpTokVal, tokstart);
		*p = eossave;
		*vpTokType = TK_VAR;
		break;
	    case SIOC_DECIMAL:
		*vpTokType = TK_FLOAT;
		if (*p == '0') {
		    if (*(p + 1) == 'x' && (sio_chtb[*(p + 2)] & SIOC_HEXADECIMAL)) {
			p += 2;
			make_number(vpTokVal, tpTokVal, (double) hexadecimal(&p)); 
		    }
		    else if (*(p + 1) == 'o' && (sio_chtb[*(p + 2)] & SIOC_OCTAL)) {
			p += 2;
			make_number(vpTokVal, tpTokVal, (double) octal(&p)); 
		    }
#ifdef	SIO_ZERO_QUOTE_FOR_CHAR_CONSTS
		    else if (*(p + 1) == '\'') {
			int   c;

			if ((c = char_constant(&p, lim, 2, '\'')) == -1) {
			    *vpTokType = TK_LEXERR;
			    *vpTokVal = SIOL_TOOLONG_CHARCONST;
			    *tpTokVal = WTP_INTEGER;
			}
			else {
			    *vpTokVal = c;
			    *tpTokVal = WTP_INTEGER;
			}
		    }
#endif /* SIO_ZERO_QUOTE_FOR_CHAR_CONSTS */
		    else
			{
				dec_val = decimal(&p, &ty);
				make_numberx(vpTokVal, tpTokVal, dec_val,ty);
			}
		}
		else
		{
			dec_val = decimal(&p, &ty);
			make_numberx(vpTokVal, tpTokVal, dec_val,ty);
		}
		CHECK_FOR_POSSIBLE_SPLIT(*p ? p+1 : p);
		break;
	    case SIOC_SPECIAL:
		if (*p == '\'')
		    quoted_atom(vpTokType, vpTokVal, tpTokVal, &p, lim, buf);
#ifdef	SIO_BACKQUOTE_FOR_CHAR_CONSTS
		else if (*p == '`') {
		    int   c;

		    if ((c = char_constant(&p, lim, 1, '`')) == -1) {
			*vpTokType = TK_LEXERR;
			*vpTokVal = SIOL_TOOLONG_CHARCONST;
			*tpTokVal = WTP_INTEGER;
		    }
		    else {
			*vpTokVal = c;
			*tpTokVal = WTP_INTEGER;
			*vpTokType = TK_FLOAT;
		    }
		}
#endif /* SIO_BACKQUOTE_FOR_CHAR_CONSTS */
		else if (*p == '\"') {
		    CHECK_FOR_POSSIBLE_SPLIT(p+1);
		    if (!quoted_string(vpTokType,vpTokVal, tpTokVal, 
				      &p, lim, buf)) {
			*vpTokType = TK_LEXERR;
			*vpTokVal = SIOL_UNTERM_STRING;
			*tpTokVal = WTP_INTEGER;
		    }
		    if ((SIO_FLAGS(buf) & SIOF_INSTRING)
		     && *tpTokVal == WTP_SYMBOL
		     && *vpTokVal == TK_NIL) {
			SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);
			SIO_ERRCODE(buf) = SIOE_READ;
			SIO_FLAGS(buf) &= ~SIOF_INSTRING;
			return 0;
		    }
		}
		else {
		    *vpTokType = TK_LEXERR;
		    *vpTokVal = SIOL_INTERNAL_1;
		    *tpTokVal = WTP_INTEGER;
		}
		break;
	}

    SIO_CPOS(buf) = p - SIO_BUFFER(buf);
    SIO_COLUMN(buf) += (p - tokstart);
    nt_tokend = SIO_BUFPOS(buf) + SIO_CPOS(buf);

    return 1;
}


/*
 * sio_next_token(Stream,TokType,TokVal)
 */

int
sio_next_token()
{
    PWord v1, v2, v3, vTokType, vTokVal;
    int   t1, t2, t3, tTokType, tTokVal;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    if (next_token0(buf, &vTokType, &tTokType, &vTokVal, &tTokVal) &&
	w_unify(v2, t2, vTokType, tTokType) &&
	w_unify(v3, t3, vTokVal, tTokVal))
	SUCCEED;
    else
	FAIL;
}



/*
 * sio_next_tokens(Stream,TokList,TokListTail)
 */

int
sio_next_tokens()
{
    PWord v1, v2, v3, vTokType, vTokVal, L, T;
    int   t1, t2, t3, tTokType, tTokVal, linenum0, preprocessing;
    int   cpos0 = 0;	/* = 0 to appease -Wall */
    PWord a2, a3;
    register PWord *H, *S;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    w_install(&a2, v2, t2);
    w_install(&a3, v3, t3);

    preprocessing = (SIO_FLAGS(buf) & SIOF_PREPROC);
    linenum0 = SIO_ELINENUM(buf);	/* get line number of previous token */
    if (preprocessing) {
	cpos0 = SIO_CPOS(buf);
    }
    if (!(next_token0(buf, &vTokType, &tTokType, &vTokVal, &tTokVal) ||
	  (preprocessing && linenum0 != SIO_LINENUM(buf)))) {
	if (w_unify(v2, t2, v3, t3))
	    SUCCEED;
	else
	    FAIL;
    }
    else {
	S = &L;

	for (;;) {
	    if (SIO_LINENUM(buf) != linenum0) {
		if (preprocessing) {
		    SIO_CPOS(buf) = cpos0;
		    SIO_LINENUM(buf) = linenum0;
		    /* We will rescan comments if we walked into one, so turn
		     * off any comment indications.
		     */
		    SIO_FLAGS(buf) &= ~SIOF_LCOMMENT;
		    SIO_COMMENT(buf) = 0;
		    tTokType = tTokVal = WTP_SYMBOL;
		    vTokType = vTokVal = TK_EOLN;
		    nt_tokend = nt_tokstart;
		    nt_tokstart--;
		    preprocessing = 0;

		}
		else {
		    H = wm_H;
		    T = MMK_STRUCTURE((PWord) H);
		    *H++ = MMK_FUNCTOR(TK_LINEINFO, 3);
		    *H++ = *(((PWord *) v1) + SIO_SD_STREAM_NAME);
		    *H++ = MMK_INT(linenum0);
		    *H++ = MMK_INT(0);	/* put stream position here later */
		    *S = MMK_LIST((PWord) H);
		    *H++ = T;
		    S = H++;
		    wm_H = H;
		    linenum0 = SIO_LINENUM(buf);
		}
	    }
	    if (tTokVal == WTP_SYMBOL && vTokVal == TK_POUND
		&& SIO_COLUMN(buf) == 1) {
		vTokType = TK_PREPROC;
		preprocessing = 1;
	    }

	    if (preprocessing &&
		vTokType == TK_SYMBOL &&
		vTokVal == TK_BACKSLASH &&
		SIO_BUFFER(buf)[SIO_CPOS(buf)] == '\n') {
		SIO_CPOS(buf)++;
		SIO_LINENUM(buf)++;

	    }
	    else {
		H = wm_H;
		T = MMK_STRUCTURE((PWord) H);
		*H++ = MMK_FUNCTOR(vTokType, 3);
		if (vTokType == TK_EOF)
		    w_install(H, v1, t1);
		else
		    w_install(H, vTokVal, tTokVal);
		H++;
		*H++ = MMK_INT(nt_tokstart);
		*H++ = MMK_INT(nt_tokend);

		*S = MMK_LIST((PWord) H);
		*H++ = T;
		S = H++;
		wm_H = H;
		if (vTokType == TK_EOF ||
		    vTokType == TK_LEXERR ||
		    vTokType == TK_EOLN ||
		    (vTokType == TK_FULLSTOP && !preprocessing)) {

		    H = wm_H;
		    T = MMK_STRUCTURE((PWord) H);
		    *H++ = MMK_FUNCTOR(TK_LINEINFO, 3);
		    *H++ = *((PWord *) (v1) + SIO_SD_STREAM_NAME);
		    *H++ = MMK_INT(SIO_LINENUM(buf));
		    *H++ = MMK_INT(0);	/* put stream position here later */
		    *S = MMK_LIST((PWord) H);
		    *H++ = T;
		    S = H++;
		    wm_H = H;

		    *S = T = MMK_SYM(TK_NIL);
		    break;
		}
	    }

	    if (preprocessing) {
		cpos0 = SIO_CPOS(buf);
		linenum0 = SIO_LINENUM(buf);
	    }
	    if (!(next_token0(buf, &vTokType, &tTokType, &vTokVal, &tTokVal) ||
		  (preprocessing && linenum0 != SIO_LINENUM(buf)))) {
		*S = T = MMK_VAR((PWord) S);
		break;
	    }

	}

	if (preprocessing)
	    SIO_FLAGS(buf) |= SIOF_PREPROC;
	else
	    SIO_FLAGS(buf) &= ~SIOF_PREPROC;

	SIO_ELINENUM(buf) = linenum0;

	if (_w_unify(a2, L) && _w_unify(a3, T))
	    SUCCEED;
	else
	    FAIL;
    }
}


/*
 * sio_skip_layout(Stream)
 */

int
sio_skip_layout()
{
    PWord v1;
    int   t1;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    if (!skip_layout(buf)) {
	if (SIO_FLAGS(buf) & SIOF_EOF) {
	    SIO_ERRCODE(buf) = SIOE_NORMAL;
	    SUCCEED;
	}
	else
	    FAIL;
    }
    else
	SUCCEED;
}


/*
 * sio_linenumber(Stream,LineNumber)
 *
 * Unifies LineNumber with the current line number for the stream Stream.
 */

int
sio_linenumber()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (w_unify(v2, t2, SIO_LINENUM(buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

/*
 * sio_get_number(Stream,InputType,Number)
 */

int
sio_get_number()
{
    PWord v1, v2, v3, vNum;
    int   t1, t2, t3, tNum;
    UCHAR *buf;
    UCHAR *lim, *endofval;
    register UCHAR *s, *p;
    UCHAR  byteval;
    short shortval;
    long  longval;
    float floatval;
    double doubleval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
	SIO_ERRCODE(buf) = SIOE_ILLREAD;
	FAIL;
    }

    if (!xform_uia(&v2, &t2)) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    switch (v2) {
	case TK_BYTE:
	case TK_UBYTE:
	case TK_CHAR:
	case TK_UCHAR:
	    s = (UCHAR *) &byteval;
	    endofval = s + sizeof (byteval);
	    break;
	case TK_SHORT:
	case TK_USHORT:
	case TK_RSHORT:
	case TK_RUSHORT:
	    s = (UCHAR *) &shortval;
	    endofval = s + sizeof (shortval);
	    break;
	case TK_LONG:
	case TK_ULONG:
	case TK_INT:
	case TK_UINT:
	    s = (UCHAR *) &longval;
	    endofval = s + sizeof (longval);
	    break;
	case TK_FLOAT:
	    s = (UCHAR *) &floatval;
	    endofval = s + sizeof (floatval);
	    break;
	case TK_DOUBLE:
	    s = (UCHAR *) &doubleval;
	    endofval = s + sizeof (doubleval);
	    break;
	default:
	    SIO_ERRCODE(buf) = SIOE_INARG;
	    FAIL;
    }


    p = SIO_BUFFER(buf) + SIO_CPOS(buf);
    lim = SIO_BUFFER(buf) + SIO_LPOS(buf);

    if (endofval - s > lim - p) {
	if (SIO_FLAGS(buf) & SIOF_EOF)
	    SIO_ERRCODE(buf) = SIOE_PARTNUM;
	else
	    SIO_ERRCODE(buf) = SIOE_READ;
	FAIL;
    }

    while (s < endofval && p < lim)
	*s++ = *p++;		/* copy the value */

    SIO_CPOS(buf) = p - SIO_BUFFER(buf);

    switch (v2) {
	case TK_BYTE:
	case TK_CHAR:
	    vNum = (PWord) byteval;
	    tNum = WTP_INTEGER;
	    break;
	case TK_UBYTE:
	case TK_UCHAR:
	    vNum = (PWord) (UCHAR) byteval;
	    tNum = WTP_INTEGER;
	    break;
	case TK_SHORT:
	    vNum = (PWord) shortval;
	    tNum = WTP_INTEGER;
	    break;
	case TK_USHORT:
	    vNum = (PWord) (unsigned short) shortval;
	    tNum = WTP_INTEGER;
	    break;
	case TK_RSHORT:
	    vNum = (PWord) (short) (((unsigned short) shortval << 8) |
				    (((unsigned short) shortval) >> 8));
	    tNum = WTP_INTEGER;
	    break;
	case TK_RUSHORT:
	    vNum = (PWord) (unsigned short) (((unsigned short) shortval << 8) |
					(((unsigned short) shortval) >> 8));
	    tNum = WTP_INTEGER;
	    break;
	case TK_LONG:
	case TK_INT:
	    make_number(&vNum, &tNum, (double) longval);
	    break;
	case TK_ULONG:
	case TK_UINT:
	    make_number(&vNum, &tNum, (double) (unsigned long) longval);
	    break;
	case TK_FLOAT:
	    make_numberx(&vNum, &tNum, (double) floatval, WTP_DOUBLE);
	    break;
	case TK_DOUBLE:
	    make_numberx(&vNum, &tNum, doubleval, WTP_DOUBLE);
	    break;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    if (w_unify(v3, t3, vNum, tNum))
	SUCCEED;
    else
	FAIL;
}


/*
 * sio_put_atom(Stream,Atom)
 */

int
sio_put_atom()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *buf, *atom;
    register UCHAR *a, *b, *l;
    int   newlineseen;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_WRITE)) {
	SIO_ERRCODE(buf) = SIOE_ILLWRITE;
	FAIL;
    }

    if ((SIO_FLAGS(buf) & (SIOF_READ | SIOF_EOF)) == SIOF_READ &&
	SIO_LPOS(buf) == 0) {
	SIO_ERRCODE(buf) = SIOE_READ;
	FAIL;
    }

    if (t2 == WTP_UIA)
	atom = (UCHAR *) M_FIRSTUIAWORD(v2);
    else if (t2 == WTP_SYMBOL)
	atom = TOKNAME(v2);
    else {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    b = SIO_BUFFER(buf) + SIO_CPOS(buf);
    l = SIO_BUFFER(buf) + SIO_BFSIZE(buf);
    a = atom + SIO_AUX(buf);
    newlineseen = 0;

    while (b < l && *a) {
	if (*a == '\n')
	    newlineseen++;
	*b++ = *a++;
    }

    if (*a)
	SIO_AUX(buf) = a - atom;
    else
	SIO_AUX(buf) = 0;

    SIO_CPOS(buf) = b - SIO_BUFFER(buf);
    if (SIO_CPOS(buf) > SIO_LPOS(buf))
	SIO_LPOS(buf) = SIO_CPOS(buf);
    SIO_FLAGS(buf) |= SIOF_DIRTY;

    if ((SIO_FLAGS(buf) & SIOF_BBYTE) ||
	((SIO_FLAGS(buf) & SIOF_BLINE) && newlineseen) ||
	(SIO_CPOS(buf) == SIO_BFSIZE(buf))) {
	SIO_ERRCODE(buf) = SIOE_WRITE;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;

}

/*
 * sio_put_number(Stream,OutType,Number)
 */

int
sio_put_number()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *buf, *sval;
    register UCHAR *s, *b, *l, *endofval;

    UCHAR  byteval;
    short shortval;
    long  longval;
    unsigned long ulongval;
    float floatval;
    double doubleval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_WRITE)) {
	SIO_ERRCODE(buf) = SIOE_ILLWRITE;
	FAIL;
    }

    if ((SIO_FLAGS(buf) & (SIOF_READ | SIOF_EOF)) == SIOF_READ &&
	SIO_LPOS(buf) == 0) {
	SIO_ERRCODE(buf) = SIOE_READ;
	FAIL;
    }

    if (!xform_uia(&v2, &t2)) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    switch (v2) {
	case TK_BYTE:
	case TK_UBYTE:
	case TK_CHAR:
	case TK_UCHAR:
	    if (!getlong(&longval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    if (v2 == TK_BYTE)
		byteval = (UCHAR) longval;
	    else
		*(UCHAR *) &byteval = (UCHAR) longval;
	    sval = (UCHAR *) &byteval;
	    endofval = sval + sizeof (byteval);
	    break;
	case TK_SHORT:
	case TK_USHORT:
	    if (!getlong(&longval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    if (v2 == TK_SHORT)
		shortval = (short) longval;
	    else
		*(unsigned short *) &shortval = (unsigned short) longval;
	    sval = (UCHAR *) &shortval;
	    endofval = sval + sizeof (shortval);
	    break;
	case TK_INT:
	case TK_LONG:
	    if (!getlong(&longval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    sval = (UCHAR *) &longval;
	    endofval = sval + sizeof (longval);
	    break;
	case TK_ULONG:
	case TK_UINT:
	    if (!getdouble(&doubleval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    ulongval = (unsigned long) doubleval;
	    sval = (UCHAR *) &ulongval;
	    endofval = sval + sizeof (ulongval);
	    break;
	case TK_FLOAT:
	    if (!getdouble(&doubleval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    floatval = (float) doubleval;
	    sval = (UCHAR *) &floatval;
	    endofval = sval + sizeof (floatval);
	    break;
	case TK_DOUBLE:
	    if (!getdouble(&doubleval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    sval = (UCHAR *) &doubleval;
	    endofval = sval + sizeof (doubleval);
	    break;
	default:
	    SIO_ERRCODE(buf) = SIOE_INARG;
	    FAIL;
	    break;
    }

    b = SIO_BUFFER(buf) + SIO_CPOS(buf);
    l = SIO_BUFFER(buf) + SIO_BFSIZE(buf);
    s = sval + SIO_AUX(buf);

    while (b < l && s < endofval)
	*b++ = *s++;

    if (s < endofval)
	SIO_AUX(buf) = s - sval;
    else
	SIO_AUX(buf) = 0;

    SIO_CPOS(buf) = b - SIO_BUFFER(buf);
    if (SIO_CPOS(buf) > SIO_LPOS(buf))
	SIO_LPOS(buf) = SIO_CPOS(buf);
    SIO_FLAGS(buf) |= SIOF_DIRTY;

    if ((SIO_FLAGS(buf) & SIOF_BBYTE) ||
	(SIO_FLAGS(buf) & SIOF_BLINE) ||
	(SIO_CPOS(buf) == SIO_BFSIZE(buf))) {
	SIO_ERRCODE(buf) = SIOE_WRITE;
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;

}


/*
 * sio_qatom(InAtom,OutAtom,Length)
 */

int
sio_qatom()
{
    PWord v1, v2, v3, vTokVal;
    int   t1, t2, t3, tTokVal;
    int   count, bscnt;
    int   mask;
    UCHAR *atom;
    register UCHAR *a, *b;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA)
	atom = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (t1 == WTP_SYMBOL)
	atom = (UCHAR *) TOKNAME(v1);
    else
	FAIL;

    if (sio_chtb[*atom] & SIOC_LOWERCASE)
	mask = SIOC_ALPHANUMERIC;
    else if (sio_chtb[*atom] & SIOC_GRAPHIC)
	mask = SIOC_GRAPHIC;
    else
	mask = 0;

    a = atom;
    count = 0;
    bscnt = 0;

    while (*a && (sio_chtb[*a] & mask)) {
	if (*a == '\\')
	    bscnt++;
	a++;
	count++;
    }

    /* Handle the special case atoms */
    if (strcmp((char *)atom, "!") == 0) {
	count = 1;
	mask = 1;
    }
    else if (strcmp((char *)atom, "[]") == 0) {
	count = 2;
	mask = 1;
    }
    else if (*a || a == atom) {
	mask = 0;
    }

    if (mask) {
	if (w_unify(v2, t2, v1, t1) && w_unify(v3, t3, count, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }

    count += bscnt;		/* double space for initial backspace
				 * characters
				 */

    while (*a) {
	if (sio_chtb[*a] & SIOC_ESCCHAR)
	    count += 2;
	else if (*a != ' ' && (sio_chtb[*a] & SIOC_WHITESPACE))
	    count += 4;
	else
	    count++;
	a++;
    }

    count += 2;			/* add two characters for the quote
				 * characters
				 */

    w_uia_alloc(&vTokVal, &tTokVal, (size_t)count);
    b = (UCHAR *) M_FIRSTUIAWORD(vTokVal);

    *b++ = '\'';		/* put down the initial quote */
    a = atom;
    while (*a) {
	if (sio_chtb[*a] & SIOC_ESCCHAR) {
	    *b++ = '\\';	/* put down the backspace */
	    switch (*a) {
		case 7:
		    *b++ = 'a';
		    break;
		case '\b':
		    *b++ = 'b';
		    break;
		case '\f':
		    *b++ = 'f';
		    break;
		case '\n':
		    *b++ = 'n';
		    break;
		case '\r':
		    *b++ = 'r';
		    break;
		case '\t':
		    *b++ = 't';
		    break;
		case '\v':
		    *b++ = 'v';
		    break;
		default:
		    *b++ = *a;
		    break;
	    }
	    a++;
	}
	else if (*a != ' ' && (sio_chtb[*a] & SIOC_WHITESPACE)) {
	    *b++ = '\\';
	    *b++ = ((*a >> 6) & 7) + '0';
	    *b++ = ((*a >> 3) & 7) + '0';
	    *b++ = (*a++ & 7) + '0';
	}
	else
	    *b++ = *a++;
    }
    *b++ = '\'';		/* put down final quote */
    *b = 0;

    if (w_unify(v2, t2, vTokVal, tTokVal) && w_unify(v3, t3, count, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;

}

/*
 * sio_var_to_atom(Var,Atom)
 *
 * Used to map variables to atoms suitable for writing out.
 */

int
sio_var_to_atom()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR  buf[20];


    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_UNBOUND)
	FAIL;

    sprintf((char *)buf, "_%lu", (long) (((PWord *) v1) - wm_heapbase));
    w_mk_uia(&v3, &t3, buf);
    if (w_unify(v2, t2, v3, t3))
	SUCCEED;
    else
	FAIL;

}

/*
 * sio_lettervar(Int,Atom)
 *
 * Used to get variable names to print for numbervar and lettervar options
 */

int
sio_lettervar()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR  buf[20];

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || v1 < 0)
	FAIL;

    if (v1 <= 25) {
	w_uia_alloc(&v3, &t3, 1);
	*(UCHAR *) M_FIRSTUIAWORD(v3) = 'A' + v1;
	*(((UCHAR *) M_FIRSTUIAWORD(v3)) + 1) = 0;
    }
    else {
	sprintf((char *)buf, "%c%d", (int)('A' + v1 % 26),(int)(v1 / 26));
	w_mk_uia(&v3, &t3, buf);
    }

    if (w_unify(v2, t2, v3, t3))
	SUCCEED;
    else
	FAIL;
}


/*
 * The following defines and function are used by sio_sprintf below
 */

#define FMT_NONE 0
#define FMT_INT 1
#define FMT_DBL 2
#define FMT_STR 3

static int
format_type(s)
    register UCHAR *s;
{
    for (;;) {
	while (*s && *s != '%')
	    s++;
	if (*s && *(s + 1) == '%')
	    s += 2;
	else
	    break;
    }

    if (*s == '\0')
	return FMT_NONE;


    for (s++;; s++) {
	switch (*s) {
	    case 0:
		return FMT_NONE;
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
	    case 'c':
		return FMT_INT;
	    case 'f':
	    case 'e':
	    case 'E':
	    case 'g':
	    case 'G':
		return FMT_DBL;
	    case 's':
		return FMT_STR;
	    default:;		/* loop */
	}
    }
}

/*
 * sio_sprintf(Format,Arg,Buffer,Length)
 */

int
sio_sprintf()
{
    PWord v1, v2, v3, v4, v;
    int   t1, t2, t3, t4, t;
#ifndef DoubleType
    PWord functor;
    int arity;
#endif
    UCHAR *fmt;
    UCHAR *buf;
    double dblval;
    int   fmt_type;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (!getstring(&fmt, v1, t1))
	FAIL;

    buf = (UCHAR *) (wm_H + 1);
    fmt_type = format_type(fmt);

    switch (t2) {
	case WTP_SYMBOL:
	    sprintf((char *)buf, (char *)fmt, TOKNAME(v2));
	    break;
	case WTP_UIA:
	    sprintf((char *)buf, (char *)fmt, (UCHAR *) M_FIRSTUIAWORD(v2));
	    break;
	case WTP_INTEGER:
	    if (fmt_type == FMT_DBL)
		sprintf((char *)buf, (char *)fmt, (double) v2);
	    else if (fmt_type == FMT_INT)
		sprintf((char *)buf, (char *)fmt, v2);
	    else
		sprintf((char *)buf, "?");
	    break;
#ifndef DoubleType
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v2);
	    w_get_functor(&functor, v2);
	    if (arity == 4 && functor == TK_DDOUBLE) {
		int i;
		for (i = 0; i < 4; i++) {
		    w_get_argn(&v, &t, v2, i + 1);
		    *(((short *) &dblval) + i) = (short) v;
		}
	    }
	    else
		FAIL;
	    if (fmt_type == FMT_DBL)
		sprintf((char *)buf, (char *)fmt, dblval);
	    else if (fmt_type == FMT_INT)
		sprintf((char *)buf, (char *)fmt, (int) dblval);
	    else
		sprintf((char *)buf, "?");
	    break;
#else
	case WTP_DOUBLE:
	    w_get_double(&dblval, v2);
	    if (fmt_type == FMT_DBL)
		sprintf((char *)buf, (char *)fmt, dblval);
	    else if (fmt_type == FMT_INT)
		sprintf((char *)buf, (char *)fmt, (int) dblval);
	    else
		sprintf((char *)buf, "?");
	    break;
#endif
	default:
	    FAIL;
    }

    w_mk_uia_in_place(&v, &t, buf);

    if (w_unify(v3, t3, v, t) 
     && w_unify(v4, t4, (PWord) strlen((char *)buf), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

/*
 * sio_isgraphicatom(Atom) will succeed iff Atom contains only graphic
 * characters.
 */

int
sio_isgraphicatom()
{
    PWord v1;
    int   t1;
    UCHAR *atom;

    w_get_An(&v1, &t1, 1);

    if (t1 == WTP_UIA)
	atom = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (t1 == WTP_SYMBOL)
	atom = TOKNAME(v1);
    else
	FAIL;

    while (*atom) {
	if (sio_chtb[*atom] & SIOC_GRAPHIC)
	    atom++;
	else
	    FAIL;
    }
    SUCCEED;
}

#ifdef SysVIPC
/*  for debugging the IPC message queue stuff */
void
display_queue(msqid)
    int   msqid;
{
    struct msqid_ds buffr;

    msgctl(msqid, IPC_STAT, &buffr);
    printf("IPC_STAT: q=%d num_msgs=%ld num_bytes=%ld lastsnd_time=%ld\n",
	   msqid, (long)buffr.msg_qnum, (long)buffr.msg_cbytes,
	   buffr.msg_stime);
}

/*
 * msgctl(Msqid, Cmd, Perms, Info)
 *
 * Called from Prolog to:
 *              1)      Remove queue Msqid from (external) system;
 *              2)      Get info about external settings of queue Msqid;
 *              3)      Change external settings for queue Msqid.
 *
 * Which action is carried out depends on Cmd
 * 	Prolog atom             Passed from Prolog      C value for msgctl
 *      ----------              ------------------      -----------------
 *      ipc_rmid                0                       IPC_RMID
 *      ipc_stat                1                       IPC_STAT
 *      ipc_set                 2                       IPC_SET
 *
 * Perms is a structure of the form
 *	perms(UID,GID,CUID,CGID,MODE,KEY)
 * where all entries are (Prolog) integers corresponding to entries
 * in the ipc_perm structure;
 *
 * Info is a structure of the form
 * 	ipc_info(NumBytes,NumMess,MaxBytes,LSPID,LRPID,STIME,RTIME,CTIME)
 * where all entries are (Prolog) integers, as follows (corresponding
 * to entries in the msqid_ds structure):
 *
 *	Prolog     C type      msqid_ds entry   Description
 *      -----      -----       -------------    -----------
 *      NumBytes   ushort      msg_cbytes;      current # bytes on q
 *      NumMess    ushort      msg_qnum;        # of messages on q
 *      MaxBytes   ushort      msg_qbytes;      max # of bytes on q
 *      LSPID      ushort      msg_lspid;       pid of last msgsnd
 *      LRPID      ushort      msg_lrpid;       pid of last msgrcv
 *      STIME      time_t      msg_stime;       last msgsnd time
 *      RTIME      time_t      msg_rtime;       last msgrcv time
 *      CTIME      time_t      msg_ctime;       last change time
 *
 */

int
pbi_msgctl()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   retrn = 0;
    struct msqid_ds msq_desc;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((t1 != WTP_INTEGER) || (t2 != WTP_INTEGER) ||
	!((t3 == WTP_UNBOUND) || (t3 == WTP_STRUCTURE)))
	FAIL;

    printf("pbi_msgctl: v1=%d IPC_RMID=%d\n", (int) v1, IPC_RMID);
    switch ((int) v2) {
	case 0:
	    retrn = msgctl((int) v1, IPC_RMID, &msq_desc);
	    break;
	case 1:
	    printf("pbi_msgctl: IPC_STAT not yet implemented\n");
	    break;
	case 2:
	    printf("pbi_msgctl: IPC_SET not yet implemented\n");
	    break;
    }

    if (retrn < 0)
	FAIL;
    else
	SUCCEED;
}
#endif /* SysVIPC */


/*-----------------------------------------------------------------------------*
 | sio_readln(Stream,Line,EOLNFlag)
 |
 | Stream is the input argument.  It should contain an open stream.
 | Line will be unified with a UIA containing as much of the current
 |      line as could be read. The end of line character(s) are not returned
 |      in this buffer.
 | EOLNFlag will take on one of the following values.
 |      0 if the end of line was not read.
 |      1 if the end of line was read.
 |
 | Note: The following code will need to be ifdef'd in order to accomodate
 | other systems.  Details like different end of line characters and number
 | of end of line characters need to be addressed.
 *-----------------------------------------------------------------------------*/

int
sio_readln()
{
    PWord v1, v2, v3, vBuf;
    int   t1, t2, t3, tBuf;
    UCHAR *buf;
    int   cpos, lpos, wpos;
    int   eoln_read;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
		FAIL;

    if (!(SIO_FLAGS(buf) & SIOF_READ)) {
		SIO_ERRCODE(buf) = SIOE_ILLREAD;
		FAIL;
    }

    if (t2 != WTP_UNBOUND || t3 != WTP_UNBOUND) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
    }

    cpos = wpos = SIO_CPOS(buf);
    lpos = SIO_LPOS(buf);
    if (cpos >= lpos) {
		SIO_ERRCODE(buf) = SIOE_READ;
		FAIL;
    }

		/* read either DOS(\r\n) or UNIX(\n) or Mac(\r) eolns */
    while (wpos < lpos && 
			SIO_BUFFER(buf)[wpos] != '\r' &&
			SIO_BUFFER(buf)[wpos] != '\n' )
		wpos++;

    if (wpos < lpos) {		/* end of line read */
		if ((SIO_BUFFER(buf)[wpos] == '\r')  &&
			  ( SIO_BUFFER(buf)[wpos+1] == '\n') )
			{
				SIO_BUFFER(buf)[wpos] = 0;
				wpos++;
			}
		SIO_BUFFER(buf)[wpos] = 0;
		w_mk_uia(&vBuf, &tBuf, SIO_BUFFER(buf) + cpos);
		SIO_BUFFER(buf)[wpos] = '\n';
		SIO_CPOS(buf) = wpos + 1;
		SIO_LINENUM(buf) += 1;
		SIO_COLUMN(buf) = 0;
		eoln_read = 1;
    }
    else {					/* end of line not read */
		SIO_BUFFER(buf)[lpos] = 0;
		w_mk_uia(&vBuf, &tBuf, SIO_BUFFER(buf) + cpos);
		SIO_CPOS(buf) = lpos;
		eoln_read = 0;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (w_unify(v2, t2, vBuf, tBuf) && w_unify(v3, t3, eoln_read, WTP_INTEGER))
		SUCCEED;
    else
		FAIL;
}

/*
 * sio_position_in_line/3
 *
 * sio_position_in_line(Stream,Count,Pos)
 *
 * This function will examine the contents of the stream buffer backward
 * from the current position and return two values.  The first value is
 * the number of characters comprising the current line.  The second
 * value will be the the number of spaces one would need to write out
 * to position a cursor at the same position on the screen from the
 * start of the line.  This second value is necessary due to the
 * treatment of tab characters on most displays.
 *
 */

int
sio_position_in_line()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *buf, *bufstart, *bufend;
    register UCHAR *p;
    int   count, pos;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    bufstart = SIO_BUFFER(buf);
    bufend = SIO_BUFFER(buf) + SIO_CPOS(buf);

    for (p = bufend; p >= bufstart && *p != '\n'; p--) ;

    /* p now points to the start of the line */

    count = bufend - p;

    for (pos = 0; p < bufend; p++)
	if (*p == '\t')
	    pos = pos + (8 - pos % 8);
	else if (*p >= ' ')
	    pos++;

    if (w_unify(v2, t2, count, WTP_INTEGER) && w_unify(v3, t3, pos, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}
