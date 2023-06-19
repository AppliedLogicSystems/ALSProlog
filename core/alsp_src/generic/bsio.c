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
 |			  various bugs in Unix emulation libraries
 | 12/11/95 - C. Houpt -- Added end-of-line type system.
 |
 | Configuration parameters (defines, in aconfig.h or mconfig.h):
 |
 |  SysVIPC             -- Unix System V IPC message queues
 |  SSBQ                -- Software Services Backplane Queues
 |  HAVE_SOCKET         -- sockets (should include Berkeley, DOS/FTP, etc)
 |   Subsidiary params for sockets:
 |     BERKELEY_SOCKETS -- The Berkely socket API found on various flavors of unix,
 |                         MacOS and DOS/Windows.
 |     PCFTP_SOCKETS    -- The PC-FTP TCP/IP sockets library found on DOS.
 *=======================================================================*/
#include "defs.h"
#include <math.h>
#include "cinterf.h"
#ifndef MSWin32
#include "linenoise.h"
#endif
#include "fpbasis.h"

#include "new_alspi.h"
#ifdef CW_PLUGIN
#include <DropInCompilerLinker.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <limits.h>
#ifdef __LP64__
#include <stdint.h>
#endif

#ifdef PURE_ANSI
#define EINTR	0
#endif /* PURE_ANSI */

#ifdef MacOS
    #ifdef HAVE_GUSI
	#include <GUSI.h>
	#include <sys/errno.h>
    #else
	#include <unix.h>
	#include <errno.h>
	#define EINTR 4 /* MetroWerks does not define the EINTR error code. */
    #endif
#else
#include <errno.h>
#endif /* MacOS */

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

#ifdef MSWin32
#include "fswin32.h"
#endif

#ifdef SysVIPC		/* For System V Message Queues: */
#include <sys/ipc.h>
#include <sys/msg.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif


/* These should be in sys/ipc.h or sys/msg.h, but some systems have really
 * lame include files.
 */
extern	key_t	ftok		( const char *, int );
extern	int	msgget		( key_t, int );
extern	int	msgsnd		( int, const void *, size_t, int );
extern	int	msgrcv		( int, void *, size_t, long, int );
extern	int	msgctl		( int, int, ... );

#endif /* SysVIPC */

#ifdef SSBQ		/* For Software Services Backplane Queues: */
/* #include "ipc_interface.h"  */
#endif /* SSBQ */

#ifdef HAVE_SOCKET		/* For sockets: */

    #if defined(BERKELEY_SOCKETS)

	#if defined(UNIX)
	    #include <sys/socket.h>
	    #include <sys/time.h>
	    #include <netinet/in.h>
	    #include <arpa/inet.h>
	    #include <netdb.h>
	    #ifndef MISSING_UNIX_DOMAIN_SOCKETS
	       #include <sys/un.h>
	    #endif /* MISSING_UNIX_DOMAIN_SOCKETS */
	    #ifdef UNIX_IRIX
            /* Required for definition of bzero(). */
               #include <bstring.h>
            #endif
	#elif defined(MacOS) && defined(HAVE_GUSI)
	    #include <GUSI.h>
	#elif defined(MSWin32)
	    #include <winsock.h>
            #include <ws2tcpip.h>
	    #undef AF_UNIX
	    
	#else
	    #error
	#endif /* UNIX, MacOS-GUSI */

	#if defined(UNIX)
	#define readsocket(a,b,c)	read((a), (b), (c))
	#define writesocket(a,b,c)	write((a), (b), (c))
	#define closesocket(a)		close((a))
#if defined(__hp9000s800) || defined(UNIX_HPUX)
        #define selectsocket(a,b,c,d,e) select(a,(int*)b,(int*)c,(int*)d,e) 
#else
        #define selectsocket(a,b,c,d,e) select(a,b,c,d,e)
#endif
#ifdef UNIX_AIX
        #define acceptsocket(a,b,c) accept(a,b,(size_t *)c)
        #define getsocknamesocket(a,b,c) getsockname(a,b,(size_t *)c)
        #define recvfromsocket(a,b,c,d,e,f) recvfrom(a,b,c,d,e,(size_t*)f)
#else
        #define acceptsocket(a,b,c) accept(a,b,c)
        #define getsocknamesocket(a,b,c) getsockname(a,b,c)
        #define recvfromsocket(a,b,c,d,e,f) recvfrom(a,b,c,d,e,f)
#endif
	#define socket_errno		errno
	#define INVALID_SOCKET	-1
	#define SOCKET_ERROR	-1
        #ifndef INADDR_NONE /* HP-UX defines its own INADDR_NONE */
	#define INADDR_NONE	-1
        #endif /* INADDR_NONE */
	
	#elif defined(MacOS) && defined(HAVE_GUSI)
	#define readsocket(a,b,c)	read((a), (b), (c))
	#define writesocket(a,b,c)	write((a), (b), (c))
	#define closesocket(a)		close((a))
        #define selectsocket(a,b,c,d,e) select(a,b,c,d,e)
        #define acceptsocket(a,b,c) accept(a,b,c)
        #define getsocknamesocket(a,b,c) getsockname(a,b,c)
        #define recvfromsocket(a,b,c,d,e,f) recvfrom(a,b,c,d,e,f)
	#define socket_errno		errno
	#define h_errno	errno
	#define INVALID_SOCKET	-1
	#define SOCKET_ERROR	-1
	
	#elif defined(MSWin32)
	#define readsocket(a,b,c)	recv((a), (b), (c), 0)
	#define writesocket(a,b,c)	send((a), (b), (c), 0)
        #define selectsocket(a,b,c,d,e) select(a,b,c,d,e)
        #define acceptsocket(a,b,c) accept(a,b,c)
        #define getsocknamesocket(a,b,c) getsockname(a,b,c)
        #define recvfromsocket(a,b,c,d,e,f) recvfrom(a,b,c,d,e,f)
	#define socket_errno		WSAGetLastError()
	
	#else
	#error
	#endif

    #elif defined(PCFTP_SOCKETS)	/* use PC/TCP Developement Kit for DOS */
	#include <sys/types.h>		/* C compiler header file */
	#include <netinet/in.h>		/* PC/TCP header files with */
	#include <netdb.h>		/* same names as BSDUNIX ones */
	#include <pctcp/types.h>
	#include <pctcp/pctcp.h>
	#include <pctcp/sockets.h>
	#include <pctcp/options.h>
	#include <pctcp/error.h>
    #else
	#error
    #endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */

#endif /* HAVE_SOCKET */

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

#include "bsio.h"
#include "newsiolex.h"

static	UCHAR *	get_stream_buffer ( PWord, int );
static	void	incr_fdrefcnt	( int );
static	int	decr_fdrefcnt	( int );
static	int	compute_flags	( char *, int , int );
#ifdef HAVE_SOCKET
static	void	delete_stream_name ( PWord );
static	int	accept_connection ( PWord, char * , char **);
#endif
static	int	stream_is_ready	( char *, long );
static	void	shift_buffer	( UCHAR * );
static	int	write_buf	( PWord, UCHAR * );
static	long	stream_end	( UCHAR * );
static	int	skip_layout	( UCHAR * );
static	int	octal		( UCHAR ** );
static	unsigned long	hexadecimal	( UCHAR ** );
static	int	decimal		( UCHAR **, UCHAR *, double *, int *);
static	int	escaped_char	( UCHAR ** );
static	void	quoted_atom	( PWord *, PWord *, int *, UCHAR **, UCHAR *lim, UCHAR *buf);
static	int	quoted_string	( PWord *, PWord *, int *, UCHAR **, UCHAR *, UCHAR * );
static	int	char_constant	( UCHAR **, UCHAR *, int, int );
static	int	next_token0	( UCHAR *, PWord *, int *, PWord *, int * );
static	int	format_type	( UCHAR * );

enum {CONSOLE_READ, CONSOLE_WRITE, CONSOLE_ERROR};

int do_lineedit = 0;
char lineedit_prompt[PATH_MAX]= "?- ";
char history_file[PATH_MAX] = "";
int  do_load_prev_history = 1;

static const char *sublineedit_prompt="?_ ";

#ifdef PURE_ANSI
long standard_console_read(char *buf, long n)
{
    return fread(buf, sizeof(char), n, stdin);
}

long standard_console_write(char *buf, long n)
{
    return fwrite(buf, sizeof(char), n, stdout);
}

long standard_console_error(char *buf, long n)
{
    return fwrite(buf, sizeof(char), n, stderr);
}
#elif defined(MSWin32)

// Quick fix for Control-C on NT
extern HANDLE InteruptCompleteEvent;

long standard_console_read(char *buf, long n)
{
	DWORD nr;
	BOOL i;
	
	ResetEvent(InteruptCompleteEvent);
	
	const char *prompt = do_lineedit == 1 ? lineedit_prompt : sublineedit_prompt;
	standard_console_write(prompt, strlen(prompt));

	i = ReadFile(GetStdHandle(STD_INPUT_HANDLE), buf, n, &nr, NULL);
	
	if (i) {
		if (nr == 0 && GetLastError() == ERROR_OPERATION_ABORTED) {
			WaitForSingleObject(InteruptCompleteEvent, INFINITE);
			errno = EINTR;
			return -1;
		} else return nr;
	} else {
		return -1;
	}
	
}

long standard_console_write(char *buf, long n)
{
    return write(STDOUT_FILENO, buf, n);
}

long standard_console_error(char *buf, long n)
{
    return write(STDERR_FILENO, buf, n);
}

#else

/*
 * linenoise_readbuffer()
 */

static long
linenoise_readbuffer(char *buf, long n)
{
    char *line;
    int count;

        /* Load the history file once (i.e., at startup)
           do_load_prev_history is set == 1 by default (set == 0 during shell startup)
           Setting it to 0 ensures the
           history will only be loaded once during an alspro shell session.
        */
    if (do_lineedit == 1 && do_load_prev_history == 1){
        linenoiseHistoryLoad(history_file);
        do_load_prev_history = 0;
    }

    line = linenoise( do_lineedit == 1 ? lineedit_prompt : sublineedit_prompt);

    if (line == NULL){
        return 0;
    }
    count = (int)strlen(line);
                /* count+1 because '\n' may be added here: */
    if (count+1 <= n )
    {
        linenoiseHistoryAdd(line);  // Add to the history.
        linenoiseHistorySave(history_file);  // Save the history on disk.
        memcpy(buf, line, count);
        buf[count] = '\n'; count++;
    }
    else {    /* Incoming line overruns buf: */
        errno = ENOBUFS;
        count = -1;
   }
    free(line);
    return (long)count;
}

long standard_console_read(char *buf, long n)
{
    if (do_lineedit !=  0)
    {
	return linenoise_readbuffer(buf, n);
   
    } else {
        return read(STDIN_FILENO, buf, n);
    }

}

long standard_console_write(char *buf, long n)
{
    return write(STDOUT_FILENO, buf, n);
}

long standard_console_error(char *buf, long n)
{
    return write(STDERR_FILENO, buf, n);
}

#endif /* PURE_ANSI */

#ifdef macintosh
console_func console_read = NULL,
	     console_write = NULL,
	     console_error = NULL;
#else
console_func console_read = standard_console_read,
	     console_write = standard_console_write,
	     console_error = standard_console_error;
#endif

static int console_io(int port, char *buf, size_t size)
{
    switch(port) {
    case CONSOLE_READ:
#ifdef MacOS
	if (MPW_Tool) {
	    printf("\n");
	}
#endif
    	if (console_read) return console_read(buf, size);
    	else return 0;
    	break;
    case CONSOLE_WRITE:
	if (console_write) return console_write(buf, size);
	else return size;
    	break;
    case CONSOLE_ERROR:
	if (console_error) return console_error(buf, size);
	else return size;
    	break;
    default:
	return -1;
	break;
    }
}

#ifdef macintosh
#pragma export on
#endif

EXPORT ALSPI_API(void)	PI_set_console_functions(console_func read,
				console_func write, console_func error)
{
	console_read = read;
	console_write = write;
	console_error = error;
}

#ifdef macintosh
#pragma export off
#endif

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
#if defined(MacOS)
    SIO_EOLNTYPE(buf) = SIOEOLN_READ_UNIV | SIOEOLN_WRITE_CR;
#elif defined(UNIX)
    SIO_EOLNTYPE(buf) = SIOEOLN_READ_UNIV | SIOEOLN_WRITE_LF;
#elif defined(MSWin32)
    SIO_EOLNTYPE(buf) = SIOEOLN_READ_UNIV | SIOEOLN_WRITE_CRLF;
#elif defined(PURE_ANSI)
    SIO_EOLNTYPE(buf) = SIOEOLN_READ_UNIV | SIOEOLN_WRITE_LF;
#else
#error
#endif   

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

#if defined(PURE_ANSI) || defined(__MWERKS__) 
/* This is a problem common to all systems that use MetroWerks ANSI Libraries open. */
/* Currently MacOS and MSWin32 */

/* Merge the Mac and Unix versions of this someday. */

typedef struct {
	int fd;
	unsigned short refcnt;
} fdrefcnt;

static fdrefcnt *fdrefcnts = NULL;
static int fdrefcnts_size;

static int openmax = 0;
#define OPEN_MAX_GUESS 256

static int refcnt_index(int fd)
{
	int first_key, last_key, key;
	
	first_key = fd % fdrefcnts_size;
	last_key = (first_key + fdrefcnts_size - 1) % fdrefcnts_size;
	for (key = first_key; key != last_key ; key = (key + 1) % fdrefcnts_size) {
		if (fdrefcnts[key].fd == fd) return key;
	}
	
	for (key = first_key; key != last_key; key = (key + 1) % fdrefcnts_size) {
		if (fdrefcnts[key].refcnt == 0) {
			fdrefcnts[key].fd = fd;
			return key;
		}
	}
	
	fatal_error(FE_FDREFOVERFLOW, 0);
	return 0;
}

#define PRIME_TABLE_MAX	55

int prime[PRIME_TABLE_MAX] = {
	2,	3,	5,	7,	11,	13,	17,	19,	23,	29,	31,	37,
	41,	43,	47,	53,	59,	61,	67,	71,	73,	79,	83,	89,
	97,	101,	103,	107,	109,	113,
	127,	131,	137,	139,	149,	151,
	157,	163,	167,	173,	179,	181,
	191,	193,	197,	199,	211,	223,
	227,	229,	233,	239,	241,	251,
	257,
};

static int next_prime(int n)
{
	int i;
	
	for (i = 0; i < PRIME_TABLE_MAX && prime[i] < n; i++) ;
	
	if (i >= PRIME_TABLE_MAX) {
	    fatal_error(FE_FDREFCNTS, 0);
	    return 0;
	} else return prime[i];
}
	
static void
incr_fdrefcnt(fd)
    int fd;
{
    if (fdrefcnts == NULL) {
#ifdef MSWin32
	openmax = 255;
#elif defined(_SC_OPEN_MAX)
	if ( (openmax = sysconf(_SC_OPEN_MAX)) < 0)
	    openmax = OPEN_MAX_GUESS;
#elif defined(OPEN_MAX)
	openmax = OPEN_MAX;
#elif defined(FOPEN_MAX)
	openmax = FOPEN_MAX;
#else
	openmax = OPEN_MAX_GUESS;
#endif /* OPEN_MAX */

	fdrefcnts_size = next_prime(openmax);
	
	if ( (fdrefcnts = (fdrefcnt *) 
			  malloc(fdrefcnts_size * sizeof(*fdrefcnts))) == NULL)
	    fatal_error(FE_FDREFCNTS, 0);
	memset(fdrefcnts, 0, fdrefcnts_size * sizeof(*fdrefcnts));
    }

    fdrefcnts[refcnt_index(fd)].refcnt++;
}

static int
decr_fdrefcnt(fd)
    int fd;
{
    if (--fdrefcnts[refcnt_index(fd)].refcnt <= 0)
	return 1;
    else
	return 0;
}

#else /* not MacOS */

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
    
    if (fd < 0 || fd >= openmax) fatal_error(FE_FDREFOVERFLOW, 0);
    fdrefcnts[fd]++;
}

static int
decr_fdrefcnt(fd)
    int fd;
{
    if (fd < 0 || fd >= openmax) fatal_error(FE_FDREFOVERFLOW, 0);
    if (--fdrefcnts[fd] <= 0)
	return 1;
    else
	return 0;
}

#endif

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
 * sio_file_open(FileName,SD,NMode,NBuffering,NEoln)
 *
 * sio_file_open is called from Prolog to open a file or other device available
 * through the file system.
 */

#ifdef MSWin32
static int MSL_open_patch(const char *path, int flags)
{
	if (flags & O_CREAT) {
		HANDLE h;
		flags = flags ^ O_CREAT;
		
		h = CreateFile(path, GENERIC_READ, 0, 0, OPEN_ALWAYS, 0, 0);
		if (h != INVALID_HANDLE_VALUE) CloseHandle(h);
	}
	
	return open(path, flags);
}
#endif

int
sio_file_open()
{
    PWord v1, v2, v3, v4, v5;
    int   t1, t2, t3, t4, t5;
    UCHAR *filename;
    UCHAR *buf;
#ifdef PURE_ANSI
    char *flags;
#else
    int   flags = 0;
#endif /* PURE_ANSI */

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (!getstring(&filename, v1, t1) || t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER || t5 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }
    SIO_TYPE(buf) = SIO_TYPE_FILE;

#ifdef PURE_ANSI
    switch (v3) {
	case SIOM_READ:
	    flags = "rb";
	    break;
	case SIOM_WRITE:
	    flags = "wb";
	    break;
	case SIOM_APPEND:
	    flags = "ab";
	    break;
	case SIOM_READ_WRITE:
	    flags = "w+b";
	    break;
    }
#else
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
#endif /* PURE_ANSI */

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;

    SIO_EOLNTYPE(buf) = v5;

#ifdef PURE_ANSI
    if ((SIO_FD(buf) = (long) fopen(filename, flags)) == (long)NULL) {
	SIO_ERRCODE(buf) = SIOE_SYSCALL;
	SIO_ERRNO(buf) = errno;
	FAIL;
    }
#else

#if defined(DOS) /* || defined(MSWin32) */
    if ((SIO_FD(buf) = open(filename, flags | O_BINARY, (S_IWRITE | S_IREAD))) == -1)
#elif	defined(MacOS)
    if ((SIO_FD(buf) = open((char *)filename, flags | O_BINARY)) == -1)
#elif   defined(MSWin32)
    if ((SIO_FD(buf) = MSL_open_patch((char *)filename, flags | O_BINARY)) == -1)
#elif defined(__GO32__) || defined(OS2)
    if ((SIO_FD(buf) = open(filename, flags|O_BINARY, 0666)) == -1)
#elif defined(UNIX)
    if ((SIO_FD(buf) = open((char *)filename, flags|O_BINARY, 0666)) == -1)
#else
#error
#endif
    {
        if (errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = errno;
	}
	FAIL;
    }
#endif /* PURE_ANSI */

    incr_fdrefcnt(SIO_FD(buf));
    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
}


int
sio_console_open()
{
    PWord v1, v2, v3, v4, v5, v6;
    int   t1, t2, t3, t4, t5, t6;
    UCHAR *buf;
/*    int   flags = 0;  */

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (t3 != WTP_INTEGER ||
	t4 != WTP_INTEGER || t5 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }
    SIO_TYPE(buf) = SIO_TYPE_CONSOLE;

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;

    switch (v3) {
	case SIOM_READ:
	    SIO_FD(buf) = CONSOLE_READ;
	    break;
	case SIOM_WRITE:
	    SIO_FD(buf) = CONSOLE_WRITE;
	    break;
    }
    if (v6 == 1) {
    	if (v3 != SIOM_WRITE) FAIL;
	SIO_FD(buf) = CONSOLE_ERROR;
    }

    SIO_EOLNTYPE(buf) = v5;

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

int
sio_gethostname()
{
    PWord v1,v2;
    int   t1,t2;
    char myhostname[MAXHOSTNAMELEN];

    w_get_An(&v1, &t1, 1);	

	gethostname(myhostname, MAXHOSTNAMELEN);
    w_mk_uia(&v2, &t2, (UCHAR *)myhostname);

    if (w_unify(v1, t1, v2, t2))
		SUCCEED;
    else
		FAIL;
}

/* nsocket stuff */

enum {internet_family, unix_family, appletalk_family};
enum {socket_stream, socket_datagram};
enum {default_protocol};

#if 0
/* How I'd like to write it: */


typedef unsigned long PObj;
typedef int PResult;
#define SUCCESS 1

double NPI_get_number(PObj o)
{
    switch (NPI_
}

int NPI_is_number(PObj o) 
{
    NPI_Type t = NPI_type(o);
    
    return (t == NPI_int_type || t == NPI_double_type);
}

int NPI_is_int(PObj o)
{
     double n = NPI_get_number(o);
     
     return (NPI_is_number(o) && n >= MIN_INIT && n <= MAX_INT);
}

int old_to_new_call(void (*new_func)(...), int arity)
{
    PObj arg[10];
    
    for (i = 1; i <= arity; i++) {
    	arg[i-1] = *(wm_SP + i + 1);
    } 
    
    switch (arity) {
    case 0: return new_func(); break;
    case 1: return new_func(); break;
    case 2: return new_func(); break;
    case 3: return new_func(); break;
    case 4: return new_func(); break;
    case 5: return new_func(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5]); break;
    }
}

static PResult true_sio_nsocket(PObj family, PObj type, PObj protocol, PObj descriptor, PObj result)
{
    int family_const, type_const, protocol_const, error = 0;

    if (!(NPI_is_int(family) && NPI_is_int(type) && NPI_is_int(protocol))) error = -1;
    
    if (!error) {
    	switch (NPI_get_int(family)) {
    	case internet_family: family_const = AF_INET; break;
    	default: error = -1; break;
    	}
    	
    	switch (NPI_get_int(type)) {
    	case stream: type_const = SOCK_STREAM; break;
    	case datagram: type_const = SOCK_DGRAM; break;
    	default: error = -1; break;
    	}
    	
    	switch (NPI_get_int(protocol)) {
    	case default_protocol: protocol_const = 0; break;
    	default: error = -1; break;
    	}
    	
    	if (!error) {
    	    sd = socket(family_const, type_const, protocol_const);
    	
    	    if (sd) (void) NPI_unify(descriptor, NPI_make_number(sd));
    	    else error = errno;
    	}
    }
    
    (void) NPI_unify(result, NPI_make_number(error));
    
    return SUCCESS;
}
#endif

int sio_nsocket(void)
{
    PWord family, type, protocol, descriptor, result;
    int family_t, type_t, protocol_t, descriptor_t, result_t;
    int family_const, type_const, protocol_const, sd, error = 0;

    PI_getan(&family, &family_t, 1);
    PI_getan(&type, &type_t, 2);
    PI_getan(&protocol, &protocol_t, 3);
    PI_getan(&descriptor, &descriptor_t, 4);
    PI_getan(&result, &result_t, 5);


    if (!(family_t == PI_INT && type_t  == PI_INT
    	&& protocol_t == PI_INT)) error = SOCKET_ERROR;
    
    if (!error) {
    	switch (family) {
    	case internet_family: family_const = AF_INET; break;
    	default: family_const = 0; error = SOCKET_ERROR; break;
    	}
    	
    	switch (type) {
    	case socket_stream: type_const = SOCK_STREAM; break;
    	case socket_datagram: type_const = SOCK_DGRAM; break;
    	default: type_const = 0; error = SOCKET_ERROR; break;
    	}
    	
    	switch (protocol) {
    	case default_protocol: protocol_const = 0; break;
    	default: protocol_const = 0; error = SOCKET_ERROR; break;
    	}
    	
    	if (!error) {
    	    sd = socket(family_const, type_const, protocol_const);
    	
    	    if (sd != SOCKET_ERROR) {
    	    	PWord num; int num_t;
    	    	PI_makedouble(&num, &num_t, sd);
    	    	(void) PI_unify(descriptor, descriptor_t, num, num_t);
    	    } else error = socket_errno;
    	}
    }
    
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocket_connect(void)
{
    PWord family, descriptor, address, port, result;
    int family_t, descriptor_t, address_t, port_t, result_t;
    int /* family_const, */ error = 0;
    char *host;
    double sd;
    struct sockaddr_in sockname_in;
    struct hostent *hp;

    PI_getan(&family, &family_t, 1);
    PI_getan(&descriptor, &descriptor_t, 2);
    PI_getan(&address, &address_t, 3);
    PI_getan(&port, &port_t, 4);
    PI_getan(&result, &result_t, 5);

    if (!(family_t == PI_INT && port_t  == PI_INT)) error = SOCKET_ERROR;

    switch (address_t) {
    case PI_SYM:
      host = PI_getsymname(NULL, address, 0);
      break;
    case PI_UIA:
      host = PI_getuianame(NULL, address, 0);
      break;
    default:
      host = NULL;
      error = SOCKET_ERROR;
      break;
    }
    
    PI_getdouble(&sd, descriptor);
    
    if (!error) {

	memset(&sockname_in, 0, sizeof sockname_in);

    	if ( (hp = gethostbyname(host)) ) {
	    memmove(&sockname_in.sin_addr, hp->h_addr, hp->h_length);
	}
#if defined(DGUX) || (defined(MacOS) && defined(HAVE_GUSI))
    	else if ((sockname_in.sin_addr.s_addr = inet_addr(host).s_addr) == INADDR_NONE)
#else
    	else if ((sockname_in.sin_addr.s_addr = inet_addr(host)) == INADDR_NONE)
#endif
	{
	    error = SOCKET_ERROR;
    	}
    	sockname_in.sin_family = AF_INET;
    	sockname_in.sin_port = htons(port);
    
    	if (!error) {
    	    error = connect(sd, (struct sockaddr *) &sockname_in, sizeof (struct sockaddr_in));
    	    if (error == SOCKET_ERROR) {
    	    	error = socket_errno;
    	    }
    	}
    }
        
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocket_bind(void)
{
    PWord family, descriptor, port, result;
    int family_t, descriptor_t, port_t, result_t;
    int /* family_const, */ error = 0;
    double sd;
    struct sockaddr_in sockport;

    PI_getan(&family, &family_t, 1);
    PI_getan(&descriptor, &descriptor_t, 2);
    PI_getan(&port, &port_t, 3);
    PI_getan(&result, &result_t, 4);

    if (!(family_t == PI_INT && port_t  == PI_INT)) error = SOCKET_ERROR;

    PI_getdouble(&sd, descriptor);
    
    if (!error) {

#ifdef UNIX
	int status = 1;
	(void) setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *) &status,
		sizeof(status));
#endif

	memset(&sockport, 0, sizeof sockport);
	sockport.sin_family = AF_INET;
	sockport.sin_addr.s_addr = INADDR_ANY;
	sockport.sin_port = htons((unsigned short)port);

	error = bind(sd, (struct sockaddr *) &sockport, sizeof(sockport));

   	if (error == SOCKET_ERROR) {
	    error = socket_errno;
    	}
    }
        
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocket_listen(void)
{
    PWord family, descriptor, backlog, result;
    int family_t, descriptor_t, backlog_t, result_t;
    int /* family_const, */ error = 0;
    double sd;

    PI_getan(&family, &family_t, 1);
    PI_getan(&descriptor, &descriptor_t, 2);
    PI_getan(&backlog, &backlog_t, 3);
    PI_getan(&result, &result_t, 4);

    if (!(family_t == PI_INT && backlog_t  == PI_INT)) error = SOCKET_ERROR;

    PI_getdouble(&sd, descriptor);
    
    if (!error) {


	error = listen(sd, backlog);

   	if (error == SOCKET_ERROR) {
	    error = socket_errno;
    	}
    }
        
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocket_accept(void)
{
    PWord family, descriptor, peer, new_descriptor, result, addr;
    int family_t, descriptor_t, peer_t, new_descriptor_t, result_t, addr_t;
    int /* family_const, */ error = 0;
    double sd;
    struct sockaddr_in cli_addr;
	socklen_t cli_len;
    int newfd;
    char  *sktaddr;

    PI_getan(&family, &family_t, 1);
    PI_getan(&descriptor, &descriptor_t, 2);
    PI_getan(&peer, &peer_t, 3);
    PI_getan(&new_descriptor, &new_descriptor_t, 4);
    PI_getan(&result, &result_t, 5);

    if (!(family_t == PI_INT && peer_t == PI_VAR)) error = SOCKET_ERROR;

    PI_getdouble(&sd, descriptor);
    
    if (!error) {

	cli_len = sizeof(cli_addr);
	newfd = acceptsocket(sd, (struct sockaddr *) &cli_addr, &cli_len);

    	if (newfd != SOCKET_ERROR) {
    	    PWord num; int num_t;
    	    PI_makedouble(&num, &num_t, newfd);
    	    (void) PI_unify(new_descriptor, new_descriptor_t, num, num_t);
    	} else error = socket_errno;
    }
    
	sktaddr = inet_ntoa(cli_addr.sin_addr);
    PI_makeuia(&addr, &addr_t, sktaddr);

    (void) PI_unify(peer, peer_t, addr, addr_t);

    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

/* sio_nsocket_select(+[read], +[write], +[except], -[readmark], -[writemark],
   -[exceptmark], +sec, +msec, -result) */
int sio_nsocket_select(void)
{
    PWord read, write, exception, read_mark, write_mark, except_mark, sec, usec, result;
    int read_t, write_t, exception_t, read_mark_t, write_mark_t, except_mark_t, sec_t, usec_t, result_t;
    int max_sd, r, error = 0;
    PWord l, item;
    int l_t, item_t;
    double sd;
    struct timeval timeout, *timeout_ptr;
    fd_set read_set, write_set, exception_set;
    
    PI_getan(&read, &read_t, 1);
    PI_getan(&write, &write_t, 2);
    PI_getan(&exception, &exception_t, 3);
    PI_getan(&read_mark, &read_mark_t, 4);
    PI_getan(&write_mark, &write_mark_t, 5);
    PI_getan(&except_mark, &except_mark_t, 6);
    PI_getan(&sec, &sec_t, 7);
    PI_getan(&usec, &usec_t, 8);
    PI_getan(&result, &result_t, 9);
    
    
    FD_ZERO(&read_set);
    FD_ZERO(&write_set);
    FD_ZERO(&exception_set);
    
    max_sd = 0;
    for (l = read, l_t = read_t; l_t == PI_LIST; PI_gettail(&l, &l_t, l)) {
    	PI_gethead(&item, &item_t, l);
    	PI_getdouble(&sd, item);
    	FD_SET((int)sd, &read_set);
    	max_sd = sd > max_sd ? sd : max_sd;
    }

    for (l = write, l_t = write_t; l_t == PI_LIST; PI_gettail(&l, &l_t, l)) {
    	PI_gethead(&item, &item_t, l);
    	PI_getdouble(&sd, item);
    	FD_SET((int)sd, &write_set);
    	max_sd = sd > max_sd ? sd : max_sd;
    }

    for (l = exception, l_t = exception_t; l_t == PI_LIST; PI_gettail(&l, &l_t, l)) {
    	PI_gethead(&item, &item_t, l);
    	PI_getdouble(&sd, item);
    	FD_SET((int)sd, &exception_set);
    	max_sd = sd > max_sd ? sd : max_sd;
    }
    
    
    if (sec_t == PI_SYM) timeout_ptr = NULL;
    else if (sec_t == PI_INT && usec_t == PI_INT) {
    	timeout.tv_sec = sec;
    	timeout.tv_usec = usec;
    	timeout_ptr = &timeout;
    } else {
        error = SOCKET_ERROR;
	timeout_ptr = NULL;
    }
    
    
    if (error != SOCKET_ERROR) {
    	r = selectsocket(max_sd+1, &read_set, &write_set, &exception_set, timeout_ptr); 
    
    	if (r >= 0) {
    	    /* Make the mark lists */
    	    PWord nl, nlist, nitem, set_sym, unset_sym, nil_list;
    	    int nl_t, nlist_t, nitem_t, set_sym_t, unset_sym_t, nil_list_t;
    	    
 	    PI_makesym(&set_sym, &set_sym_t, "set");
 	    PI_makesym(&unset_sym, &unset_sym_t, "unset");
 	    PI_makesym(&nil_list, &nil_list_t, "[]");
 
	    for (l = read, l_t = read_t, nl = read_mark, nl_t = read_mark_t;
	         l_t == PI_LIST;
	         PI_gettail(&l, &l_t, l), PI_unify(nl, nl_t, nlist, nlist_t),
	         PI_gettail(&nl, &nl_t, nlist)) {
	    	PI_gethead(&item, &item_t, l);
	    	PI_getdouble(&sd, item);
	    	PI_makelist(&nlist, &nlist_t);
	    	PI_gethead(&nitem, &nitem_t, nlist);
	    	if (FD_ISSET((int)sd, &read_set)) PI_unify(nitem, nitem_t, set_sym, set_sym_t);
	    	else PI_unify(nitem, nitem_t, unset_sym, unset_sym_t);  
	    }
	    PI_unify(nl, nl_t, nil_list, nil_list_t);

	    for (l = write, l_t = write_t, nl = write_mark, nl_t = write_mark_t;
	         l_t == PI_LIST;
	         PI_gettail(&l, &l_t, l), PI_unify(nl, nl_t, nlist, nlist_t),
	         PI_gettail(&nl, &nl_t, nlist)) {
	    	PI_gethead(&item, &item_t, l);
	    	PI_getdouble(&sd, item);
	    	PI_makelist(&nlist, &nlist_t);
	    	PI_gethead(&nitem, &nitem_t, nlist);
	    	if (FD_ISSET((int)sd, &read_set)) PI_unify(nitem, nitem_t, set_sym, set_sym_t);
	    	else PI_unify(nitem, nitem_t, unset_sym, unset_sym_t);  
	    }
	    PI_unify(nl, nl_t, nil_list, nil_list_t);

	    for (l = exception, l_t = exception_t, nl = except_mark, nl_t = except_mark_t;
	         l_t == PI_LIST;
	         PI_gettail(&l, &l_t, l), PI_unify(nl, nl_t, nlist, nlist_t),
	         PI_gettail(&nl, &nl_t, nlist)) {
	    	PI_gethead(&item, &item_t, l);
	    	PI_getdouble(&sd, item);
	    	PI_makelist(&nlist, &nlist_t);
	    	PI_gethead(&nitem, &nitem_t, nlist);
	    	if (FD_ISSET((int)sd, &read_set)) PI_unify(nitem, nitem_t, set_sym, set_sym_t);
	    	else PI_unify(nitem, nitem_t, unset_sym, unset_sym_t);  
	    }
	    PI_unify(nl, nl_t, nil_list, nil_list_t);
    
    	} else error = socket_errno;
    }
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocket_close(void)
{
    PWord descriptor, result;
    int descriptor_t, result_t;
    int error = 0;
    double sd;

    PI_getan(&descriptor, &descriptor_t, 1);
    PI_getan(&result, &result_t, 2);
    
    PI_getdouble(&sd, descriptor);
    
    if (!error) {

	error = closesocket(sd);
	if (error == SOCKET_ERROR) error = socket_errno;
    }
        
    (void) PI_unify(result, result_t, error, PI_INT);
    
    SUCCEED;
}

int sio_nsocketpair(void)
{
#ifdef MISSING_UNIX_DOMAIN_SOCKETS
   PWord result; int result_t;
  PI_getan(&result, &result_t, 3);
  (void) PI_unify(result, result_t, -1, PI_INT);

  SUCCEED;
#else
  int rc, sockfd[2];
  PWord descriptor0, descriptor1, result, num0, num1;
  int descriptor0_t, descriptor1_t, result_t, num0_t, num1_t;
  int error;

  PI_getan(&descriptor0, &descriptor0_t, 1);
  PI_getan(&descriptor1, &descriptor1_t, 2);
  PI_getan(&result, &result_t, 3);
  
  rc = socketpair(AF_UNIX, SOCK_STREAM, 0, sockfd);

  if (rc == 0) {
    PI_makedouble(&num0, &num0_t, sockfd[0]);
    PI_makedouble(&num1, &num1_t, sockfd[1]);
    (void) PI_unify(descriptor0, descriptor0_t, num0, num0_t);
    (void) PI_unify(descriptor1, descriptor1_t, num1, num1_t);
    error = 0;
  } else {
    error = errno;
  }

  (void) PI_unify(result, result_t, error, PI_INT);

  SUCCEED;
#endif
}


/* end nsocket stuff */


int
sio_nsocket_open()
{
    PWord descriptor, type, mode, buffering, eoln, stream;
    int   descriptor_t, type_t, mode_t, buffering_t, eoln_t, stream_t;
    UCHAR *buf;
    double sd;


    w_get_An(&descriptor, &descriptor_t, 1);
    w_get_An(&type, &type_t, 2);
    w_get_An(&mode, &mode_t, 3);
    w_get_An(&buffering, &buffering_t, 4);
    w_get_An(&eoln, &eoln_t, 5);
    w_get_An(&stream, &stream_t, 6);

    if ((buf = get_stream_buffer(stream, stream_t)) == (UCHAR *) 0)
	FAIL;
    
    SIO_ERRCODE(buf) = SIOE_INARG;
    
    if (mode_t != WTP_INTEGER || buffering_t != WTP_INTEGER || eoln_t != WTP_INTEGER)
	FAIL;

    if (compute_flags((char *)buf,mode,buffering) < 0)
	FAIL;

    SIO_EOLNTYPE(buf) = eoln;

    SIO_SOCKET_ADDRESS(buf) = 0;
    SIO_SOCKET_ADDRESS_LEN(buf) = 0;
    SIO_ERRCODE(buf) = SIOE_NORMAL;

    PI_getdouble(&sd, descriptor);

    SIO_FD(buf) = sd;

    switch (type) {
	case socket_stream :
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_STREAM;
	    break;
	case socket_datagram :
	    SIO_TYPE(buf) = SIO_TYPE_SOCKET_DGRAM;
	    break;
    }

    incr_fdrefcnt(SIO_FD(buf));
    SUCCEED;
}

static AP_Result GetHostError(AP_World *w, int error_code, AP_Obj object)
{
	switch (error_code) {
	case HOST_NOT_FOUND:
		return AP_SetError(w, AP_NewInitStructure(w,
					AP_NewSymbolFromStr(w, "host_not_found"), 1,
					object));
	case NO_DATA:
		return AP_SetError(w, AP_NewInitStructure(w,
					AP_NewSymbolFromStr(w, "no_data"), 1,
					object));
	case NO_RECOVERY:
		return AP_SetError(w, AP_NewSymbolFromStr(w, "no_recovery"));
	case TRY_AGAIN:
		return AP_SetError(w, AP_NewSymbolFromStr(w, "try_again"));
	default:
		return AP_SetStandardError(w, AP_SYSTEM_ERROR);
	}
}

static AP_Result UnifyEntity(AP_World *w, struct hostent *entity,
	AP_Obj official_name, AP_Obj alias_list, AP_Obj address_list)
{
	AP_Obj new_alias = AP_UNBOUND_OBJ, new_addr_list = AP_UNBOUND_OBJ,
	  cons, oldcons;
	int first;
	char **a;
	struct in_addr **n;
	
	if (entity->h_addrtype != AF_INET || entity->h_length != sizeof(struct in_addr))
		return AP_SetStandardError(w, AP_SYSTEM_ERROR);

	if (AP_Unify(w, official_name, AP_NewSymbolFromStr(w, entity->h_name))
		!= AP_SUCCESS) return AP_FAIL;
	
	for (first = 1, a = entity->h_aliases; *a; a++, first = 0) {
		cons = AP_NewInitList(w, AP_NewSymbolFromStr(w, *a), AP_UNBOUND_OBJ);
		if (first) new_alias = cons;
		else AP_Unify(w, AP_ListTail(w, oldcons), cons);
		oldcons = cons;
	}
	if (!first) AP_Unify(w, AP_ListTail(w, cons), AP_NullList(w));
	else new_alias = AP_NullList(w);	
	
	if (AP_Unify(w, alias_list, new_alias) != AP_SUCCESS) return AP_FAIL;
		
	for (first = 1, n = (struct in_addr **)entity->h_addr_list;
		 *n;
		 n++, first = 0) {
		cons = AP_NewInitList(w, 
			AP_NewSymbolFromStr(w, inet_ntoa(**n)), AP_UNBOUND_OBJ);
		if (first) new_addr_list = cons;
		else AP_Unify(w, AP_ListTail(w, oldcons), cons);
		oldcons = cons;
	}
	if (!first) AP_Unify(w, AP_ListTail(w, cons), AP_NullList(w));
	else new_addr_list = AP_NullList(w);

	if (AP_Unify(w, address_list, new_addr_list) != AP_SUCCESS) return AP_FAIL;
	
	return AP_SUCCESS;
}

static AP_Result do_gethostbyaddr(AP_World *w, AP_Obj address,
	AP_Obj official_name, AP_Obj alias_list, AP_Obj address_list)
{
	struct hostent *entity;
	struct in_addr addr;
	
	if (AP_ObjType(w, address) != AP_ATOM)
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), address);

#ifdef MacOS
	addr.s_addr = inet_addr((char *)AP_GetAtomStr(w, address)).s_addr;
#else
	addr.s_addr = inet_addr((char *)AP_GetAtomStr(w, address));
#endif
	entity = gethostbyaddr((char *)&addr, sizeof(struct in_addr), AF_INET);
	if (!entity) return GetHostError(w, h_errno, address);
	
	return UnifyEntity(w, entity, official_name, alias_list, address_list);
}

static AP_Result do_gethostbyname(AP_World *w, AP_Obj name,
	AP_Obj official_name, AP_Obj alias_list, AP_Obj address_list)
{
	const char *name_string;
	struct hostent *entity;
	
	if (AP_ObjType(w, name) != AP_ATOM)
		return AP_SetStandardError(w, AP_TYPE_ERROR,
					AP_NewSymbolFromStr(w, "atom"), name);
	
	name_string = AP_GetAtomStr(w, name);
	entity = gethostbyname((char *)name_string);
	if (!entity) return GetHostError(w, h_errno, name);

	return UnifyEntity(w, entity, official_name, alias_list, address_list);
}

int pbi_gethostbyname(void) {return AP_OldToNewCall(do_gethostbyname, 4);}
int pbi_gethostbyaddr(void) {return AP_OldToNewCall(do_gethostbyaddr, 4);}



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
    PWord v1, v2, v3, v4, v5, v6, v7, v8, v9, v10;
    int   t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;
    UCHAR *buf, *host_or_path, *clone_buf;
    int   portnum, domain, socktype;
    char myhostname[MAXHOSTNAMELEN];
    int status = 0;
    int isserver = 0;

#if defined(BERKELEY_SOCKETS)
    struct sockaddr_in sockname_in;	/* Internet socket addresses */
    struct hostent *hp;

#ifndef MISSING_UNIX_DOMAIN_SOCKETS
    struct sockaddr_un sockname_un;	/* Unix socket addresses */
#endif

#elif defined(PCFTP_SOCKETS)
    struct addr sockname;
#else
    #error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */

    w_get_An(&v1, &t1, 1);	/* Host name or path name (may be var) */
    w_get_An(&v2, &t2, 2);	/* Port to connect to (may be var) */
    w_get_An(&v3, &t3, 3);	/* Domain / protocol number */
    w_get_An(&v4, &t4, 4);	/* Socket type */
    w_get_An(&v5, &t5, 5);	/* Stream mode number (read or write) */
    w_get_An(&v6, &t6, 6);	/* Buffering number */
    w_get_An(&v7, &t7, 7);	/* end-of-line type */
    w_get_An(&v8, &t8, 8);	/* Queue length for listen */
    w_get_An(&v9, &t9, 9);	/* Socket Descriptor to clone */
    w_get_An(&v10, &t10, 10);	/* Stream Descriptor */

    if ((buf = get_stream_buffer(v10, t10)) == (UCHAR *) 0)
	FAIL;
    
    SIO_ERRCODE(buf) = SIOE_INARG;

    if (!getstring(&host_or_path, v1, t1)) {
	if (t1 == WTP_UNBOUND)
	    host_or_path = NULL;
	else
	    FAIL;
    }
    
    if (t2 == WTP_UNBOUND)
	portnum = 0;
    else if (t2 == WTP_INTEGER)
	portnum = v2;
    else
	FAIL;
    
    if (t3 == WTP_INTEGER && (
#ifndef MISSING_UNIX_DOMAIN_SOCKETS
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
    

    if (t5 != WTP_INTEGER || t6 != WTP_INTEGER || t7 != WTP_INTEGER || t8 != WTP_INTEGER)
	FAIL;

    if (compute_flags((char *)buf,v5,v6) < 0)
	FAIL;

    SIO_EOLNTYPE(buf) = v7;

    SIO_SOCKET_ADDRESS(buf) = 0;
    SIO_SOCKET_ADDRESS_LEN(buf) = 0;
    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if ( (clone_buf = get_stream_buffer(v9, t9)) ) {

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

#if defined(BERKELEY_SOCKETS)

    if ((SIO_FD(buf) = socket(domain, socktype, 0)) == (signed long)INVALID_SOCKET)
    {
	SIO_ERRCODE(buf) = SIOE_SYSCALL;
	SIO_ERRNO(buf) = socket_errno;
	FAIL;
    }

    switch (domain) {
#ifndef MISSING_UNIX_DOMAIN_SOCKETS
	case AF_UNIX :
	    sockname_un.sun_family = AF_UNIX;
	    strcpy(sockname_un.sun_path, (char *)host_or_path);

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
#endif /* MISSING_UNIX_DOMAIN_SOCKETS */
	case AF_INET :
	    gethostname(myhostname, MAXHOSTNAMELEN);
	    memset(&sockname_in, 0, sizeof sockname_in);
	    sockname_in.sin_family = AF_INET;
	    sockname_in.sin_addr.s_addr = INADDR_ANY;
	    sockname_in.sin_port = htons((u_short)portnum);

#ifdef UNIX
	    {
	      int status = 1;
	      (void) setsockopt(SIO_FD(buf), SOL_SOCKET, SO_REUSEADDR,
				(char *) &status, sizeof(status));
	    }
#endif

/*
	    if ( (host_or_path == NULL || strcmp(myhostname,host_or_path) == 0)
		 && bind(SIO_FD(buf), (struct sockaddr *) &sockname_in,
		         sizeof (struct sockaddr_in)) == 0 ) {
		isserver = 1;
	    }
*/
	    if ( (host_or_path == NULL )
		 && bind(SIO_FD(buf), (struct sockaddr *) &sockname_in,
		         sizeof (struct sockaddr_in)) == 0 ) {
		isserver = 1;
	    }

	    else {
		if (host_or_path == NULL)
		    host_or_path = (UCHAR *)myhostname;

		if ( (hp = gethostbyname((char *) host_or_path)) )
		    memmove((UCHAR *) &sockname_in.sin_addr,
			    (UCHAR *) hp->h_addr,
			    (size_t) hp->h_length);
#if defined(DGUX) || (defined(MacOS) && defined(HAVE_GUSI))
		else if ((sockname_in.sin_addr.s_addr = inet_addr(host_or_path).s_addr)
#else
		else if ((sockname_in.sin_addr.s_addr = inet_addr((char *)host_or_path))
#endif
				== (unsigned long) INADDR_NONE) {
		    status = SOCKET_ERROR;
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
		status = listen(SIO_FD(buf), v8);
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
		memset(sa, 0, sizeof (struct sockaddr_in));
		sa->sin_family = AF_INET;
		sa->sin_addr.s_addr = INADDR_ANY;
		/* any initial writes will go to the discard service (port 9) */
		sa->sin_port = 9;
	    }
	    break;
    }

/* FIXME: PCFTP_SOCKETS needs to be updated to match the unix rewrite */
#elif defined(PCFTP_SOCKETS)

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

#else
    #error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */

    if (status == SOCKET_ERROR) {
	if (socket_errno == EINTR)
	    SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	else {
	    SIO_ERRCODE(buf) = SIOE_SYSCALL;
	    SIO_ERRNO(buf) = socket_errno;
	}
	FAIL;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (t1 == WTP_UNBOUND && domain == AF_INET) {
	PWord vh;
	int th;
	w_mk_uia(&vh,&th,(UCHAR *)myhostname);
	(void) w_unify(v1,t1, vh,th);	/* cannot fail */
    }

    if (t2 == WTP_UNBOUND && domain == AF_INET) {
	socklen_t size = sizeof sockname_in;
	if (getsocknamesocket(SIO_FD(buf),
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
#ifndef MISSING_UNIX_DOMAIN_SOCKETS
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
	    && strcmp((char *)domainname,"unix") == 0 )
	    unlink((char *)pathname);
    }
#endif /* MISSING_UNIX_DOMAIN_SOCKETS */
}


/*
 * accept_connection will accept a connection on a stream socket.  If the
 * accept succeeds or if no accept is necessary, 0 will be returned.  If
 * accept fails, accept_connection will return -1.
 */

static int
accept_connection(vsd, buf, sktaddr)
    PWord vsd;
    char  *buf;
    char  **sktaddr;
{
		/* Internet socket addresses */
    struct sockaddr_in c_addr;	
	socklen_t c_addr_len;

	c_addr_len = sizeof(c_addr);
    if (SIO_FLAGS(buf) & SIOF_NEEDACCEPT) {
			/* int newfd = accept(SIO_FD(buf), (struct sockaddr *) 0, (int *) 0);  */
		int newfd = acceptsocket(SIO_FD(buf), (struct sockaddr *)&c_addr, &c_addr_len);
		if (newfd < 0) {
	    	return -1;
		}
		else {
	    	SIO_FLAGS(buf) &= ~SIOF_NEEDACCEPT;
	    	if (decr_fdrefcnt(SIO_FD(buf))) {
				delete_stream_name(vsd);
				/* close connection descriptor */
				if (closesocket(SIO_FD(buf)) == SOCKET_ERROR)
		    		perror("accept_connection");
	    	}
		*sktaddr = inet_ntoa(c_addr.sin_addr);
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
    PWord v1,v2,v3;
    int t1,t2,t3;
    UCHAR *buf;
    char *sktaddr;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
		FAIL;
    
    if (accept_connection(v1, (char *)buf, &sktaddr) == 0) {
    	w_mk_uia(&v3, &t3, (UCHAR *)sktaddr);
    	if (w_unify(v2, t2, v3, t3))
			SUCCEED;
    	else
			FAIL;
	}
    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = socket_errno;
		FAIL;
    }
}

#endif /* HAVE_SOCKET */

#ifdef REXEC
/*
 * sio_fork(ID)
 */
int
sio_fork(void)
{
#ifdef UNIX
    PWord v1;
    int   t1, pid;

    w_get_An(&v1, &t1, 1);

    signal(SIGCHLD, SIG_IGN);

    if (t1 != WTP_INTEGER && t1 != WTP_UNBOUND) {
	FAIL;
	}
    if ( (pid = fork()) < 0) {
	FAIL;			/* Can't fork anymore */
    }
	/* Just unify pid with v1; 
	   let prolog upstairs figure out about parent and child 
	*/
    if (w_unify(v1, t1, (PWord) pid, WTP_INTEGER))
		SUCCEED;
	else
		FAIL;
#else
    FAIL;
#endif
}


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

    if (!(getstring(&command, v2, t2)))
	{
	FAIL;
	}
    
    if (!getstring(&hostname, v1, t1))
		{ hostname = NULL; }

    if (!getstring(&username, v3, t3))
	{
	username = NULL;
	}
    
    if (!getstring(&password, v4,t4))
	{
	password = NULL;
	}
    
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
	    hostname = (UCHAR *)myhostname;
	}

	if ((rfd = rexec((char **) &hostname, se->s_port, (char *)
username, (char *)password, (char *)command,
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
		if ((rfdpair[1] = open("/dev/null",O_WRONLY|O_BINARY)) < 0)
		    _exit(1);
	    }
	    if (wfdpair[0] == -1) {
		if ((wfdpair[0] = open("/dev/null",O_RDONLY|O_BINARY)) < 0)
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

	    if (selectsocket(SIO_FD(buf)+1, &rfds, &wfds, &efds, &wait_time) > 0)
		return 1;
	    else
		{
		   return 0;
		}
#else
	    return 1;
#endif
	}
	default :
	    return 1;
    }
}

/* simple_select
 */

#if defined(HAVE_SOCKET) && defined(HAVE_SELECT)
/*------------------------------------------------------------*
 |	Called by:
 |		simple_select(SBList, Timeout)
 |		simple_select(+, +)
 |
 |	The Prolog call is:
 |
 |		simple_select(List, Int)
 |	
 |	where List is a list of streams;  this is converted to
 |	SBList, which is a list of the corresponding stream buffers.
 *------------------------------------------------------------*/
int
sio_simple_select()
{
    PWord v1, v2, v3, st;
    int t1, t2, t3, stt, wait_val;
	fd_set rfds, wfds, efds;
	struct timeval wait_time;
	int bfd, nfds = 0;
    UCHAR *buf;
	int rrr;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if ((t3 != WTP_UNBOUND) && (t2 != WTP_INTEGER) ) 
		FAIL;

	/* If v2 is a real, convert to integer: */
#ifdef DoubleType
    if (t2 == WTP_DOUBLE)
		wait_val = (int) v2;
#else  /* DoubleType */
    if (t2 == WTP_STRUCTURE) {
	PWord functor, d_arg;
	int   arity, d_argt;
	int   i;
	double vvv;
	w_get_arity(&arity, v2);
	w_get_functor(&functor, v2);

	if (arity == 4 && functor == TK_DDOUBLE) {
	  for (i = 0; i < 4; i++) {
	    w_get_argn(&d_arg, &d_argt, v2, i + 1);
	    *(((short *) &vvv) + i) = d_arg;
	  }
	  wait_val = (int) vvv;
	}
	else
	  FAIL;
    }
#endif
    else if (t2 == WTP_INTEGER)
		wait_val = v2;
	else
		FAIL;

	wait_time.tv_sec = wait_val / 1000000;
	wait_time.tv_usec = wait_val % 1000000;

	FD_ZERO(&rfds);
	FD_ZERO(&wfds);
	FD_ZERO(&efds);

	while (v1 != TK_NIL) {
		w_get_car(&st,&stt,&v1);
    	if ((buf = get_stream_buffer(st, stt)) == (UCHAR *) 0)
			FAIL;
		bfd = SIO_FD(buf);
		FD_SET(bfd, &rfds);
		nfds = max(nfds, bfd);
		w_get_cdr(&v1,&t1,v1);
	}

	if (v2 > 0)
		rrr = selectsocket(nfds+1, &rfds, &wfds, &efds, &wait_time);
	else
		rrr = selectsocket(nfds+1, &rfds, &wfds, &efds, NULL);

/*	if (rrr  >= 0) */
    if (w_unify(v3, t3, (PWord) rrr, WTP_INTEGER))
		SUCCEED;
	else
		FAIL;
}

#endif

#if HAVE_SOCKET
/*
 * sio_poll will check if an I/O operation is ready on a stream.  It will
 * succeed if an I/O operation on the stream will not block. Otherwise it will
 * fail.  A time value in microseconds may be given as the time to wait
 * for the stream to become ready.
 *
 * sio_poll(Stream, WaitTime)
 */

extern	int	get_number	(PWord, int, double *);

int
sio_poll()
{
    PWord v1, v2;
    int t1, t2;
    UCHAR *buf;
	double waitval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

/*
    if (t2 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }
*/
	if (!get_number(v2, t2, &waitval)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	}


    SIO_ERRCODE(buf) = SIOE_NORMAL;

    if (stream_is_ready((char *)buf,(long)waitval))
		SUCCEED;
    else
		FAIL;
}
#endif /* HAVE_SOCKET */

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
    PWord v1, v2, v3, v4, v5;
    int   t1, t2, t3, t4, t5;
    UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if ((buf = get_stream_buffer(v2, t2)) == (UCHAR *) 0)
	FAIL;

    if (t1 != WTP_INTEGER || t3 != WTP_INTEGER || t4 != WTP_INTEGER || t5 != WTP_INTEGER) {
	SIO_ERRCODE(buf) = SIOE_INARG;
	FAIL;
    }

    if (compute_flags((char *)buf,v3,v4) < 0)
	FAIL;

    SIO_EOLNTYPE(buf) = v5;

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
#ifdef HAVE_SOCKET
    char *sktaddr;
#endif

#ifdef SysVIPC
    struct msgbuf *msgp;
#endif /* SysVIPC */

    if (SIO_FLAGS(buf) & SIOF_READ) {
#ifdef PURE_ANSI
	if (fseek((FILE *) SIO_FD(buf), (long) SIO_BUFPOS(buf), SEEK_SET) == -1)
#else
	if (lseek((int) SIO_FD(buf), (long) SIO_BUFPOS(buf), SEEK_SET) == -1)
#endif /* PURE_ANSI */
	{
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
#ifdef PURE_ANSI
	    writeflg = fwrite((void *)SIO_BUFFER(buf), 1, (size_t)SIO_LPOS(buf), (FILE *)SIO_FD(buf));
#else
	    writeflg = write(SIO_FD(buf), (char *)SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#endif /* PURE_ANSI */
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
	    if (accept_connection(vsd, (char *)buf, &sktaddr)) {
		writeflg = SOCKET_ERROR;
		break;		/* break early */
	    }

#if defined(BERKELEY_SOCKETS)
	    writeflg = writesocket(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#elif defined(PCFTP_SOCKETS)
	    writeflg = net_write(SIO_FD(buf), SIO_BUFFER(buf),
				 SIO_LPOS(buf), 0);
#else
#error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */
	    break;

	case SIO_TYPE_SOCKET_DGRAM:
#if defined(BERKELEY_SOCKETS)
	    if (SIO_SOCKET_ADDRESS(buf))
		writeflg = sendto(SIO_FD(buf),
				  SIO_BUFFER(buf),
				  SIO_LPOS(buf), 0,
				  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
				  SIO_SOCKET_ADDRESS_LEN(buf));
	    else
		writeflg = writesocket(SIO_FD(buf), SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
#elif defined(PCFTP_SOCKETS)
	    /* FIXME: SIO_SOCKET_ADDRESS(buf) == 0 */
	    writeflg = net_writeto(SIO_FD(buf),
				   SIO_BUFFER(buf),
				   SIO_LPOS(buf),
				   SIO_SOCKET_ADDRESS(buf),
				   0);
#else
#error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */
	    break;
#endif /* HAVE_SOCKET */

	case SIO_TYPE_CONSOLE:
	    writeflg = console_io(SIO_FD(buf), (char *)SIO_BUFFER(buf), (size_t)SIO_LPOS(buf));
	    break;

	default:
	    SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	    return 0;
	    break;
    }

    switch (SIO_TYPE(buf)) {
#ifdef HAVE_SOCKET
    case SIO_TYPE_SOCKET_STREAM:
    case SIO_TYPE_SOCKET_DGRAM:
	if (writeflg == SOCKET_ERROR) {
	    if (socket_errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = socket_errno;
	    }
	    return 0;
	}
	break;
#endif
    default:
	if (writeflg == -1) {
	    if (errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = errno;
	    }
	    return 0;
	}
	break;
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
#ifdef PURE_ANSI
		closeflg = fclose((FILE *)SIO_FD(buf));
#else
		closeflg = close(SIO_FD(buf));
#endif /* PURE_ANSI */
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

#if defined(BERKELEY_SOCKETS)
		if (SIO_TYPE(buf) == SIO_TYPE_SOCKET_DGRAM)
		    closeflg = closesocket(SIO_FD(buf));
		else {
/* FIXME: Figure out how to check for broken shutdown */
#if defined(SysVR3) || (defined(MacOS) && defined(HAVE_GUSI) || defined(MSWin32))
		    /* shutdown seems to be broken on svr3 */
		    closeflg = closesocket(SIO_FD(buf));
#else /* SysVR3, MacOS-GUSI */
		    closeflg = shutdown(SIO_FD(buf),2);
#endif /* SysVR3, MacOS-GUSI */
		}
#elif defined(PCFTP_SOCKETS)
		closeflg = net_release(SIO_FD(buf));
#else
#error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */
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

    switch (SIO_TYPE(buf)) {
#ifdef HAVE_SOCKET
    case SIO_TYPE_SOCKET_STREAM:
    case SIO_TYPE_SOCKET_DGRAM:
#ifdef HAVE_SOCKET
	if (closeflg == SOCKET_ERROR) {
	    if (socket_errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = socket_errno;
	    }
	    FAIL;
	}
#endif
	break;
#endif
    default:
	if (closeflg == -1) {
	    if (errno == EINTR)
		SIO_ERRCODE(buf) = SIOE_INTERRUPTED;
	    else {
		SIO_ERRCODE(buf) = SIOE_SYSCALL;
		SIO_ERRNO(buf) = errno;
	    }
	    FAIL;
	}
	break;
    }

    SIO_ERRCODE(buf) = SIOE_NORMAL;
    SUCCEED;
} /* sio_close */


/*
 * read_eoln() and write_eoln() return true iff reading/writing the byte b to buf will
 * input/output an end of line.
 *
 * These functions are only used by sio_get_byte() and sio_put_byte().
 *
 */
 
static int read_eoln(UCHAR *buf, int b)
{
    int new_line;
    
    switch(SIO_EOLNTYPE(buf) & SIOEOLN_READ_MASK) {
    case SIOEOLN_READ_CRLF:
	new_line = ((b == LF) && (SIO_FLAGS(buf) & SIOF_GOT_CR));
	break;
    case SIOEOLN_READ_CR:
	new_line = (b == CR);
	break;
    case SIOEOLN_READ_LF:
	new_line = (b == LF);
	break;
    case SIOEOLN_READ_UNIV:
    	new_line = ((b == CR) || (b == LF && !(SIO_FLAGS(buf) & SIOF_GOT_CR)));
    	break;
    default:
	SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	new_line = 0;
	break;
    }

    SIO_FLAGS(buf) &= ~SIOF_GOT_CR;
    if (b == CR) SIO_FLAGS(buf) |= SIOF_GOT_CR;
    
    return new_line;
}

static int write_eoln(UCHAR *buf, int b)
{
    int new_line;
    
    switch(SIO_EOLNTYPE(buf) & SIOEOLN_WRITE_MASK) {
    case SIOEOLN_WRITE_CRLF:
	new_line = ((b == LF) && (SIO_FLAGS(buf) & SIOF_PUT_CR));
	    break;
    case SIOEOLN_WRITE_CR:
	new_line = (b == CR);
	break;
    case SIOEOLN_WRITE_LF:
	new_line = (b == LF);
	break;
    default:
	SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	new_line = 0;
	break;
    }

    SIO_FLAGS(buf) = (SIO_FLAGS(buf) & ~SIOF_PUT_CR);
    if (b == CR) SIO_FLAGS(buf) |= SIOF_PUT_CR;
    
    return new_line;
}



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

    /* EOLN Question:  The standard says get_byte() can only be used on binary
       files.  Although ALS Prolog does not make a distinction between text and
       binary files, should get_byte() really keep track of column and line numbers?
       Isn't this also inefficient?
    */
    if (read_eoln(buf, pos)) {
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

    /* EOLN Question:  The standard says put_byte() can only be used on binary
       files.  Although ALS Prolog does not make a distinction between text and
       binary files, should put_byte() really keep track of line buffering?
       Isn't this also inefficient?
    */
    if ((SIO_FLAGS(buf) & SIOF_BBYTE) ||
	((SIO_FLAGS(buf) & SIOF_BLINE) && write_eoln(buf, v2)) ||
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

/* EOLN Question:  sio_unget_byte() does not update columns and line numbers.
   Should this be made consistent with sio_get_byte() or should byte-oriented
   predicates not update text-oriented stream information?
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


/*
 * sio_set_do_lineedit()
 */

int
sio_set_do_lineedit()
{
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    do_lineedit = (int)v1;
    SUCCEED;
}

/*
 * sio_set_lineedit_prompt(Prompt)
 */

int
sio_set_lineedit_prompt()
{
    PWord v1;
    int   t1;
    const char *prompt;

    w_get_An(&v1, &t1, 1);

    if (!getstring((UCHAR **)&prompt, v1, t1)){
        prompt = "?- ";
    }
    strncpy(lineedit_prompt, prompt, sizeof(lineedit_prompt)-1);
    lineedit_prompt[sizeof(lineedit_prompt) - 1] = '\0';
    SUCCEED;
}

/*
 * sio_set_history_file(FileName)
 */

int
sio_set_history_file()
{
    PWord v1;
    int   t1;
	const char *path;

    w_get_An(&v1, &t1, 1);

    if (!getstring((UCHAR **)&path, v1, t1)) {
        path = "";
    }
    strncpy(history_file, path, sizeof(history_file)-1);
    history_file[sizeof(history_file) - 1] = '\0';
    SUCCEED;
}


/*
 * sio_set_no_load_prev_history
 * Sets do_load_prev_history = 0 to suppress loading previous history:
 * Default is to load the previous history.
 */

int
sio_set_no_load_prev_history()
{
        do_load_prev_history = 0;
        SUCCEED;
}


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
#ifdef HAVE_SOCKET
    char *sktaddr;
#endif

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
#ifdef PURE_ANSI
	    nchars = fread((void *)buffer, 1, (size_t)nchars, (FILE *)SIO_FD(buf));
#else
	    nchars = read(SIO_FD(buf), (char *)buffer, (size_t)nchars);
#endif /* PURE_ANSI */
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
	    if (accept_connection(v1,(char *)buf,&sktaddr) < 0) {
		nchars = -1;
		break;			/* break early */
	    }

	    if ((SIO_FLAGS(buf) & SIOF_DONTBLOCK) && !stream_is_ready((char *)buf,0)) {
		SIO_ERRCODE(buf) = SIOE_NOTREADY;
		return 0;
	    }
		
#if defined(BERKELEY_SOCKETS)
	    nchars = readsocket(SIO_FD(buf), buffer, (size_t)nchars);
#elif defined(PCFTP_SOCKETS)
	    /* FIXME! */
	    nchars = net_read(SIO_FD(buf), buffer, nchars, &a, 0);
#else
#error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */
	    break;

	case SIO_TYPE_SOCKET_DGRAM:
#if defined(BERKELEY_SOCKETS)
	    if (SIO_SOCKET_ADDRESS(buf)) {
		socklen_t len = SIO_SOCKET_ADDRESS_LEN(buf);
		nchars = recvfromsocket(SIO_FD(buf), buffer,nchars, 0,
				  (struct sockaddr *) SIO_SOCKET_ADDRESS(buf),
				  &len);
	    }
	    else
		nchars = readsocket(SIO_FD(buf), buffer, (size_t)nchars);
#elif defined(PCFTP_SOCKETS)
	    /* FIXME! */
	    nchars = net_read(SIO_FD(buf), buffer, nchars, &a, 0);
#else
#error
#endif /* BERKELEY_SOCKETS, PCFTP_SOCKETS */
	    SIO_FLAGS(buf) |= SIOF_EOF;	/* each packet is self contained */
	    break;
#endif /* HAVE_SOCKET */

	case SIO_TYPE_CONSOLE:
	    nchars = console_io(SIO_FD(buf), (char *)buffer, (size_t)nchars);
	    break;
	   
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

/*-------------------------------------------------------------------------*
 | sio_getpos(Stream,Pos)
 |
 |   sio_getpos is used to get the current file position.  This could be
 |   implemented from Prolog with sio_seek (see below), but is much faster
 |   and will work on non-seekable streams.
 *-------------------------------------------------------------------------*/

int
sio_getpos()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *buf;
    int   curpos;


    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((buf = get_stream_buffer(v1, t1)) == (UCHAR *) 0)
	FAIL;

    curpos = SIO_BUFPOS(buf) + SIO_CPOS(buf);

	make_numberx(&v3, &t3, curpos, WTP_INTEGER);

/*    if (w_unify(v2, t2, (PWord) curpos, WTP_INTEGER)) */

    if (w_unify(v2, t2, (PWord) v3, t3))
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

#ifdef PURE_ANSI
    curpos = fseek((FILE *)SIO_FD(buf), 0, SEEK_CUR);
    endpos = fseek((FILE *)SIO_FD(buf), 0, SEEK_END);
    (void) fseek((FILE *)SIO_FD(buf), curpos, SEEK_SET);
#else
    curpos = lseek(SIO_FD(buf), 0, SEEK_CUR);
    endpos = lseek(SIO_FD(buf), 0, SEEK_END);
    (void) lseek(SIO_FD(buf), curpos, SEEK_SET);
#endif /* PURE_ANSI */
    return endpos;
}

/*-------------------------------------------------------------------*
 | sio_seek(Stream,OldPos,NewPos,Whence)
 |
 |   Stream  is a file stream to get/set the position of
 |   OldPos  will be unified with the current stream position
 |   NewPos  is the position to be set
 |   Whence  is the value used by lseek (0,1,2) to either
 |           set the absolute position, set from current position,
 |           or to set from end-of-file
 *-------------------------------------------------------------------*/

int
sio_seek()
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    UCHAR *buf;
    long  pos;
    long  curpos;

	PWord functor, v0;
	int   arity, t0;
	double rv;

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

    if (t2 != WTP_INTEGER && t2 != WTP_UNBOUND) // OldPos not int && not unbound
    {
		// is NewPos a double? (rep. by struct: TK_DDOUBLE/4 )
	if (t3 != WTP_STRUCTURE) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	}
    	else {
		w_get_arity(&arity, v3);
		w_get_functor(&functor, v3);
		if (arity != 4 || functor != TK_DDOUBLE) {
			SIO_ERRCODE(buf) = SIOE_INARG;
			FAIL;
		}
	}
    }
	// if here, OldPos is int or unbound, or
	// OldPos not int && not unbound && NewPos is double

    if (t3 != WTP_INTEGER)  	// NewPos not int
    {
		// is NewPos a double? (rep. by struct: TK_DDOUBLE/4 )
	if (t3 != WTP_STRUCTURE) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	}
    	else {
		w_get_arity(&arity, v3);
		w_get_functor(&functor, v3);
		if ((arity == 4 && functor == TK_DDOUBLE)) 
		{
			int i;
			for (i = 0; i < 4; i++) {
				w_get_argn(&v0, &t0, v3, i + 1);
				*(((short *) &rv) + i) = v0;
			}
			v3 = (PWord)floor(rv);
		}
			// No, NewPos not a double
    		else {
			SIO_ERRCODE(buf) = SIOE_INARG;
			FAIL;
		}
	}
    }
    if (t4 != WTP_INTEGER) {
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
#ifdef PURE_ANSI
	if ((pos = fseek((FILE *)SIO_FD(buf), v3, v4)) < 0)
#else
	if ((pos = lseek(SIO_FD(buf), v3, v4)) < 0)
#endif /* PURE_ANSI */
	{
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

	make_numberx(&v0, &t0, curpos, WTP_INTEGER);
/*    if (w_unify(curpos, WTP_INTEGER, v2, t2)) */
    if (w_unify(v0, t0, v2, t2))
	SUCCEED;
    else
	FAIL;
}

/*
 * scan_eoln() scans the beginning of a null-terinated string for an end-of-line
 * using the eoln convension passed in eoln_type.
 * if an end-of-line is found, the length of the end-of-line chars (1 or 2) is return,
 * otherwise 0 is returned.
 *
 */

static int scan_eoln(const char *cs, int eoln_type)
{
    int len;
    
    switch (eoln_type) {
    case SIOEOLN_READ_CR:
	len = (*cs == CR) ? 1 : 0;
	break;
    case SIOEOLN_READ_LF:
	len = (*cs == LF) ? 1 : 0;
	break;
    case SIOEOLN_READ_CRLF:
	len = (*cs == CR && *(cs+1) == LF) ? 2 : 0;
	break;
    case SIOEOLN_READ_UNIV:
	if      (*cs == LF) len = 1;
	else if (*cs == CR) len = (*(cs+1) == LF) ? 2 : 1;
	else                len = 0;
	break;
    default:
	len = 0;
	break;
    }
    
    return len;
}

/*
 * get_eoln() scans the stream buffer buf at position p (up to lim) for an end-of-line
 * using the streams eoln type.
 * If an end-of-line is found, then the length of the end-of-line chars (1 or 2) is returned.
 * If there is not end-of-line at p, then 0 is returned.
 * If there aren't enough chars from p up to lim to detect an end-of-line, then -1 is returned.
 *
 */

static int get_eoln(UCHAR *p, UCHAR *lim, UCHAR *buf)
{
    int len;
    
    switch(SIO_EOLNTYPE(buf) & SIOEOLN_READ_MASK) {
    case SIOEOLN_READ_CR:
	len = (p < lim) ? ((*p == CR) ? 1 : 0) : -1;
	break;
    case SIOEOLN_READ_LF:
	len = (p < lim) ? ((*p == LF) ? 1 : 0) : -1;
	break;
    case SIOEOLN_READ_CRLF:
	len = (p < lim-1) ? (((*p == CR) && (*(p+1) == LF)) ? 2 : 0)
	                  : ((p == lim-1 && (*p != CR || (SIO_FLAGS(buf) & SIOF_EOF))) ? 0 : -1);
	break;
    case SIOEOLN_READ_UNIV:
    	if (p < lim) {
	    if      (*p == LF) len = 1;
	    else if (*p == CR) len = (p < lim-1 && *(p+1) == LF) ? 2 : 1;
	    else               len = 0;
	} else len = -1;
	SIO_FLAGS(buf) &= ~SIOF_GOT_CR;
	SIO_FLAGS(buf) |=  (len == 1 && *p == CR) ? SIOF_GOT_CR : 0;
    	break;
    default:
	SIO_ERRCODE(buf) = SIOE_INVALIDTYPE;
	len = -1;
	break;
    }
    
    return len;
}

/*
 * skip_trailing_lf() will advance the streams read position over a line-feed, if
 * the last character read from the stream was a carriage return and the streams
 * is using the universal end-of-line read type.
 *
 * This should only be used when crossing buffer boundries (which might split a
 * CR,LF pair).
 *
 */
 
static void skip_trailing_lf(UCHAR *buf)
{
    if ((SIO_EOLNTYPE(buf) & SIOEOLN_READ_MASK) == SIOEOLN_READ_UNIV
        && SIO_FLAGS(buf) & SIOF_GOT_CR
        && SIO_CPOS(buf) < SIO_LPOS(buf)
        && SIO_BUFFER(buf)[SIO_CPOS(buf)] == LF) {
    	SIO_CPOS(buf)++;
    	SIO_FLAGS(buf) &= ~SIOF_GOT_CR;
    }
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
    int   nesting, inlinecomment, nl;

    startpos = p = SIO_BUFFER(buf) + SIO_CPOS(buf);
    lim = SIO_BUFFER(buf) + SIO_LPOS(buf);

    if (!(SIO_FLAGS(buf) & (SIOF_INATOM | SIOF_INSTRING | SIOF_INQATOM))) {

	nesting = SIO_COMMENT(buf);
	inlinecomment = SIO_FLAGS(buf) & SIOF_LCOMMENT;
	nl = 0;

	while (p < lim) {
	    if (inlinecomment) {
		while (p < lim && !(nl = get_eoln(p, lim, buf)))
		    p++;
		if (p < lim) {
		    if (nl == -1) lim = p;
		    else {
			p += nl;
		    inlinecomment = 0;
		    SIO_LINENUM(buf)++;
		    startpos = p;
		    SIO_COLUMN(buf) = 0;
		}
	    }
	    }
	    else if (nesting > 0) {
		while (p < lim && nesting > 0) {
		    if (*p == '/') {
		    	if (p+1 >= lim) lim = p;
		    	else { 
		    	    if (*(p + 1) == '*') {
				p += 2;
				nesting++;
			    } else p++;
			}
		    }
		    else if (*p == '*') {
		    	if (p+1 >= lim) lim = p;
		    	else {
		    	    if(*(p+1) == '/') {
		    		p += 2;
		    		nesting--;
		    	    } else p++;
		    	}
		    }
		    else if ((nl = get_eoln(p, lim, buf))) {
		        if (nl == -1) lim = p;
		        else {
			    p += nl;
			    SIO_LINENUM(buf)++;
			    startpos = p;
			    SIO_COLUMN(buf) = 0;
			}
		    }
		    else p++;
		}
	    }
	    else {
		while (p < lim && (sio_chtb[*p] & SIOC_WHITESPACE)) {
		    if ((nl = get_eoln(p, lim, buf))) {
		        if (nl == -1) lim = p;
		        else {
		            p += nl;
			    SIO_LINENUM(buf)++;
			    startpos = p;
			    SIO_COLUMN(buf) = 0;
			}
		    }
		    else p++;
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
binary(UCHAR **pp)
{
    register UCHAR *p;
    register int val;

    p = *pp;
    val = *p - '0';
    p++;
    while ( *p == '0' || *p == '1') {
	val = (val << 1) + (*p - '0');
	p++;
    }
    *pp = p;
    return val;
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

static unsigned long
hexadecimal(pp)
    UCHAR **pp;
{
    register UCHAR *p;
    register unsigned long val;
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

/*
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
	int   expnt = 0;
	int   neg = 0;
	double m, z;

	if (*++p == '-') {
	    neg = 1;
	    p++;
	}
	else if (*p == '+')
	    p++;

	while (sio_chtb[*p] & SIOC_DECIMAL)
	    expnt = 10 * expnt + (*p++) - '0';

	m = 1.0;
	z = 10.0;
	while (expnt != 0) {
	    if (expnt & 1) {
		m *= z;
		expnt -= 1;
	    }
	    else {
		z *= z;
		expnt >>= 1;
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
*/
static int
decimal(UCHAR **pp, UCHAR *lim, double *dval, int *ty)
{
    register UCHAR *p;
    double d, frac;
    int success = 1;

    *ty = WTP_INTEGER;
    p = *pp;
    d = *p++ - '0';
    frac = 0;

    while (sio_chtb[*p] & SIOC_DECIMAL) {
	d = 10.0 * d + (*p++) - '0';
    }
	
    if (*p == '.') {
    	if ((sio_chtb[*(p + 1)] & SIOC_DECIMAL)) {
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
	} else if ((p+1) >= lim) success = 0;
    }

    if ((sio_chtb[*p] & SIOC_E)) {
    	if (((sio_chtb[*(p + 1)] & SIOC_PLUSMINUS)
	  && (sio_chtb[*(p + 2)] & SIOC_DECIMAL))
	 || (sio_chtb[*(p + 1)] & SIOC_DECIMAL)) {
	    int   expnt = 0;
	    int   neg = 0;
	    double m, z;

	    if (*++p == '-') {
		neg = 1;
		p++;
	    }
	    else if (*p == '+')
		p++;

	    while (sio_chtb[*p] & SIOC_DECIMAL)
		expnt = 10 * expnt + (*p++) - '0';

	    m = 1.0;
	    z = 10.0;
	    while (expnt != 0) {
		if (expnt & 1) {
		    m *= z;
		    expnt -= 1;
		}
		else {
		    z *= z;
		    expnt >>= 1;
		}
	    }
	    if (neg)
		d /= m;
	    else
		d *= m;
    	} else if ((p+2) >= lim) success = 0;
    }

    if (success) {
	*pp = p;
	*dval = d;
	return 1;
    } else return 0;
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
	    return LF;
	case 'r':
	    *pp = p + 1;
	    return CR;
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
    int   escapefound, eossave, nl;

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
	    else if ((nl = get_eoln(p+1, lim, buf))) {
	    	if (nl == -1) {count--; lim = p;}
	    	else {
		    p += 1+nl;
		count--;
		escapefound = 1;
	    }
	    }
	    else {
		pmem = p;
		(void) escaped_char(&pmem);
		if (pmem >= lim) {
		    count--;
		    lim = p;
		} else {
		p = pmem;
		escapefound = 1;
	    }
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
	else if ((nl = get_eoln(p, lim, buf))) {
	    if (nl == -1) {count--; lim = p;}
	    else {
	    (*pp)++;		/* advance over initial quote */
	    *vpTokType = TK_LEXERR;
	    *vpTokVal = SIOL_UNTERM_SYMBOL;
	    *tpTokVal = WTP_INTEGER;
	    return;		/* error: newline in atom */
	}
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
		if ((nl = scan_eoln((char *)(p+1), SIO_EOLNTYPE(buf) & SIOEOLN_READ_MASK))) {
		    SIO_LINENUM(buf)++;
		    p += 1+nl;
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
    int   t2;
    int ccount = 0, nl;

    p = *pp + 1;		/* advance over initial quote */

    SIO_FLAGS(buf) &= ~SIOF_INSTRING;

    for (;;) {
	if (p >= lim)
	    break;	/* broken string */

	c = *p;		/* fetch next character */

	if ((nl = get_eoln(p, lim, buf))) {
	    if (nl == -1) break;
	    else {
		*pp = p+nl;
		return 0;	/* error: newline in string */
	    }
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
	    if ((nl = get_eoln(p+1, lim, buf))) {
	        if (nl == -1) break;
	        else {
	            p += 1+nl;
		    SIO_LINENUM(buf)++;
		    continue;
	        }
	    } else { 
	    pmem = p;
	    c = escaped_char(&pmem);
	    if (pmem >= lim)
		break;
	    p = pmem;
	}
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
#if !defined(SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS) && defined(__MWERKS__)
#pragma unused(endc)
#endif
    int   val;

    *pp += inc;			/* skip over initial segment */
    if (*pp >= lim)
	return -1;

    val = **pp;
    if (val == '\\')
	val = escaped_char(pp);
    else if (val == LF || val == CR)
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
    UCHAR *p, *lim, *tokstart, *oldp;
    int   eossave;
	int ty;
	double dec_val;

    skip_trailing_lf(buf);
    
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
	CHECK_FOR_POSSIBLE_SPLIT(p);
	if (SIO_FLAGS(buf) & SIOF_INQATOM) {
	    oldp = p;
	    p--;		/* fake out single quote */
	    quoted_atom(vpTokType, vpTokVal, tpTokVal, &p, lim, buf);
	    if (p == oldp && *vpTokType != TK_LEXERR) { 
			SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);
			SIO_ERRCODE(buf) = SIOE_READ;
			return 0;
	    }
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
		    else if (*(p + 1) == 'b' && (*(p + 2)=='0' || *(p + 2)=='1')) {
			p += 2;
			make_number(vpTokVal, tpTokVal, (double) binary(&p)); 
			}
				/* IEEE NAN */
		    else if (*(p + 1) == 'n') {		
		    if ( *(p+2) == 'a' && *(p+3) == 'n')
				p += 4;
			else 
				p += 2;
			make_ieee_nan(vpTokVal, tpTokVal); 
			}

				/* IEEE Infinity */
		    else if (*(p + 1) == 'i') {	
		    if (*(p+2) == 'n' && *(p+3) == 'f')
				p += 4;
			else
				p += 2;
			make_ieee_inf(vpTokVal, tpTokVal); 
		    }
				/* Character Constants */
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
				if (decimal(&p, lim, &dec_val, &ty)) {
				    make_numberx(vpTokVal, tpTokVal, dec_val,ty);
				} else {
				    SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);
				    SIO_ERRCODE(buf) = SIOE_READ;
				    return 0;
				}
			}
		}
		else
		{
			if (decimal(&p, lim, &dec_val, &ty)) {
			    make_numberx(vpTokVal, tpTokVal, dec_val,ty);
			} else {
			    SIO_CPOS(buf) = tokstart - SIO_BUFFER(buf);
			    SIO_ERRCODE(buf) = SIOE_READ;
			    return 0;
			}
		}
		CHECK_FOR_POSSIBLE_SPLIT(*p ? p+1 : p);
		break;
	    case SIOC_SPECIAL:
		if (*p == '\'') {
		    CHECK_FOR_POSSIBLE_SPLIT(p+1);
		    oldp = p+1;
		    quoted_atom(vpTokType, vpTokVal, tpTokVal, &p, lim, buf);
		    if (p == oldp && *vpTokType != TK_LEXERR) { 
			SIO_CPOS(buf) = p - SIO_BUFFER(buf);
			SIO_ERRCODE(buf) = SIOE_READ;
			return 0;
		    }
		}
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
		    /*CHECK_FOR_POSSIBLE_SPLIT(p+1);*/
		    if (!quoted_string(vpTokType,vpTokVal, tpTokVal, 
				      &p, lim, buf)) {
			*vpTokType = TK_LEXERR;
			*vpTokVal = SIOL_UNTERM_STRING;
			*tpTokVal = WTP_INTEGER;
		    }
		    if ((SIO_FLAGS(buf) & SIOF_INSTRING)
		     && *tpTokVal == WTP_SYMBOL
		     && *vpTokVal == TK_NIL) {
			SIO_CPOS(buf) = p /*tokstart*/ - SIO_BUFFER(buf);
			SIO_ERRCODE(buf) = SIOE_READ;
			/*SIO_FLAGS(buf) &= ~SIOF_INSTRING;*/
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

#ifdef CW_PLUGIN
{
	extern CompilerLinkerParameterBlockPtr gCPB;
	CWDisplayLines(gCPB, SIO_LINENUM(buf));
}
#endif

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

#if 0
	    /* EOLN Question:  is this code really used?  It looks like C's preprocessor '\', but
	       I don't know how it could be used in Prolog?  In C, its used mostly in large multi-line
	       defines, but the Prolog preprocessor doesn't support defines.
	       
	       I modified the code a little to work with eoln types, but I haven't worked out
	       how to handle the end-of-buffer cases.
	     */
	    if (preprocessing &&
		vTokType == TK_SYMBOL &&
		vTokVal == TK_BACKSLASH &&
		(nl = get_eoln(SIO_BUFFER(buf) + SIO_CPOS(buf), SIO_BUFFER(buf) + SIO_LPOS(buf), buf))) {
		if (nl == -1) {printf("Preprocessing fatal error\n"); exit(0);}
		else {
		    SIO_CPOS(buf) += nl;
		    SIO_LINENUM(buf)++;
		}
	    } else
#endif
	    {
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

    skip_trailing_lf(buf);
    
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
 * Using ILP64 data model (5/24/2020), int/long --> 32bit
 *	https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models 
 *	https://docs.oracle.com/cd/E19620-01/805-3024/lp64-1/index.html
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
#ifdef __LP64__
    int16_t shortval;
    int32_t intval;
#else
    short shortval;
    long  longval;
#endif
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
#ifdef __LP64__
	    s = (UCHAR *) &intval;
	    endofval = s + sizeof (intval);
#else
	    s = (UCHAR *) &longval;
	    endofval = s + sizeof (longval);
#endif
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
#ifdef __LP64__
	    make_number(&vNum, &tNum, (double) intval);
#else
	    make_number(&vNum, &tNum, (double) longval);
#endif
	    break;
	case TK_ULONG:
	case TK_UINT:
#ifdef __LP64__
	    make_number(&vNum, &tNum, (double) (uint32_t) intval);
#else
	    make_number(&vNum, &tNum, (double) (unsigned long) longval);
#endif
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
 * get_eoln_str() returns a pointer to a string containing the end-of-line characters for
 * the stream.
 */
static const char *get_eoln_str(UCHAR *buf)
{
    const char *eoln_str;
    
    switch(SIO_EOLNTYPE(buf) & SIOEOLN_WRITE_MASK) {
    case SIOEOLN_WRITE_CRLF:
	eoln_str = CRLFSTR;
	break;
    case SIOEOLN_WRITE_CR:
	eoln_str = CRSTR;
	break;
    case SIOEOLN_WRITE_LF:
	eoln_str = LFSTR;
	break;
    default:
	eoln_str = 0;
	break;
    }
	    
    return eoln_str;
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
 * Using ILP64 data model (5/24/2020), int/long --> 32bit
 *	https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models 
 *	https://docs.oracle.com/cd/E19620-01/805-3024/lp64-1/index.html
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
#ifdef __LP64__
    int  intval;
    unsigned int  uintval;
#endif
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
#ifdef __LP64__
	    endofval = sval + sizeof (intval);
#else
	    endofval = sval + sizeof (longval);
#endif
	    break;
	case TK_ULONG:
	case TK_UINT:
	    if (!getdouble(&doubleval, v3, t3)) {
		SIO_ERRCODE(buf) = SIOE_INARG;
		FAIL;
	    }
	    ulongval = (unsigned long) doubleval;
	    sval = (UCHAR *) &ulongval;
#ifdef __LP64__
	    endofval = sval + sizeof (uintval);
#else
	    endofval = sval + sizeof (ulongval);
#endif
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
	if (*a == '/' && *(a+1) == '*') 
		break;
	if (*a == '.') 
		break;
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
    else if (strcmp((char *)atom, ".") == 0) {
	mask = 0;
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
		case LF:
		    *b++ = 'n';
		    break;
		case CR:
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
	    else if (fmt_type == FMT_INT) {
	        if (dblval > (double) INT_MAX)
	            sprintf((char *)buf, (char *)fmt, (unsigned int) dblval);
			else sprintf((char *)buf, (char *)fmt, (int) dblval);
	    } else
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
 * sio_sprintf_number(Number, Buffer, Length)
 */

int sio_sprintf_number(void)
{
    PWord v1, v2, v3, v;
    int   t1, t2, t3, t;
#ifndef DoubleType
    PWord functor;
    int arity;
#endif
    UCHAR *buf;
    double dblval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    buf = (UCHAR *) (wm_H + 1);

    switch (t1) {
	case WTP_INTEGER:
	    sprintf((char *)buf, "%ld", v1);
	    break;
#ifndef DoubleType
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v1);
	    w_get_functor(&functor, v1);
	    if (arity == 4 && functor == TK_DDOUBLE) {
			int i;
			for (i = 0; i < 4; i++) {
		    	w_get_argn(&v, &t, v1, i + 1);
		    	*(((short *) &dblval) + i) = (short) v;
			}
	    }
	    else
			FAIL;

	    if (is_ieee_nan(dblval))
			sprintf((char *)buf, "0nan");
	    else if (is_ieee_inf(dblval)) {
			if (dblval < 0)
				sprintf((char *)buf, "-0inf");
			else
				sprintf((char *)buf, "0inf");
		}
	    /* for small integral doubles, print with a trailing ".0"
	       using %.1f, otherwise use %.10g */
	    else if (floor(dblval) == dblval
				&& dblval > -10000000000.0
				&& dblval < 10000000000.0)
			sprintf((char *)buf, "%.1f", dblval);
	    else
			sprintf((char *)buf, "%.10g", dblval);
	    break;
#else
	case WTP_DOUBLE:
	    w_get_double(&dblval, v1);

	    if (is_ieee_nan(dblval))
			sprintf(buf, "0nan");
	    else if (is_ieee_inf(dblval)) {
			if (dblval < 0)
				sprintf(buf, "-0inf");
			else
				sprintf(buf, "0inf");
		}
	    /* for small integral doubles, print with a trailing ".0"
	       using %.1f, otherwise use %.10g */
	    else if (floor(dblval) == dblval
		&& dblval > -10000000000.0
		&& dblval < 10000000000.0)
		sprintf(buf, "%.1f", dblval);
	    else
		sprintf(buf, "%.10g", dblval);
	    break;
#endif
	default:
	    FAIL;
    }

    w_mk_uia_in_place(&v, &t, buf);

    if (w_unify(v2, t2, v, t) 
    		&& w_unify(v3, t3, (PWord) strlen((char *)buf), WTP_INTEGER))
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
 | sio_nl(Stream) -- writes out a new line to the given stream
 | Stream is the input argument.  It should contain an open stream.
 *-----------------------------------------------------------------------------*/
int
sio_nl(void)
{
    PWord v1;
    int   t1;
    UCHAR *buf, *b, *l;
    const UCHAR *s, *eoln_str;

    w_get_An(&v1, &t1, 1);

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

    eoln_str = (UCHAR *)get_eoln_str((UCHAR *)buf);

    b = SIO_BUFFER(buf) + SIO_CPOS(buf);
    l = SIO_BUFFER(buf) + SIO_BFSIZE(buf);
    s = eoln_str + SIO_AUX(buf);
    
    while (b < l && *s) {
	*b++ = *s++;
    }

    if (*s)
	SIO_AUX(buf) = s - eoln_str;
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
    int   eoln_read, read_length, nl;

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

    skip_trailing_lf(buf);

    cpos = wpos = SIO_CPOS(buf);
    lpos = SIO_LPOS(buf);
    if (cpos >= lpos) {
		SIO_ERRCODE(buf) = SIOE_READ;
		FAIL;
    }

    nl = 0;

    while (wpos < lpos && !(nl = get_eoln(SIO_BUFFER(buf) + wpos, SIO_BUFFER(buf) + lpos, buf))) wpos++;
    
    if (wpos == cpos && nl <= 0) {
	    SIO_ERRCODE(buf) = SIOE_READ;
	    FAIL;
    }
    	
    eoln_read = (wpos <= lpos && nl > 0);
    
    read_length = wpos - cpos;
     
    w_mk_len_uia(&vBuf, &tBuf, SIO_BUFFER(buf) + cpos, (size_t)read_length);
    
    if (eoln_read) {		/* end of line read */
		SIO_CPOS(buf) = wpos + nl;
		SIO_LINENUM(buf) += 1;
		SIO_COLUMN(buf) = 0;
    }
    else {					/* end of line not read */
		SIO_CPOS(buf) = wpos;
		SIO_COLUMN(buf) += read_length;
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

    for (p = bufend; p >= bufstart && !scan_eoln((char *)p, SIO_EOLNTYPE(buf) & SIOEOLN_READ_MASK); p--) ;

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
