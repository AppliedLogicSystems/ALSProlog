/*=====================================================================*
 |		bsio.h	
 |	Copyright (c) 1991-1995 Applied Logic Systems, Inc.
 |
 |		-- include file for stream I/O primitives
 |
 | Author: Kevin A. Buettner
 | Creation: 10/8/91
 *=====================================================================*/

/*----------------------------------------------------------------------*
 | A stream descriptor will be a UIA which will contain a buffer and certain
 | information pertaining to the stream.
 |
 | In order to manage the buffer for both ordinary character/byte I/O as
 | well as lexical analysis, the UIA will contain some additional information.
 | The following are indices into the stream descriptor.  The values at these
 | locations are as follows:
 |
 |	SIO_CPOS	--	index of next character to be read/written
 |	SIO_TPOS	--	index of start of previous lexical entity
 |					(token). This value is used for error reporting.
 |	SIO_LPOS	--	number of characters in current buffer
 |	SIO_BUFPOS	--	number of characters in file prior to current
 |					buffer.  This value coupled with SIO_CPOS
 |					may be used to get the current position in
 |					the file.
 |	SIO_LINENUM	-- 	line number of character at SIO_CPOS
 |	SIO_ERRCODE	--	place to store error value should an error
 |					occur in performing operation
 |	SIO_ERRNO	-- 	place to store errno value should an error
 |					occur while performing I/O operation
 |	SIO_FLAGS	--	place to put flags if necessary
 |	SIO_FD		--	place where file descriptor is stored
 |	SIO_COMMENT	--	C-Style comment nesting level count
 |	SIO_ELINENUM	--	line number where error starts
 |	SIO_AUX		--	auxillary field used by certain primitives
 |					including sio_put_atom
 |	SIO_AUX2	--	auxillary field for other types of streams
 |	SIO_AUX3	--	auxillary field for other types of streams
 |	SIO_AUX4	--	auxillary field for other types of streams
 |	SIO_BFSIZE	--	size of a full buffer
 |	SIO_TYPE	--	Numeric stream type code
 |	SIO_COLUMN	--	number of character in current line (this
 |					value will be zero at the start of a line)
 |					This value is valid only on read-only streams
 |	SIO_EOLNTYPE	--	End of line type.
 |
 | Certain types of streams need a "header" to be written out before the actual
 | data to be written.  SIO_HEADER(buf,index) provides longword access to
 | elements of the header.
 |
 | SIO_BUFFER(buf) provides a character pointer to the start of the character
 | buffer which immediately follows the header.
 |
 | At least one longword of padding will follow the actual buffer.  This
 | longword may be modified as desired.
 |
 | SIO_SDSIZE(n) gives the size of the uia to allocate given buffer size n.
 |
 | Note: positions 1 and 14 are available as they used to be used for storing
 | the TPOS value and the half buffer size respectively.
 *----------------------------------------------------------------------*/

#define SIO_CPOS(b)		(*((long *)(b) + 0))
#define SIO_LPOS(b)		(*((long *)(b) + 2))
#define SIO_BUFPOS(b)		(*((long *)(b) + 3))
#define	SIO_LINENUM(b)		(*((long *)(b) + 4))
#define SIO_ERRCODE(b)		(*((long *)(b) + 5))
#define SIO_ERRNO(b)		(*((long *)(b) + 6))
#define SIO_FLAGS(b)		(*((long *)(b) + 7))
#define SIO_FD(b)		(*((long *)(b) + 8))
#define SIO_COMMENT(b)		(*((long *)(b) + 9))
#define SIO_ELINENUM(b)		(*((long *)(b) + 10))
#define SIO_AUX(b)		(*((long *)(b) + 11))
#define SIO_AUX2(b)		(*((long *)(b) + 12))
#define SIO_AUX3(b)		(*((long *)(b) + 13))
#define SIO_BFSIZE(b)		(*((long *)(b) + 15))
#define SIO_TYPE(b)		(*((long *)(b) + 16))
#define SIO_COLUMN(b)		(*((long *)(b) + 17))
#define SIO_EOLNTYPE(b)		(*((long *)(b) + 18))

#define SIO_LAST	19

#define SIO_HDSIZE	8		/* size of header (in long words) */

#define SIO_HEADER(b,i) 	(*((long *)(b) + SIO_LAST + i))
#define SIO_HEADER_PTR(b,i) ((long *)(b) + SIO_LAST + i)
#define SIO_BUFFER(b) 		((unsigned char *)(b) + sizeof(long)*(SIO_LAST+SIO_HDSIZE))

#define SIO_SDSIZE(n)		(sizeof(long)*(SIO_LAST+SIO_HDSIZE) + n)

/*---------------------------------------------------------
 | The following defines are for use in assembly language
 *--------------------------------------------------------*/

#define sio_CPOS		0
#define sio_TPOS		4
#define sio_LPOS		8
#define sio_BUFPOS		12
#define sio_LINENUM		16
#define sio_ERRCODE		20
#define sio_ERRNO		24
#define sio_FLAGS		28
#define sio_FD			32
#define sio_COMMENT		36
#define sio_ELINENUM		40
#define sio_AUX			44
#define sio_AUX2		48
#define sio_AUX3		52
#define sio_BFSIZE		60
#define sio_TYPE		64
#define sio_COLUMN		68
#define sio_EOLNTYPE		72

#define sio_BUFFER	4*(SIO_LAST+SIO_HDSIZE)
		/* ^ Assembly language translator doesn't understand */
		/* sizeof(long) */

/*
 *	Stream type codes
 */

#define SIO_TYPE_FILE		0
#define SIO_TYPE_SYSVQ		1
#define	SIO_TYPE_SYSVSTREAM	2
#define	SIO_TYPE_SSBQ		3
#define	SIO_TYPE_SOCKET_STREAM	4
#define SIO_TYPE_SOCKET_DGRAM	5
#define SIO_TYPE_PROLOG_MANAGED	6

#define NONSEEKABLE(x) ((x) != SIO_TYPE_FILE)

/*-------------------------------------------------------------------------
 | Values for SIO_ERRCODE
 |
 | Should a primitive in bsio.c fail, the reason for failure may be determined
 | by examining SIO_ERRCODE and possible SIO_ERRNO.  We could potentially
 | combine these values for the Unix operating system since the errno values
 | are quite small.  Other operating systems, such as VMS for example, have
 | error values which are quite large and harder to multiplex.
 |
 | Note: Error codes 10, 11, 12  should be reused.
 *------------------------------------------------------------------------*/

#define SIOE_NORMAL	0	/* normal successful completion */
#define SIOE_SYSCALL	1	/* error occurred during system call. */
				/* See SIO_ERRNO to get exact error */
#define SIOE_INARG	2	/* input argument invalid */
#define SIOE_WRITE	3	/* buffer contents need to be written */
#define SIOE_INVALIDTYPE 4	/* Type of stream invalid for requested
				   operation */
#define SIOE_READ	5	/* entire buffer needs to be read */
#define SIOE_ILLWRITE	6	/* attempt made to write to non-writable
				   stream */
#define SIOE_ILLREAD	7	/* attempt made to read from non-readable
				   stream*/
#define SIOE_EOF	8	/* End of file encountered on read */
#define SIOE_UNGET	9	/* Unable to back up required amount on unget */
#define SIOE_ILLSEEK	13	/* attempt to seek on non-seekable stream */
#define SIOE_NOTREADY	14	/* I/O operation would block on a non-blocking
				   stream */
#define SIOE_PARTNUM	15	/* During a call to sio_get_number, only
				   part of a number of the size requested
				   was able to be read */
#define SIOE_INTERRUPTED 16	/* A system call was interrupted by a signal */

/*-------------------------------------------------------------------------*
 | File open modes are given by the following codes.  The Unix operating
 | system modes are not used due to the fact that the exact numbers used
 | may differ between Unix versions and also that the Prolog system
 | needs to be portable to operating systems beyond Unix.
 *-------------------------------------------------------------------------*/

#define SIOM_READ		0	/* open for read access */
#define SIOM_READ_WRITE	1	/* open for read/write access */
#define SIOM_WRITE		2	/* open for write access (create) */
#define SIOM_APPEND		3	/* open for write access and position at end */

/*
 * Flags associated with streams
 */

#define SIOF_READ		0x01	/* file open for reading */
#define SIOF_WRITE		0x02	/* file open for writing */
#define SIOF_DIRTY		0x04	/* buffer contents modified */
#define SIOF_DONTBLOCK	0x08	/* don't permit I/O operation to block */
#define SIOF_LCOMMENT	0x10	/* line comment encountered, eoln not */
#define SIOF_BLINE		0x20	/* line buffering in effect */
#define SIOF_BBYTE		0x40	/* byte buffering in effect */
#define SIOF_EOF		0x80	/* buffer contains end-of-file */
#define SIOF_INATOM		0x100	/* current position in very long atom */
#define SIOF_INSTRING	0x200	/* current position in very long string */
#define SIOF_INQATOM	0x400	/* current position in very long quoted atom */
#define SIOF_PREPROC	0x800	/* current position in midst of preprocessor
									line */
#define SIOF_NEEDACCEPT	0x8000	/* accept required before read on stream
				   					socket */

#define SIOF_CLONE	(SIOF_NEEDACCEPT) 	/* flags which need to be cloned */

#define SIOF_GOT_CR	0x10000		/* last byte read was a carriage return */
#define SIOF_PUT_CR	0x20000		/* last byte written was a carriage return */

/*-------------------------------------------------------------------------*
 | Buffering modes:	These are the numeric codes that sio_open receives
 |			to determine the buffering flags to set (see above)
 *-------------------------------------------------------------------------*/

#define SIOB_BLOCK		0	/* block mode */
#define SIOB_LINE		1	/* line mode */
#define SIOB_BYTE		2	/* byte mode */
#define SIOB_DONTBLOCK	4	/* or'd in with one of the above */


/* End-Of-Line Type modes */

#define SIOEOLN_READ_CRLF	0	/* Read DOS/Internet TTY style lines. */
#define SIOEOLN_READ_CR		1	/* Read Macintosh style lines. */
#define SIOEOLN_READ_LF		2	/* Read Unix style lines. */
#define SIOEOLN_READ_UNIV	3	/* Read any end-of-line style. */

#define SIOEOLN_WRITE_CRLF	0	/* Write DOS/Internet TTY style lines. */
#define SIOEOLN_WRITE_CR	4	/* Write Macintosh style lines. */
#define SIOEOLN_WRITE_LF	8	/* Write Unix style lines. */

#define SIOEOLN_READ_MASK	3	/* A mask for extracting end-of-line read values. */
#define SIOEOLN_WRITE_MASK	12	/* A mask for extracting end-of-line write values. */

/*
 * The following defines give the functor and arity for stream descriptors
 */

#define	SIO_SD_FUNCTOR		TK_STREAM_DESCRIPTOR
#define SIO_SD_ARITY		20
#define SIO_SD_STREAM_NAME 	4	/* stream name argument number */

/*-------------------------------------------------------------------------*
 | The following are portions of masks used to determine character types
 | in the lexical analyzer
 |
 | Only one of SIOC_SINGLE, SIOC_GRAPHIC, SIOC_UPPERCASE, SIOC_LOWERCASE,
 | or SIOC_DECIMAL will be set per character.  These are the permissible
 | start of token types.  The meanings of these lexical classes are as follows
 |
 |	SIOC_SINGLE		-- character is to be a token by itself
 |	SIOC_GRAPHIC	-- character is one of the graphic characters.  It
 |			   			may combine with other graphic characters to form
 |			   			a symbol.
 |	SIOC_UPPERCASE	-- character is one of the uppercase letters. underscore
 |			   			is considered to be an uppercase letter.  An
 |			   			uppercase letter, of course, starts a variable
 |	SIOC_LOWERCASE	-- character is a lowercase letter
 |	SIOC_DECIMAL	-- character is one of 0-9
 |	SIOC_SPECIAL	-- any character which does not fit in the above
 |			   			classes.  It may be a quote character.
 |
 | A character may also have some of one or more of the following bit masks
 | or'd into its character mask.
 |
 |	SIOC_HEXLOWER	-- character is a lowercase hexadecimal letter (a-f)
 |	SIOC_HEXUPPER	-- character is an uppercase hexadecimal letter (A-F)
 |	SIOC_PLUSMINUS	-- character is either a plus sign or a minus sign
 |	SIOC_E			-- character is an E (either uppercase or lower)
 |	SIOC_OCTAL		-- character is an octal digit
 |	SIOC_WHITESPACE	-- character is a whitespace character
 |	SIOC_ESCCHAR	-- character is a needs to be escaped with a backslash
 |					   when printed (used in sio_qatom)
 *-------------------------------------------------------------------------*/

#define SIOC_SINGLE		0x0001
#define SIOC_GRAPHIC		0x0002
#define SIOC_UPPERCASE		0x0004
#define SIOC_LOWERCASE		0x0008
#define SIOC_DECIMAL		0x0010
#define SIOC_SPECIAL		0x0020

#define	SIOC_HEXLOWER		0x0100
#define SIOC_HEXUPPER		0x0200
#define SIOC_PLUSMINUS		0x0400
#define SIOC_E			0x0800
#define SIOC_OCTAL		0x1000
#define SIOC_WHITESPACE		0x2000
#define SIOC_ESCCHAR		0x4000

/*
 * Composites
 */

#define SIOC_TOKENSTART (SIOC_SINGLE | SIOC_GRAPHIC | SIOC_UPPERCASE | \
			SIOC_LOWERCASE | SIOC_DECIMAL | SIOC_SPECIAL)
#define SIOC_HEXADECIMAL (SIOC_DECIMAL | SIOC_HEXLOWER | SIOC_HEXUPPER)
#define SIOC_ALPHANUMERIC (SIOC_UPPERCASE | SIOC_LOWERCASE | SIOC_DECIMAL)

/*-------------------------------------------------------------------------*
 | Lexical error codes
 |
 | In the event that an occurs in getting the next token, the lexical
 | analyzer will return the symbol 'lexerr' as the token type and one
 | of the following integers as the token value.  These integers can
 | be decoded by the error reporting mechanism.
 *-------------------------------------------------------------------------*/

#define SIOL_UNTERM_STRING		0	/* Unterminated string */
#define SIOL_UNTERM_SYMBOL		1	/* Unterminated (quoted) symbol */
#define SIOL_TOOLONG_CHARCONST	2	/* Character const more than 1 char */
#define SIOL_BADCOMMENT			3	/* Comment unterminated */

#define SIOL_INTERNAL_1		1001	/* Internal Error 1 */
#define SIOL_INTERNAL_2		1002	/* Internal Error 2 */
#define SIOL_INTERNAL_3		1003	/* Internal Error 3 */
#define SIOL_INTERNAL_4		1004	/* Internal Error 4 */

#ifdef SysVIPC
#define SIO_SysVIPC_MTYPE(b) 	SIO_AUX2(b)
#define SIO_SysVIPC_WAIT(b)  	SIO_AUX3(b)
#endif	/* SysVIPC */

#ifdef SSBQ
#define SIO_SSBQ_TARGETNODE(b)	SIO_AUX2(b)
#define SIO_SSBQ_WAIT(b)		SIO_AUX3(b)
#define SIO_SSBQ_INCB(b)		SIO_HEADER(b,0)
#define SIO_SSBQ_INCB_PTR(b)	SIO_HEADER_PTR(b,0)
#define SIO_SSBQ_USEDCS(b)		SIO_HEADER(b,1)
#define SIO_SSBQ_REMCS(b)		SIO_HEADER(b,2)
#define SIO_SSBQ_REMCS_PTR(b)	SIO_HEADER_PTR(b,2)
#endif	/* SSBQ */

#ifdef HAVE_SOCKET
#ifdef __MWERKS__
#define SIO_SOCKET_ADDRESS(b)   (SIO_AUX2(b))
#else
#define SIO_SOCKET_ADDRESS(b)	((char *) SIO_AUX2(b))
#endif
								/* ptr to address structure */
#define SIO_SOCKET_ADDRESS_LEN(b) SIO_AUX3(b)	/* length of above */
#define ALS_STREAM	1
#define ALS_DGRAM	2
#endif	/* HAVE_SOCKET */

#define WINS_INSERT_POS(b)		SIO_HEADER(b,0)
#define WINS_POS_GV(b)			SIO_HEADER(b,1)

/*-------------------------------------------------------------------------*
 | Character constant macros
 |
 | At the time of implementation, character constants were to be represented
 | as 0'C' where C is the character.  A short time later, the draft standard
 | was changed so that character constants would be represented as `C`. (ugh!)
 |
 | I am sufficiently skeptical concerning the backquote approach that I
 | decided to ifdef the whole mess.
 |
 | If SIO_BACKQUOTE_FOR_CHAR_CONSTS is defined, then the backquote mechanism
 | may be used to specify character constants.
 |
 | If SIO_ZERO_QUOTE_FOR_CHAR_CONSTS is defined, then 0'C' will be recognized
 | in source code as a character constant.
 |
 | If both are defined, then both mechanisms may be used.  If neither are
 | defined, then character constants will be disallowed.
 |
 |------ 
 |						12-21-92
 |
 | My skepticism was vindicated.  I now learn that the representation of
 | characters constants will be 0'C where C is the character.  Note that
 | there is no final endquote.  Apparently, the backquote idea was
 | scrapped (hurray!).  But, things could change again, so ....
 | 
 | If SIO_ENDCHAR_REQUIRED_FOR_CHAR_CONSTS is defined, a final quoting 
 | character will be required for character constants.  Note that since there
 | is no final quote character anymore, we will not define it.  I've also
 | taken out SIO_BACKQUOTED_FOR_CHAR_CONSTS.  The code in bsio.c is still
 | ifdef'd in case someone changes their mind yet another time.
 |
 *-------------------------------------------------------------------------*/

#define SIO_ZERO_QUOTE_FOR_CHAR_CONSTS 1
