/*=============================================================
 |				socket_test.pro
 |			Copyright (c) 1993 Applied Logic Systems, Inc.
 |
 |		Tests for socket-based stream communication
 |
 |		Developed by Kevin Jiang
 *============================================================*/

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%   Simple primitive tests  %%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        /*---------------------------------------------------------*
         |     INTERNET   D O M A I N                              |
         *---------------------------------------------------------*/

isr :-                  /* Internet Stream Read */
	open(socket('', af_inet, sock_stream),read,[],SD),
	get_byte(SD, B1),
	get_byte(SD, B2),
	get_byte(SD, B3),
	get_byte(SD, B4),
	pbi_write([b1=B1,b2=B2,b3=B3,b4=B4]),pbi_nl,
	sio_close(SD).

isw :-                  /* Internet Stream Write */
	write('To: '),
	read(Target),
	open(socket(Target, af_inet, sock_stream),write,[],SD),
	put_byte(SD, 0'h),
	put_byte(SD, 0'e),
	put_byte(SD, 0'l),
	put_byte(SD, 0'o),
	flush_output(SD),
	sio_close(SD).

idr :-                  /* Internet Datagram Read */
	open(socket('', af_inet, sock_dgram),read,[],SD),
	get_byte(SD, B1),
	get_byte(SD, B2),
	get_byte(SD, B3),
	get_byte(SD, B4),
	pbi_write([b1=B1,b2=B2,b3=B3,b4=B4]),pbi_nl,
	sio_close(SD).

idw :-                  /* Internet Datagram Write */
	write('To: '),
	read(Target),
	open(socket(Target, af_inet, sock_dgram),write,[],SD),
	put_byte(SD, 0'h),
	put_byte(SD, 0'e),
	put_byte(SD, 0'l),
	put_byte(SD, 0'o),
	flush_output(SD),
	sio_close(SD).

        /*---------------------------------------------------------*
         |     UNIX   D O M A I N                                  |
         *---------------------------------------------------------*/

usr :-				/* Unix Stream Read */
	open(socket('/tmp/beethoven', af_unix, sock_stream),read,[],SD),
	get_byte(SD, B1),
	get_byte(SD, B2),
	get_byte(SD, B3),
	get_byte(SD, B4),
	pbi_write([b1=B1,b2=B2,b3=B3,b4=B4]),pbi_nl,
	sio_close(SD).


usw :-				/* Unix Stream Write */
	open(socket('/tmp/beethovenaf_unix_socket'),write,[],SD),
	put_byte(SD, 0'h),
	put_byte(SD, 0'e),
	put_byte(SD, 0'l),
	put_byte(SD, 0'o),
	flush_output(SD),
	sio_close(SD).


udr :-				/* Unix Datagram Read */
	open(socket('/tmp/beethoven', 'af_unix', 'sock_dgram'),read,[],SD),
	get_byte(SD, B1),
	get_byte(SD, B2),
	get_byte(SD, B3),
	get_byte(SD, B4),
	pbi_write([b1=B1,b2=B2,b3=B3,b4=B4]),pbi_nl, flush_output,
	sio_close(SD).

udr2 :-				 
	open(socket('/tmp/beethoven', 'af_unix', 'sock_dgram'),read,[],SD),
	udr2l(SD).

udr2l(SD) :-
	get_byte(SD, B),
	pbi_write(byte=B),pbi_write('--'),
	put_byte(0'>),put_byte(B), flush_output,
	udr2l(SD).



udw :-				/* Unix Datagram Write */
	open(socket('/tmp/beethovenaf_unix_socket', 'af_unix', 'sock_dgram'),write,[],SD),
	put_byte(SD, 0'h),
	put_byte(SD, 0'e),
	put_byte(SD, 0'l),
	put_byte(SD, 0'o),
	flush_output(SD),
	sio_close(SD).

udw2 :-
	open(socket('/tmp/beethoven', 'af_unix', 'sock_dgram'),write,[],SD),
	put_byte(0'>),
	get_byte(B),
	udw2l(B, SD).

udw2l(0'q, SD) :-
	flush_output(SD),
	sio_close(SD).

udw2l(B, SD) :-
	put_byte(SD, B),
	flush_output(SD),
	put_byte(0'>),
	get_byte(B0),
	udw2l(B0, SD).


		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%   Character Level Tests   %%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        /*---------------------------------------------------------*
         |     INTERNET   D O M A I N                              |
         *---------------------------------------------------------*/

inet_read_socket(socket('',af_inet, sock_stream)).
inet_write_socket(socket(Target,af_inet, sock_stream))
	:-
	get_target_machine(Target).

get_target_machine(Target)
	:-
	write('Target Machine:'),flush_output,
	read(Target).

i_csr :-                  /* Internet Stream Read */
	inet_read_socket(SocketDesc),
	open(SocketDesc, read, [], InStream),
	i_csr(InStream),
	close(InStream).

i_csr(InStream) :-
	get_char(InStream,C),
	disp_print(C),
	i_csr(InStream).

disp_print(-1) :-!.
disp_print(C)
	:-
	put_char(C).

i_csw :-                  /* Internet Stream Write */
	inet_write_socket(SocketDesc),
	open(SocketDesc, write, [], OutStream),
	i_csw(OutStream),
	close(OutStream).

i_csw(OutStream)
	:-
	get_char(C),
	disp_i_csw(C, OutStream).

disp_i_csw(0'\n, OutStream)
	:-
	put_char(OutStream,0'\n),
	flush_output(OutStream),
	put_char(0'>),
	i_csw(OutStream).
disp_i_csw(C, OutStream)
	:-
	put_char(OutStream,C),
	i_csw(OutStream).


/*
idr :-                  /* Internet Datagram Read */
	open(socket('', af_inet, sock_dgram),read,[],InStream),



idw :-                  /* Internet Datagram Write */
	write('To: '),
	read(Target),
	open(socket(Target, af_inet, sock_dgram),write,[],OutStream),
*/
