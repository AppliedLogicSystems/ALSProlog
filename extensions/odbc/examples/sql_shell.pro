/* ========================================================================*
 |            sql_shell.pro
 |        Copyright (c) 1998-99 Applied Logic Systems, Inc.
 |
 |  An example of using the high level prolog interface to ODBC.
 |
 |  Author: Chuck Houpt
 |
 |  Typical Usage:
 |   
 |  ?- sql_init.
 |   
 |   yes.
 |
 |   ?- sql_shell. {Use Manager popup to select Data Source netnews}
 |   { Alternative start:  ?- sql_shell('DSN=netnews').            }
 |   SQL> SELECT Subject FROM comp.lang.prolog
 |   ... Results ...
 |   SQL> Control-D or Z
 |   
 |   yes.
 |   ?- sql_shutdown.
 |   
 |   yes.
 |
 |  Note: ODBC must be initialized (via sql_init) before running sql_shell.
 |    Requires:
 |        consult('odbcintf.psl')  
 |            {equv. at lowlevel: 
 |               load_slib(~...~/alsdir/shared/odbcintf.psl'). }
 |    and
 |        [odbc, prolog_odbc, meta_odbc].
 *===========================================================================*/


/*!-----------------------------------------------------------*
 | sql_shell/0 
 | sql_shell
 | sql_shell
 |
 | - Start sql_shell, prompting for a database connection. 
 *-----------------------------------------------------------*/
sql_shell :-
	sql_open_connection(C, ConnectionString),
	printf('Connected to %t\n', [ConnectionString]),	
	shell_loop(C).

/*!-----------------------------------------------------------*
 | sql_shell/1 
 | sql_shell(ConString)
 | sql_shell(+) 
 |
 | - Start sql_shell, connecting via a connection string. 
 *-----------------------------------------------------------*/
sql_shell(ConString) :-
	sql_open_connection(ConString, OutString, C),
	printf('Connected to %t\n', [OutString]),
	shell_loop(C).

/*!-----------------------------------------------------------*
 | sql_shell/3 
 | sql_shell(DataSource, User, Password)
 | sql_shell(+, +, +)
 |
 | - Start sql_shell, connecting to a predefined data source. 
 *-----------------------------------------------------------*/
sql_shell(DataSource, User, Password) :-
	sql_open_connection(DataSource, User, Password, X),
	printf('Connected to %t\n', [DataSource]),
	shell_loop(C).
	
/*!-----------------------------------------------------------*
 | shell_loop/1 
 | shell_loop(C)
 | shell_loop(+)
 | 
 |	- Outer skin of the sql_shell main loop
 |
 | C is a previously opened connection to a data source.
 | Ensures that the connection is closed, even when an
 | error is raised.
 *-----------------------------------------------------------*/
shell_loop(C) :-
	catch (read_eval_print_loop(C),
	       LoopError,
	       (sql_close_connection(C), throw(LoopError))),
	sql_close_connection(C).

/*!-----------------------------------------------------------*
 | read_eval_print_loop/1 
 | read_eval_print_loop/1 
 | read_eval_print_loop/1 
 |
 |	- core of the sql_shell main loop
 |
 | C is a previously opened connection to a data source.
 | Reads, executes and prints the results of SQL statements, 
 | until end-of-file is seen. 
 *-----------------------------------------------------------*/
read_eval_print_loop(C) :-
	catch ((
		printf('\nSQL> '),
		get_line(Line),
		Line \= end_of_file,
		sql_open_statement(C, Line, S),
		sql_execute_statement(S),
		fetch_all(S),
		%%sql_commit(C),
		sql_close_statement(S)
	), error(sql_error(_, ErrorList), _),
	(
		printf('SQL Error:\n'),
		print_errors(ErrorList)
	)),
	read_eval_print_loop(C).
read_eval_print_loop(_).

/*!-----------------------------------------------------------*
 |	fetch_all/1
 |	fetch_all(S)
 |	fetch_all(+)
 |
 |	- fetches and print rows in an SQL statement
 |
 |	S is a previously opened SQL statement.
 |	Fetches and prints all the rows in S.
 *-----------------------------------------------------------*/
fetch_all(S) :-
	sql_fetch_row(S, R),
	writeq(R), nl,
	fetch_all(S).
fetch_all(_).

/*!-----------------------------------------------------------*
 *-----------------------------------------------------------*/
print_errors([]).
print_errors([error(Code, _, Desc) | Rest]) :-
	printf('%t: %t\n', [Code, Desc]).
