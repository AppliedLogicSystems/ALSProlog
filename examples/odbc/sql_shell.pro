/* sql_shell.pro
   An example of using the high level prolog interface to ODBC.
   ODBC must be initialized (via sql_init) before running sql_shell.

   Typical Usage:
   
   ?- sql_init.
   
   yes.
   ?- sql_shell. or ?- sql_shell('DSN=netnews').
   SQL> SELECT Subject FROM comp.lang.prolog
   ... Results ...
   SQL> Control-D or Z
   
   yes.
   ?- sql_shutdown.
   
   yes.
*/

:- ['odbcintf.psl'].
:- [odbc, prolog_odbc, meta_odbc].

/* sql_shell/0 Prompt the user for a database connection. */
sql_shell :-
	sql_open_connection(C, ConnectionString),
	printf('Connected to %t\n', [ConnectionString]),	
	shell_loop(C).

/* sql_shell/1 connect with a connection string. */	
sql_shell(ConString) :-
	sql_open_connection(ConString, OutString, C),
	printf('Connected to %t\n', [OutString]),
	shell_loop(C).

/* sql_shell/3 connects to a predefined data source. */	
sql_shell(DataSource, User, Password) :-
	sql_open_connection(DataSource, User, Password, X),
	printf('Connected to %t\n', [DataSource]),
	shell_loop(C).
	
/* shell_loop/1 ensures that the connection is closed, even when an
   error is raised. */
shell_loop(C) :-
	catch (read_eval_print_loop(C),
	       LoopError,
	       (sql_close_connection(C), throw(LoopError))),
	sql_close_connection(C).

/* read_eval_print_loop/1 reads, executes and prints the results
   of SQL statements, until the end-of-file is seen. */
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

fetch_all(S) :-
	sql_fetch_row(S, R),
	writeq(R), nl,
	fetch_all(S).
fetch_all(_).

print_errors([]).
print_errors([error(Code, _, Desc) | Rest]) :-
	printf('%t: %t\n', [Code, Desc]).
