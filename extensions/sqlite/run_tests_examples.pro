/*============================================================================*
                                run_tests_examples.pro

	Systematically run the available sqlite3 interface tests 
	and examples.

	By default, run_tests_examples/0 pedals thru the test sequence,
	prompting you at each step to go forward.  Just hitting return
	executes the next step.  This is controlled by prompt_next/1.
	If prompt_next(V) is any value other than V=true, the entire sequence 
	is executed at once.
 *============================================================================*/
:-dynamic(prompt_next/1).
%prompt_next(true).
prompt_next(false).

do_next :- prompt_next(true),!,printf('>>>> Next test?', []),get_line(X).
do_next.

run_tests_examples
	:-
	get_cwd(BaseDir),
	assert(base_dir(BaseDir)),

	do_next,
	consult('tests_sqlite3_intf.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['tests_sqlite3_intf.pro','test_sqlite3']),
	test_sqlite3,
	printf('< FINISHED: tests_sqlite3_intf.pro\n=====\n\n'),

consult('tools/dbsetup.pro'),
	do_next,
	change_cwd('tools/singers'),
dbsetup_one('singers.db_spec'),
	consult('crooners.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['crooners.pro','singers_test']),
	singers_test,
	printf('< FINISHED: crooners.pro\n=====\n\n'),
	change_cwd(BaseDir),

	do_next,
	change_cwd('tools/singers'),
	consult('inj_test.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['inj_test.pro','test_insert_with_single_quote']),
	test_insert_with_single_quote,
	printf('< FINISHED: inj_test.pro / test_insert_with_single_quote\n=====\n\n'),

	do_next,
	drop_db(singers_data),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['inj_test.pro','test_insert_bobby_tables']),
	test_insert_bobby_tables,
	printf('< FINISHED: inj_test.pro / test_insert_bobby_tables\n=====\n\n'),
	change_cwd(BaseDir),

	do_next,
	change_cwd('tools/bp'),
dbsetup_one('bp.db_spec'),
	consult('bp_code.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['bp_code.pro','all_db_bp_tests']),
	all_db_bp_tests,
	printf('< FINISHED: bp_code.pro\n=====\n\n'),
	change_cwd(BaseDir),

	do_next,
	change_cwd('examples'),
	consult('extract_anthonies.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['examples/extract_anthonies.pro','setup_aa_db']),

	do_next,
	setup_aa_db,
	printf('< FINISHED: %t created and loaded from %t\n=====\n\n',['anthonies.sqlite3','AA_Lincoln.txt']),
	printf('\n=====\n> QUERIES:\n=====\n',[]),
	all_aa_queries,
	printf('< FINISHED: anthonies.pro \n=====\n\n'),
	change_cwd(BaseDir),

	do_next,
	change_cwd('examples'),
	consult('do_ca_vt_db.pro'),
	printf('=====\n> STARTING: %t:\t%t\n=====\n',['examples/do_ca_vt_db.pro','setup_births_db_ca_vt']),
	setup_births_db_ca_vt,
	printf('< FINISHED: %t created and loaded from %t\n=====\n\n',['db_ca_vt_2016_2021.sqlite3.sqlite3','ca_vt_births_2016_2021.csv']),
	printf('\n=====\n> QUERIES:\n=====\n',[]),
	query_births0,
	printf('< FINISHED: do_ca_vt_db.pro \n=====\n\n'),
	change_cwd(BaseDir),
true.