/*
 * copylines.pro	-- copy lines from one file to another
 *
 *	Copyright (c) 1992, Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 *
 * Purpose:
 *	This program tests out the get_line and put_line predicates.
 *
 * Running it:
 *	You can either call copylines directly from the shell or, more simply,
 *	you can run the 'test' goal.  This will copy the termcap file to
 *	copylines.out.  You should then diff these two files to make sure
 *	that they are the same.
 *
 * Note:
 *	The structure of this program is different than the structure of
 *	many Prolog programs which copy their input to output, primarily
 *	due to the way in which end-of-file is handled.  get_line/2 does
 *	not return an end-of-file condition other than failing, thus
 *	obviating the need to "prime the pump" by getting some input,
 *	passing this input to some other routine which examines it to
 *	see if an end-of-file condition is found or if it is normal input,
 *	handling the normal input if that is the case, getting the next
 *	bit of input, and tail recursing.  Rather, it gets a line, failing
 *	if there are no more.  If it does not fail, it outputs the current
 *	line and loops.  (See cl0).  When it does fail, it fails to the
 *	one and only choice point created in the loop (by non-system code)
 *	in cl/2.
 */

/*
test :-
	copylines('/etc/termcap','copylines.out').
*/
test :-
	copylines(['testmath.pro', 'compare.pro', 'timeio.pro'], 'copylines.out').



copylines(InFileList,OutFile) 
	:-
	select_test_file(InFileList, InFile),
	open(InFile,read,InStream),
	open(OutFile,write,OutStream),
	cl(InStream,OutStream),
	close(InStream),
	close(OutStream).

copylines(InFile,OutFile) 
	:-
	printf('copylines: no input files exist!!\n', []).

find_file(Name, Name) :-
	exists_file(Name).

find_file(Name, FullName) :-
	builtins:searchdir(SDir),
	pathPlusFile(SDir, Name, FullName),	
	exists_file(FullName).

find_file(Name, _) :-
	printf('File Not Found: %t\n', [Name]),
	fail.

select_test_file([File | InFileList], FullName)
	:-
	find_file(File, FullName),
	!.

select_test_file([_ | InFileList], File)
	:-
	select_test_file(InFileList, File).

cl(InStream,OutStream) :- cl0(InStream,OutStream), !.
cl(_,_).

cl0(InStream,OutStream) :-
	get_line(InStream,Line),
	put_line(OutStream,Line),
	cl0(InStream,OutStream).
