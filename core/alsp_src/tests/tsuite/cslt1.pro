/*
 * consult1.pro
 *
 * This test will invoke recursive consults of files with pathnames which are
 * in different directories from where we started.  Other files associated
 * with this test are in consult1dir.
 *
 * This test is run, simply by consulting this file.  If everything worked
 * ok, the message 'Test passed' should be printed out.
 */

:- ['cslt1dir/a'].

main :- a.

:- main.
