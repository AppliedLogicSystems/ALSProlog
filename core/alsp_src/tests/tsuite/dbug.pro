/*
 * dbug.pro	-- program that might cause debugger bug
 *
 * Chris White came up with this little program which caused
 * the debugger to crash (premerge debugger).  The problem has
 * since been fixed, but this little program should be run
 * occassionally in the debugger to make sure everything else
 * is still ok.  It should ultimately fail.
 */

a(X) :- b(X), c(X), d(X), e(X).

c(X) :- !, f(X), g(X), h(X).

b(_).
d(_).
f(_).
g(_).
h(_).
e(X) :- write(X), i(X), j(X).
