%% cref_test1.pro

:-[cref_test2, cref_test3].

module tmod.
use vmod.

export a/0.

a :- b.  
b :- c, u, a.
c :- kmod:q, d.

x(0).
x(1).
x(2).

endmod.

module vmod.
e.
endmod.
