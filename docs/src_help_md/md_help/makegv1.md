—-
title: 'make_gv/1'
predicates:
 - 'make_gv/1' : create named global variable and access method
 - 'make_det_gv/1' : create named global variable and access methods which preserves instantiations of structures
 - 'free_gv/1' : release store associated with named global variable
—-
`make_gv/1` `—` create named global variable and access method

`make_det_gv/1` `—` create named global variable and access methods which preserves instantiations of structures

`free_gv/1` `—` release store associated with named global variable


## FORMS

make_gv(Name)

make_det_gv(Name)

free_gv(Name)


## DESCRIPTION

make_gv/1 allocates an internal global variable and creates two access predicates called setNAME/1 and getNAME/1 where NAME is the atom Name. These access predicates are installed in the module from which make_gv/1 is called.

The setNAME/1 predicate is used to set the allocated global variable to the term given to setNAME as its only argument. This operation is safe in that the contents of the global variable will survive backtracking without any dangling references. Care should be taken when using these global variables with backtracking as it is easy to create a ground structure in which &quot; holes &quot; will appear upon backtracking. These holes are uninstantiated variables where there used to be a term. They are caused by some bit of non-determinism when creating the term. If the non-determinism is removed via cut prior to a global variable operation, these &quot; holes &quot; will often not show up upon backtracking. If the non-determinism is removed after the global variable operation takes place, these holes will very likely show up. The reason that this is so is because the global variable mechanism will(as a consequence of making the structure safe to backtrack over) eliminate the ability of cut to discriminate among those trail entries which may be safely cut and those which are needed in the event of failure.

In situations where this is a problem, a call to copy_term/2 may be used to create a copy of the term prior to setting the global variable. The instantiation of the term that exists at the time of the copy will be the instantiation of the term which survives backtracking over the copy operation.

Also of interest is the time complexity of the set operation. So long as the argument to setNAME/1 is a non-pointer type, that is a suitably small integer or certain types of atoms(the non-UIA variety), the set operation is a constant time operation. Otherwise it requires time linearly proportional to the current depth of the choice point stack.

The getNAME/1 predicate created by make_gv/1 is used to get the contents of one of these global variables. The contents of the global variable is unified with the single parameter passed to getNAME/1.

make_det_gv/1 creates access methods just like make_gv/1 but the setNAME/1 method avoids the problems referred to above concerning certain instantiations in structure becoming undone. It does this by making a copy of the term prior to setting the global variable. Making a copy of the term has the disadvantage of the increased space and time requirements associated with making copies.

free_gv/1 removes access methods created by make_gv/1 and frees up the global variable.


## EXAMPLES

:- make_gv(' _demo ') .%% Create get_demo/1 and set_demo/1.

print_demo(N) :- get_demo(X), printf(' demo%d : %t\n ', [N, X ]) .

demo1 :- demo1(_) .

demo1 :- print_demo(1) .

demo1(_) :- X = f(Y),(Y = i ; Y = j), set_demo(X), print_demo(1), fail.

demo2 :- demo2(_) .

demo2 :- print_demo(2) .

demo2(_) :- X = f(Y),(Y = i ; Y = j), !, set_demo(X), print_demo(2), fail.

demo3 :- demo3(_) .

demo3 :- print_demo(3) .

demo3(_) :- X = f(Y),(Y = i ; Y = j), set_demo(X), !, print_demo(3), fail.

demo4 :- demo4(_) .

demo4 :- print_demo(4) .

demo4(Y) :- X = f(Y),(Y = i ; Y = j), !, set_demo(X), print_demo(4), fail.

demo5 :- _ = f(Y), set_demo([a ]), demo5(Y) .

demo5 :- print_demo(5) .

demo5(Y) :- X = f(Y),(Y = i ; Y = j), !, set_demo(X), print_demo(5), fail.

demo6 :- set_demo([a ]), _ = f(Y), demo6(Y) .

demo6 :- print_demo(6) .

demo6(Y) :- X = f(Y),(Y = i ; Y = j), !, set_demo(X), print_demo(6), fail.

demo7 :- demo7(_) .

demo7 :- print_demo(7) .

demo7(_) :- X = f(Y),(Y = i ; Y = j), copy_term(X, Z),

set_demo(Z), !, print_demo(7), fail.

demo :- demo1, nl, demo2, nl, demo3, nl,

demo4, nl, demo5, nl, demo6, nl,

demo7.

The above program demonstrates the subtelties of combining global variables with backtracking. Here is a sample run of this program :

```
?- demo.
demo1:f(i)
demo1:f(j)
demo1:f(_A)
demo2:f(i)
demo2:f(i)
demo3:f(i)
demo3:f(_A)
demo4:f(i)
demo4:f(i)
demo5:f(i)
demo5:f(_A)
demo6:f(i)
demo6:f(i)
demo7:f(i)
demo7:f(i)
Ineachofthesesevendifferenttests,somenon-determinismisintroducedthroughtheuseof;/2.
demo1makesnoattempteliminatethisnon-determinism.Yettheresultsmightbesomewhatsurprising.set_demo/1iscalledtwice;oncewithXinstantiatedtof(i),thesecondtimewithXinstantiatedtof(j).Yetwhenwefailoutofdemo1/1,print_demo/1reportsthe&quot;demo&quot;variabletohaveanuninstantiatedportion.
demo2eliminatesthenon-determinisminastraightforwardfashionthroughtheuseofacut.Herethef(i)ismadeto&quot;stick&quot;.
demo3isaslightvariationondemo2.Itshowsthateliminatingdeterminismaftersettingtheglobalvariableistoolatetomaketheinstantiations&quot;stick&quot;.
demo4issimilartodemo3,butshowsthatitisalrightforYtobe&quot;older&quot;thanthestructurecontainingit.
demo5showsthataninterveningglobalvariableoperationmayscrewthingsupbymakingYliveinaportionoftheheapwhichmustbetrailedwhenYisbound.ThecutpriortosettingtheglobalvariableisnotpermittedtoremovethetrailentrywhicheventuallycausesYtoloseitsinstantiation.
demo6showsthatcreatingthevariableaftertheglobalvariableoperationhasthesameeffectasdemo4.
demo7demonstratesatechniquethatmaybeusedtoalwaysmakeinstantiations&quot;stick&quot;.Itcreatesanewcopyofthetermandcallsset_demo/1withthisnewcopy.
Ifthecalltomake_gv/1atthetopofthefilewerereplacedwithacalltomake_det_gv/1,thenalloftheinstantiationswould&quot;stick&quot;asmake_det_gv/1automaticallymakesacopyofthetermthusdoingimplicitlywhatdemo7doesexplicitly.
```

## BUGS

free_gv/1 does not work for access methods created by make_det_gv/1.


## SEE ALSO

- `make_hash_table/1`  
`copy_term/2`  
`mangle/3.
