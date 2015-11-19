ALS Prolog provides a method of globally associating values with arbitrary terms
(which occur on the heap).  The associations are immune to backtracking. That is,
once an association is installed, backtracking to a point prior to creation of the association does not undo the association. (However, see the discussion below for fine
points concerning this.) Because both the associated term and value may occur on
the heap, both a term and its associated value can contain uninstatiated variables.

##8.1 ‘Named’ Global Variables

The underlying primitive predicates set_global/2 and get_global/2 defined in the
next section maintain a uniform global association list. This has the disadvantage
that as the number of distint associations to be mainted grows, the performance of
both set_global/2 and get_global/2 will degrade. The facility described in this section avoids this problem by providing individual global variables which are accessed by programmer-specified unary predicates; hence this mechanism is said to provide ‘named global variables.’
````
make_gv/1
make_gv(Name)
make_gv(+)
````
This predicate creates a single (primitive) global variable (see the next section), together with predicates for setting and retrieving its value. If Name is either an
atom or a Prolog string (list of ASCII codes), the call

    make_gv(Name)

allocates a primitive global variable and dynamically defines (asserts clauses for)
two predicates, setNAME/1 and getNAME/1, where NAME is the atom Name or the
atom corresponding to the string Name. The definitions are installed in the module
in which make_gv/1 is called. These two predicates are used, respectively, to set
or get the values of the global variable which was allocated. Here are some examples:
````
?-make_gv(‘_flag’).
yes.
?-set_flag(hithere).
yes.
?-get_flag(X).
X = hithere.
?-make_gv(‘CommonCenter’).
yes.
?-setCommonCenter(travel_now).
yes.
?-getCommonCenter(X).
X = travel_now.
````

##8.2 The Primitive Global Variable Mechanism.

The underlying or primitive global variable mechanism is best described in terms
of a simple implementation point of view. Global variables are value cells with the
following properties:
* They can contain pointers into the heap (but not into the stack)
* These value cells do not lie on either the heap or the stack.
* The pointers contained in these value cells are not affected by either the
backtracking process or the garbage collection process.
The figure below suggests the global variable mechanism.

{ADD PICTURE

Figure. The Global Variables Area and the Heap.

The underlying mechanism is implemented by the following routines:
````
gv_alloc/1
gv_alloc(Num) - allocates a global variable
gv_alloc(+)
gv_free/1
gv_free(Num) - frees a global variable
gv_free(+)
gv_get/2
gv_get(Num, Value) - gets the value of a global variable
gv_get(+, -)
gv_set/2
gv_set(Num, Value) - sets the value of a global variable
gv_set(+, +)
````
These four predicates implement the primitive global variable mechanism. They
achieve an effect often implemented using assertions in the database. The value of
the present mechanism is its greater speed, its separation from the database, and its
ability to deal with terms from the heap which may incorporate uninstatioated variables. Global variables are referred to by unique identifying integers sequentially
starting from 1. The number of available global variables is implementation dependent. Note that the system itself allocates a number of global variables.

gv_alloc(Num) allocates a free global variable and unifies the number of this variable with Num. Since several global variables are used by the system itself, the first call to gv_alloc(Num) normally returns an integer greater than 1. 

gv_free(Num) deallocates global variable number Num, after which Num can be reused by subsequent calls to gv_alloc/1. 

gv_set(Num,Value) sets the value of global variable number Num to be Value, which can be any Prolog term, including partially instantiated terms. Correspondingly, gv_get(Num, Value) unifies Value with the current value of global variable number Num. A call to gv_get(Num, Value) before a call
to gv_set(Num, Value) returns the default value for global variables, which is 0.

Attempts to use gv_set(Num,Value) or gv_get(Num,Value) without a preceding call to gv_alloc(Num) returning a value for the variable Num is an error which will generally cause unpredictable behavior, including system crashes.

The immediate values of global variables survive backtracking and persist across
top level queries. However, if a global variable is set to a structure containing an
unbound variable, say X, which is later bound during a computation, the binding of
X is an ordinary Prolog binding which will not survive either backtracking or return
to the top level of the Prolog shell. Thus variables in a structure which is bound to a global variable do not inherit the globalness of the outermost binding.

Here are some examples:
````
?- gv_alloc(N), gv_set(N,hi), write(hi).
hi
N = 2
yes.
?- gv_get(2,V),write(V).
hi
V = hi
yes.
?- gv_set(2,bye).
yes.
?- gv_get(2,V1),write(V1),nl,fail;
gv_get(2,V2),write(V2).
bye
bye
V1 = _4
V2 = bye
yes.
?- gv_get(2,V).
V = bye
````
Note that gv_set/2 is a constant time operation so long as the second argument
is an atom or integer. Otherwise, it requires time linearly proportional to the current
depth of the choicepoint stack.

##8.3 Destructive Modification/Update of Compound Terms

ALS Prolog provides a predicate which allows programs to destructively modify arguments of compound terms (or structures). This predicate is mangle/3. The
effects of mangle/3 are destructive in the sense that they survive backtracking. The
calling pattern for this predicate is similar to arg/3:

    mangle(Nth, Structure, NewArg)

This call destructively modifies an argument of the compound term Structure
in a spirit similar to Lisp’s rplaca and rplacd. Structure must be instantiated to a compound term with at least N arguments. The Nth argument of Structure will become NewArg. Lists are considered to be structures of arity two.
NewArg must satisfy the restriction that NewArg is not itself an uninstatiated variable
(though it can be a compound term containing uninstatiated variables). Modifications made to a structure by mangle/3 will survive failure and backtracking.

Even though mangle/3 implements destructive assignment in Prolog, it is not
necessarily more efficient than copying a term. This is due to the extensive cleanup
operation which ensures that the effects of a mangle/3 persist across failure.
Here are some examples:
````
?- Victim = doNot(fold,staple,mutilate),
mangle(2,Victim,spindle).
Victim = doNot(fold,spindle,mutilate)
yes.
````


