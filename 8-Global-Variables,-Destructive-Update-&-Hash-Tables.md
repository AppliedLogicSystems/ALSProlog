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
