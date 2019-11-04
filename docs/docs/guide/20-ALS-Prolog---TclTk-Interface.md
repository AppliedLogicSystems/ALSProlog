---
---

# 20 ALS Prolog - TclTk Interface
{:.no_toc}

* TOC
{:toc}

## 20.1 Introduction and Overview
The interface between ALS Prolog and Tcl/Tk allows prolog programs to create,
manipulate and destroy Tcl/Tk interpreters, to submit Tcl/Tk expressions for evaluation in those interpreters, and to allow expressions being evaluated to make calls
back into Prolog. Computed data can be passed in both directions:
* A Tcl/Tk function called from Prolog can return a value to Prolog;
* A Prolog goal called from Tcl/Tk can bind Tcl variables to computed values.
The conversions between the datatypes of the two languages are described in the
next section. In general, one cannot count on the interface to automatically handle
all situations. One must either assure that the originating code (Prolog or Tcl) creates a data entity which coverts into the the desired target type, or one must explicitly
coerce the entity after it has passed through the interface (e.g., with tcl_coerce).

## 20.2 Prolog to Tcl Type Conversion

## 20.3 Prolog to Tcl Interface Predicates

### 20.3.1 tcl_new(?Interpreter)  
   tk_new(?Interpreter)

tcl_new/1 creates a new Tcl interpreter. If the Interpreter argument is an
uninstantiated (Prolog) variable, then a unique name is generated for the interpreter. If Interpreter is a atom, the new Tcl interpreter is given that name.

tk_new/1 functions in the same manner as tcl_new/1, except that the newly created Tcl interpreter is initialized with theTk package.

Examples
```
tcl_new(i). Succeeds, creating a Tcl interpreter named i.
tcl_new(X). Succeeds, unifying X with the atom 'interp1'.
```
Errors
* Interpreter is not an atom or variable.
    type_error(atom_or_variable).
* The atom Interpreter has already been use as the name of a Tcl interpreter.
    permission_error(create,tcl_interpreter,Interpreter)
* Tcl is unable to create the interpreter.
    tcl_error(message)

### 20.3.2 tcl_call(+Interpreter, +Script, ?Result)  
   tcl_eval(+Interpreter, +ArgList, ?Result)

tcl_call and tcl_eval both execute a script using the Tcl interpreter and returns the
Tcl result in Result. tcl_call passes the Script argument as a single argument
toTcl's eval command. tcl_eval passes the elements of ArgList as arguments to the
Tcl's eval command, which concatenates the arguments before evaluating them.

tcl_call's Script can take the following form:
* List - The list is converted to a Tcl list and evaluated by the Tcl interpreter. The
list may contain, atoms, numbers and lists.
* Atom - The atom is converted to a string and evaluated by the Tcl interpreter.
tcl_eval's ArgList may contain atoms, numbers or lists.

Examples
```
tcl_call(i, [puts, abc], R). Prints 'abc' to standard output, and bind R to ''.
tcl_call(i, [set, x, 3.4], R). Sets the Tcl variable x to 3.4 and binds R to 3.4.
tcl_call(i, 'set x', R). Binds R to 3.4.
tcl_eval(i, ['if [file exists ', Name, '] puts file-found'], R).
```
Errors
* Interpreter is not an atom.
* Script is not an atom or list.
* Script generates a Tcl error.
    tcl_error(message)

### 20.3.3 tcl_coerce_number(+Interpreter, +Object, ?Number)  
### 20.3.4 tcl_coerce_atom(+Interpreter, +Object, ?Atom)  
   tcl_coerce_list(+interpreter, +Object, ?List)  

These three predicates convert the object Object to a specific Prolog type using the
Tcl interpreter Interpreter. Object can be an number, atom or list. If the object is
already the correct type, then it is simple bound to the output argument. If the object
cannot be converted, an error is generated.  
Examples
```
tcl_coerce_number(i, ' 1.3', N) Succeeds, binding N to the float 1.3
tcl_coerce_number(i, 1.3, N) Succeeds, binding N to the float 1.3
tcl_coerce_number(i, 'abc', N) Generates an error.
tcl_coerce_atom(i, [a, b, c], A) Succeeds, binding A to 'a b c'
tcl_coerce_atom(i, 1.4, A) Succeeds, binding A to '1.4'
tcl_coerce_list(i, 'a b c', L) Succeeds, binding L to [a, b, c]
tcl_coerce_list(i, 1.4, L) Succeeds, binding L to [1.4]
tcl_coerce_list(i, '', L) Succeeds, binding L to []
```
Errors
* Interpreter is not an atom.
* Object is not a number, atom or list.
* Object cannot be converted to the type.
    tcl_error(message)

### 20.3.5 tcl_delete(+Interpreter)  
   tcl_delete_all

Tcl_delete deletes the interpreter named Interpreter.
Tcl_delete_all deletes all Tcl interpreters created by tcl_new/1.

## 20.4 Tcl Prolog Interface

### 20.4.1 prolog - call a prolog term
Synopsis  
prolog option ?arg arg... ?  

Description  
The prolog command provides methods for executing a prolog query in ALS Prolog from Tcl. Option indicates how the query is expressed. the valid options are:  

#### prolog call module predicate ?-type arg ...?
Directly calls a predicate in a module with type-converted arguments. The
command returns 1 if the query succeeds, or 0 if it fails. The arguments can take
the following forms:
*number arg  
Passes arg as an integer or floating point number.
*atom arg  
Passes arg as an atom.  
*list arg  
Passes arg as a list.  
*var varName  
Passes an unbound Prolog variable. When the Prolog variable is bound, the Tcl
variable with the name varName is set to the binding.

#### prolog read_call termString ?varName ...?  
The string termString is first read as a prolog term and then called. The command returns 1 if the query succeeds, or 0 if it fails. The optional variables
named by the varName arguments are set when a Prolog variable in the query
string is bound. The prolog variables are matched to varNames in left-to-right
depth first order.

Examples  
```
prolog call builtins append -atom a -atom b -var x
    Returns 1, and the Tcl variable x is set to {a b}.

prolog read_call “append(a, b, X)” x
    Returns 1, and the Tcl variable x is set to {a b}.
```

## 20.5 Stand-Alone TCL

Normally Tcl/Tk is installed as a system independent of ALS Prolog. Typically the
Tcl/Tk shared/dynamic libraries are stored in a system directory (/usr/local/lib on
Linux, including Mac OS, and  \winnt\system32 on Windows NT). Tcl/Tk support libraries are similarly stored in a global location.

When creating a stand alone Prolog/Tcl-Tk application, it is sometimes convenient
to create a package which includes Tcl/Tk so that the application will run correctly
even on systems without Tcl/Tk.

Basicly this is done by moving the Tcl/Tk shared/dynamic libraries and support libraries into the same directory as ALS Prolog. Here are sample directories for Linux, including Mac OS X, and Win32:
Linux  
* alspro
* libtcl8.0.so or libtcl8.0.sl
* libtk8.0.so or libtk8.0.sl
* lib (directory containing:)
tcl8.0 (support library directory)
tk8.0 (support library directory)

Win32  
* alspro.exe
* tcl80.dll
* tk80.dll
* lib (directory containing:)
tcl8.0 (support library directory)
tk8.0 (support library directory)

On Solaris, unlike other Unix systems, the search path list for shared objects does
not include the executable's directory. To ensure that the Tcl/Tk shared objects are
found, the current directory '.' must be added to the LD_LIBRARY_PATH environment variable.
