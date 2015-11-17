Just like most other computer languages, Prolog allows you to store programs and
data in text files. The builtin predicates consult/1, reconsult/1, read/1,
and their variants will translate the textual representation for programs and data into
the internal representations used by the Prolog system. This section describes what
kinds of syntactic objects can appear in source files and how they are interpreted.

###2.1 Source Terms

Every Prolog source file must be a sequence of zero or more Prolog terms, each
term followed by a period (.) and a white space character. A period followed by a
white space character is called a full stop. Full stops are needed in source files to
show where one term ends and another begins. Each term in a file is treated as a
closed logical formula. This means that even though two seperate terms have variable names in common, each term’s variables are actually distinct from the variables in any other term. Most variables are quantified once for each term. However, there is a special variable, the anonymous variable (written ‘_ ’), which is quantified for each occurrance. This means that within a single term, every occurrance
of ‘_’ is a different variable.

###2.1.1 Rules

A rule is the most common programming construct in Prolog. A rule says that a
particular property holds if a conjunction of properties holds. Rules are always written with :-/2 as the principal functor. The first argument of the :- is called the
head, and the second argument is called the body. Here are some examples of rules:

    a(X) :- b(X).  
    blt :- bacon, lettuce, tomato.  
    test(A,B,C) :- cond1(A,B), cond2(B,C).  

consult/1 and reconsult/1 load rules (and facts - see below) from a source
file into the internal run-time Prolog database in the order they occur in the source
file.

###2.1.2 Facts

A fact is any term which is not a rule and which cannot be interpreted as a directive
or declaration. For example,

    module mymodule.

would not be interpreted as a fact since it is a module declaration - see Chapter 3
Modules.{ADD LINK}  More specifically, a fact is any term that cannot be interpreted as a declaration and whose principal functor is not :-/1, ?-/1, or :-/2. One way to understand a fact is to say that it is a rule without a body, or a rule with a trivial body
(one that is always true). These are example facts:  

    mortal(socrates).  
    big(ben).  
    identical(X,X).  

As with rules, consult/1 and reconsult/1 load facts (and rules)from a source
file into the internal database in the order they occur in the source file.

###2.1.3 Commands and Queries

A command or directive is any term whose principal functor is :-/1. Queries are
terms whose principal functor is ?-/1. The single argument to a command or query
is a conjunction of goals to be executed by the prolog engine.  Commands and queries can be submitted via the prolog shell, or can be included in a prolog program, to be executed whenever the program is loaded (consulted) by the engine.  

Commands and queries either succeed or fail depending on the program context in which they occur.  Commands are silent in all contexts unless they fail, or unless some subsidiary goal inside the command invokes a write on the current output stream.  When a command fails (either using the prolog shell or during consulting a file), a warning message is written to standard output, which is usually the screen or console window.

Like commands, when queries fail, a warning message is written to standard output.  However, when queries succeed, a success message is also written to standard output.  If a query contains unbound prolog variables and it is submitted via the prolog shell, the success message will describe values for those variables which were bound during the successful execution.

Commands are often used in prolog source files to require the consult or reconsult of other prolog source files, or to add operator declarations to the prolog parser.

Here are some examples of commands:

    :- op(300, xfx, #).
    :- [file1], [file2], write(’Files 1&2 have been loaded’).
    :- initializeProgram, topLevelGoal.

The prolog shell accepts queries from a terminal or window, and prints the results.  Because the shell assumes that each input is a query, it uses the query symbol ?- as a prompt.
Suppose the following program of simple facts has been loaded:

    wavelength(red, 620-750).
    wavelength(yellow, 570-590).
    wavelength(blue, 450-495).

Then the follow are some simple query interations with this program using the shell:
````
?- wavelength(blue,W).

W=450-495 

yes.
?- wavelength(orange, W).

no.
?- wavelength(C, W).

C=red 
W=620-750 ;

C=yellow 
W=570-590 ;

C=blue 
W=450-495 ;

no.
````

Notice that when a success answer is printed, the shell waits for input from the user.  If the user types a semi-colon (;), the shell attempts to find further solutions for the query, and prints the next one if found, or "no." if no solutions are found.  Any input other than a semi-colon is interpreted to mean that no further solutions are required.

