#include <stdio.h>
#include <stdlib.h>
#include "cexception.h"

/* 

Failures:

There are basicly two method of handling errors. The first is simply to
abort the program at the first sign of trouble. The second is to try and
gracefully back out of an error in order to allow some form of recovery
(or retry). Obviously the second alternative is the most desirable. In fact,
aborting is simply a degenerate case of graceful failure because the
operating system catches the abort and cleans up.

The reason graceful failure is seldom used is that it seems expensive
to implement.

Aborting at the first sign of trouble is easy to implement. Just exit
whenever an error is detected. This stratagy has almost no overhead beyound
the neccesary error detections (which MUST always be checked anyway).

Backing out gracefully can be VERY expensive when using normal flow control.
The only way to do this is to check each operation to see if its failed, and
if it has failed then cleanup code must be executed. This means that every
operations must have an if-statement associated with it, creating a unreadable
mess of nested code (the structured equivalent of spagetti code).

The solution is to use a structured form of goto (a contradiction in terms :-).
Instead of testing each operation for failure, have a failing operation
automatically call the cleanup code using some form of goto and when the
cleanup code is finished have control automatically goto the next enclosing
level of cleanup code. All these gotos are, of course, hidden in a new
control structure: the TRY.

A TRY consists of two blocks of code, the try-block and the fail-block.

TRY try-block FAIL fail-block ENDTRY

Under normal conditins a TRY simply executes the try-block. The fail-block
is only executed if their is a failure during the execution of the try-block.
When the fail-block is finished, the fail-block of the next enclosing TRY is
executed. If there is no enclosing TRY, then the program aborts.

The only exception to the fail-block behavior is that it is OK to re-execute
(retry) the try-block (if at first you don't succeed, ...). This isn't
supported by the TRY macros, so you must use a goto to jump out of the
fail-block to a label just before the TRY.

An alternative to the TRY..FAIL..ENDTRY control is the TRY..ALWAYS..ENDTRY
control. The always-block is always executed, wether or not there was a
failure. FAIL and ALWAYS are meant to be used in different situations.

The fail-block is only executed when there is a failure.
FAIL is useful for gracefully backing out of allocations and other unbalance
operations.

The always-block is always executed whether or not there was a failure.
ALWAYS is useful for ensuring closure of balanced opertions such as temporary
malloc/free or fopen/fclose.


For a better desciption of failure handling see Bertrand Meyer's book.
Meyer's ideas can be summarized as a set of axioms:

A block either succeeds or fails, there is no in-between state.

If Block A calls Block B and Block B fails, the Block A fails.

When a block fails it has two choices: cleanup and fail or cleanup and retry.


Implementation:

To implement the TRY failure mechanism in C we use setjmp() and a series of
macros. Setjmp() is called before the try-block to save the TRYs envioronment.
If setjmp() returns 0 (which it always does when called normally), then push
the stored enviorment on the fail stack and execute the try-block.

If there is no failure then the enviorment is poped from the fail stack and
the TRY is exited (or in the case of ALWAYS the always-block is executed).

If there is a failure then Failure() is called which pops the enviorment off
the fail stack and calls longjmp() to re-execute the setjmp(). This time
setjmp() returns a non-zero result, which executes the fail-block
(or always-block).

Once the fail-block has cleaned up, Failure() is called again to trigger
the failure handling of any enclosing TRYs.


The Macros:

We use a switch statement to handle the result of setjmp instead of
if..then because this makes ALWAYS easy to construct as a case fallthru.
See failure.h for details.

The following macro:

TRY {
	DoSomething();
} 
FAIL {
	CleanUp();
}
ENDTRY

expands to:
{ 
	FailFrame __FailRecord;
	PushTry(&__FailRecord);
	switch (setjmp(__FailRecord.try_env)) {
	case 0:
		{
			DoSomething();
		}
		PopTry();
		break;
	default:
		{
			CleanUp();
		}
		if (FailError()) Failure(FailError(), FailMessage());
		break;
	}
}

TRY...ALWAYS...ENDTRY expands in the same way except that case 0 has no break
statement.


Warnings:

Because TRY-FAIL-ENDTRY is a macro it is inheirently dangerous. Here are some
things to watch out for.

Do not use return() inside the try-block or fail-block, this will leave an
inconsistent fail stack.

Do not use a naked break in the try-block or fail-block. This can happen when
you put a TRY inside a for/while loop and try to use break to exit the loop.
It will exit the TRYs switch statment, not the loop. Again this will leave
an inconsistent fail stack.


The Local Variables in Registers Problem:

Beware of register varaibles and optimizing compilers.

The abstract semantics of the FAIL macro is meant to maintain the current
value of all local variables during a failure. Unfortunatly setjmp() and
longjmp() do not properly restore the value of variables stored in registers.

Setjmp() saves the registers when it is called and longjmp() restores them.
The result is that during a failure any variables in registers will not have
current values while those stored on the stack will be current.

Note that just avoiding register declarations is not the solution, since
optimizing compilers may silently put variables in registers.

The solution is to declare variables as volatile, which forces the compiler
to keep the variables value on the stack in real memory. (The ANSI standard
is vague about the meaning of volatile. It claims its meaing is implementation
specific, but the comentaries and existing compilers describe it properly.)

The general rule is:
If you want to use the variable n in the fail-block, then declare n as
volatile.

WRONG: register int n;
WRONG: int n;
RIGHT: volatile int n;

The real problem here is that setjmp/longjmp are not part of the C language
(they're library functions), so they have no control over how local variables
are restored. New versions of C++ incorporate failure handling into the
language.

Example Usage:

Here's a typical example using both TRY-FAIL-ENDTRY and TRY-ALWAYS-ENDTRY.
This example creates a new object of type thing and initilizes it from data
in a file. If there is any failure during the creation of a thing, you are
guaranteed that NewThing() fails cleanly without leaving unclosed files or
unreachable memory blocks.

thing *New_Things()
{
	volatile thing *p;
	
	// Allocate a thing. If there's not enoungh memory, signal a failure for
	//the caller to handle.
	
	p = malloc(sizeof(thing));
	if (!p) Failure(OUT_OF_MEMORY_ERROR, 0);
	
	// Now try to initilize the thing by reading info from a file and
	// allocating array.
	
	TRY
	{
		// Try to allocate a substructure of p.
		p->array = NULL;
		p->array = malloc(gNeeded_Array_Size);
		if (!p->array) Failure(OUT_OF_MEMORY_ERROR, 0);
		
		// Try to initilize p from data in a file.
		Open_and_Read_File(p);
	}
	FAIL
	{
		// When there is an failure, clean up by freeing p and its
		// substructures.
		
		if (p->array) free(p->array);
		free(p);
	}
	ENDTRY
	
	// If control reaches this point, you know everything worked.
	
	return p;
}

void Open_and_Read_File(thing *p)
{
	volatile FILE f;
	
	// Open thingfile. If it can't be opened, signal a failure for the
	// caller to handle.
	
	f = fopen("thingfile", "r");
	if (!f) Failure(IO_ERROR, 0);
	
	TRY
	{
		// Reading the file could cause a failure due to IO problems.
		
		Read_File(f, p);
	}
	ALWAYS
	{
		// Whether or not there is a failure, always close the file.
		
		fclose(f);
	}
	ENTRY
}

*/

// THREAD
/* Pointer to the top of the failure frame stack. */
static volatile FailFrame *gFailStack = NULL;

// THREAD
/* The error code for the current failure. */
static volatile int gFailError = 0;
static volatile long gFailMessage = 0;		

void PushTry(FailFrame *failRec)
{
	failRec->nextFrame = gFailStack;
	gFailStack = failRec;
}

void PopTry(void)
{
	if (gFailStack) {
		gFailStack = gFailStack->nextFrame;
	}
}


int FailError(void)
{
	return gFailError;
}

long FailMessage(void)
{
	return gFailMessage;
}

void ClearFailure(void)
{
	gFailError = 0;
	gFailMessage = 0;
}


/* Failure executes the try-bock of the most recently executed TRY...ENDTRY
statement. If there is no failure handler then the program is aborted.
Error is the error code that identifies the reason for the failure. */

void Failure(int error, long message)
{
	volatile FailFrame *currentFrame;
	
	/* Pop frame off the fail stack and jump to its enviorment. */
	if (gFailStack) {	
	
		currentFrame = gFailStack;
		gFailStack = gFailStack->nextFrame;
		
		if (gFailError == 0) {
			gFailError = error;
			gFailMessage = message;
		}
		
		longjmp(((FailFrame *)currentFrame)->try_env, 1);
	
	/* When there is no failure handler, abort program. */
	/* I've left this as exit, so that the console window will
	stay up during a failure. */
	} else { 
	
		/*//exit(EXIT_FAILURE);*/
		
	}
}
