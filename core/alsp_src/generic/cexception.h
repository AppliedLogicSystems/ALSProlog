#ifndef _failure_
#define _failure_

#include <setjmp.h>


/* Active TRYs form a stack. Each TRY stores its state in a FailFrame. */

typedef struct FailFrame {
	jmp_buf try_env;				/* The stored envioroment of this TRY. */
	volatile struct FailFrame *nextFrame;	/* The next failure handler. */
} FailFrame;


/* TRY ... FAIL ... ENDTRY Macro or TRY ... ALWAYS ... ENDTRY Macro */

/* _FailRecord_ is the stack frame for the TRY's failure handling. */
/* Save the current enviorment in _FailRecord_ and push it on
the fail stack. */

/* A _Retry_ label could be added just before the switch, so that a RETRY
macro in the FAIL code could jump to it to re-execute the TRY code. There
are two reasons why RETRY has been left out. First, RETRYs are rair (usually
only once at the top-most event loop), and this can be handled with a normal
goto. The second and more important reason is that adding a label makes it
impossible to nest more than one TRY in the same function. This is because
the labels name-space spans the entire fuction, regarless of brace nesting.
*/
/* NO!!! By looking at the Think C 5.0 exception handling, it became clear
that a rety is just setting the error flag to 0 and longjmp-ing back to 
the start of the try. */
/* work on this tonight. */

/* Note that _FailRecord_ is declared inside a block which define its
scope. This allows TRYs to be nested because each embebeded _FailRecord_ mask
out (suspends) _FailRecords_ outside the block.
*/

#define TRY \
	{ \
		FailFrame __FailRecord; \
		PushTry(&__FailRecord); \
		switch (setjmp(__FailRecord.try_env)) { \
		case 0:

/* Its possible to have CATCH() macros that catch specific error codes.
These have not been implemented. */

/* If the try-block is successful, then pop the fail stack
and exit normally. */
/* The recovery code is executed when setjmp returns a non-zero result. */

#define ONEXCEPTION \
			PopTry(); \
			break; \
		default:

/* Note that ALWAYS relies on the fact that switch allows control to fall
thru from one case to the next when there is no break statement. */

#define ALWAYS \
			PopTry(); \
		default:
			
/* At the end of the fail-block or always-block, trigger the next level of
failure code if an error has occured. */

#define ENDTRY \
			if (FailError()) Failure(FailError(), FailMessage()); \
			break; \
		} \
	}

/* 
Should ENDTRY implicitly cause a new failure, or should the programmer be
given the choice of recovering or failing?

Stroika gives the programmer the choice. But this goes against Meyer's axiom
that a block succeeds or fails or retrys, it can NEVER recover without
succeeding. So ENDTRY implicitly calls Failure() which causes the failure
to bubble up to the top of the program.
*/

void PushTry(FailFrame *failRec);
void PopTry(void);
int FailError(void);
long FailMessage(void);
void ClearFailure(void);

void Failure(int error, long message);

#endif
