#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alspi.h"

static void
error(const char *message)
{
	printf("Error: %s\n", message);
	exit(EXIT_FAILURE);
}

static int
run_unary_predicate(PWord module, 
					char *predstr, char *argstr)
{
	PWord gfv, gv, Av, av;           /* Vals for functor,goal,args */
	int        gt, At, at;           /* Goal and types */

	PI_makesym(&gfv, 0, predstr);    /* build goal functor */
	PI_makestruct(&gv, &gt, gfv, 1); /* build goal structure */
	PI_makesym(&av, &at, argstr);    /* build argument to goal*/
	PI_getargn(&Av, &At, gv, 1);     /* install argument... */
	PI_unify(Av, At, av, at);        /*  ...in the goal */
	
	/* run goal & return status: */
	return PI_rungoal(module, gv, gt); 
}

static int
run_binary_predicate(PWord module, char *predstr, 
                     char *arg1str, PWord *a2v, int *a2t)
{
	PWord gv, fctr, A1v,a1v; /* Vals for functor,goal,args*/  
	int   gt, fctrt,A1t,a1t;     /* Goal and types */
	int stat;
		
	PI_makesym(&fctr, &fctrt, predstr); /* build goal functor */
	PI_makestruct(&gv, &gt, fctr, 2);   /* build goal struct */
	PI_makesym(&a1v, &a1t, arg1str);    /* build arg #1 to goal */
	PI_getargn(&A1v, &A1t, gv, 1);      /* install arg 1 */
	PI_unify(A1v, A1t, a1v, a1t);       /*  ...in the goal */      

	stat = PI_rungoal_with_update(module, &gv, &gt);  /* Run goal, get status */
	PI_getargn(a2v, a2t, gv, 2);        /* Extract arg 2 */       
	return stat;
}

static void
load_know(void)
{
	PWord sym_builtins;
	
	PI_makesym(&sym_builtins, 0, "builtins");
	if (!run_unary_predicate(sym_builtins, "consult", "know"))
		error("Unable to load knowledge base.");
}

#define BUFSIZE 256

static void
qa_dialog(void)
{
	char item[BUFSIZE];
    char sval[BUFSIZE];
    double dval;
    PWord sym_user;

    PI_makesym(&sym_user, 0, "user");
    
    while (1) {
        PWord arg;
        int   argt, status;

		/* Prompt for and get an item to look up */
		printf("Enter item >\n"); 
		if (scanf("%s", item) != 1 || strcmp(item, "quit") == 0)
			break;

		/* Query "know" knowledge base in Prolog: */
		status = run_binary_predicate(sym_user, 
					"know", item, &arg, &argt);

   		/* Output value if found, failure message if not. */
		if (status) {
			/* switch on the type argt of the returned term */
			switch (argt) {
			case PI_SYM:
				PI_getsymname(sval, arg, BUFSIZE);
				printf("C: The value of %s is %s\n", item, sval); 
				break;
			case PI_INT:
				printf("C: The value of %s is %d\n", item, arg); 
				break;
			case PI_DOUBLE:
				PI_getdouble(&dval, arg);
				printf("C: The value of %s is %g\n", item, dval); 
				break;
			default:
				printf("C: Cannot understand value\n"); 
				break;
			}
		} else
			printf("Unable to find %s in knowledge base.\n", item);
	}
}

int main(int argc, char *argv[])
{
	int status;

	printf("Welcome to Q&A.\n");
	
	/* Initialize the ALS Prolog library */
	status = PI_prolog_init(argc, argv);
	if (status != 0) error("Prolog initilization failed.");

	/* Load the Prolog code. */
	load_know();

	/* Interact with user. */
	qa_dialog();
    
    /* Shut down the ALS Prolog library */
	PI_shutdown();
    
	printf("Goodbye.\n");

	return EXIT_SUCCESS;
}