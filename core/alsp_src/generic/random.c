/**
 ** random.c: Minimal Standard Pseudo-Random Number Generator
 **
 ** Author: Fuat C. Baran, Columbia University, 1988
 **
 ** Based on code in "Random Number Generators: Good Ones are Hard to Find",
 ** by Stephen K. Park and Keith W. Miller in Communications of the ACM,
 ** 31, 10 (Oct. 1988) pp. 1192-1201.
 **
 ** Requirements: maxint must be 2^31 -1 or larger.
 **
 ** Compile with -DTEST and run to see if it was implemented correctly.
 **/

#include <stdio.h>
#include "random.h"

/* some constants we need */
#define A 16807
#define M 2147483647		/* Mersenne prime 2^31 -1 */
#define Q 127773			/* M div A (M / A) */
#define R 2836				/* M mod A (M % A) */

static long seed = 1;		/* initialize with rand_init() */


/*
 * minimal_standard_random:
 * minimal standard random number generator.
 * call rand_init first to initialize seed.
 * returns a random float.
 */

double minimal_standard_random (void)
{
    long hi, lo;
    
    hi = seed / Q;
    lo = seed % Q;
    seed = A * lo - R * hi;
    if (seed <= 0) seed += M;
    return ((double) seed / M);
}


/*
 * minimal_standard_rand_init:
 * initialize seed.
 */

void minimal_standard_rand_init (long s)
{
    seed = s;
}

#if 0
#ifdef TEST

main ()
{
    int i;
    float r;

    rand_init (1);
    for (i = 0; i < 10000; i++)
	r = random();
    printf ("Seed is %ld (should be 1043618065)\n", seed);
}

#else

main ()
{
    int i;

    for (i = 0; i < 1000000; i++)
	random();
}

#endif /* TEST */
#endif

