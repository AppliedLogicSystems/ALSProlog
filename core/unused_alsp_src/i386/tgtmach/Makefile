COPYLCNS = DOSPHAR DOSERGO MSWins

MACH = ZIPPER
MACH = DOSPHAR


test:
	echo mach=$(MACH)
	@if  whd $(MACH) ; then echo ONE ; else echo TWO ; fi

test2:
	echo mach=$(MACH)
	@if [ $(MACH) = DOSPHAR -o \
		  $(MACH) = DOSERGO -o \
		  $(MACH) = MSWins ]; then echo ONE ; else echo TWO ; fi
