#define DELAYINT 3

#define CHK_DELAY(v) (MFUNCTOR_TOKID(*(v-1)) == TK_DELAY) && (MFUNCTOR_ARITY(*(v-1)) == 4)

extern 	void	combin_dels PARAMS( (PWord, PWord));

