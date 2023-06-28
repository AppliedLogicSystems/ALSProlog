#define DELAYINT 3

#define CHK_DELAY(v) (MFUNCTOR_TOKID(*(((PWord *)v)-1)) == TK_DELAY) && (MFUNCTOR_ARITY(*(((PWord *)v)-1)) == 4)

extern 	void	combin_dels (PWord, PWord);


