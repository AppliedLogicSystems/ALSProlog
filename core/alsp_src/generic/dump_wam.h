/* wam_task_state contains a stored state of the WAM "chip" */
typedef struct {
	
	PWord P,	/* Program Counter Register */
		  B,	/* Backtrack Frame Register */
	      HB,	/* Heap Backtrack Register */
		  SPB,	/* Stack Backtrack Register */
		  E,	/* Environment Register */
		  TR,	/* Trail Register */
		  H,	/* Heap Register */
		  SP,	/* Stack Register */
		  F;	/* Fail Program Register */
		  
		  
	/* These are not essential, but nice to have around. */
	PWord *choice_trail_base,	 /* The base of the choicepoint/trail stack (grows down). */
		  *heap_and_stack_base,  /* The base of the heap (grows up), and stack (grows down). */
		  *stack_limit,		 /* The maximum top of the stack. */
		  *global_end;		 /* The end of the global allocation area. */

} wam_task_state;

void get_wam_task_state(wam_task_state *s);
void dump_wam_task_state(const wam_task_state *s);
