/*---------------------------------------------------------------*
 *	hickgram.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Expanded grammar rules for Hickory identification input.
 *---------------------------------------------------------------*/
:-[-readsent].

descriptor(desc(T, V), List,  Tail) 
	:-
	trait(T, List, InterTail),  value(V, InterTail, Tail).

descriptor(desc(T, V), List,  Tail) 
	:-
	value(V, List, InterTail),
	trait(T,  InterTail, Tail).

descriptor(done, List, Tail)
	:-
	finished(List, Tail).


trait(bud_color, [bud_color | Tail], Tail).
trait(bud_scales, [bud_scales | Tail], Tail).
trait(terminal_bud_size, [terminal_bud_size | Tail], Tail).
trait(outer_scales, [outer_scales | Tail], Tail).
trait(twig_color, [outer_scales | Tail], Tail).

value(yellow, [yellow | Tail], Tail).
value(brownish, [brownish | Tail], Tail).
value(valvate, [valvate | Tail], Tail).
value(imbricate, [imbricate | Tail], Tail).
value(short, [short | Tail], Tail).
value(large, [large | Tail], Tail).
value(persistent, [persistent | Tail], Tail).
value(orange_brown, [orange_brown | Tail], Tail).
value(reddish_brown, [reddish_brown | Tail], Tail).

finished([done | Tail],  Tail).

run1(D) :-
	descriptor(D, [bud_color,yellow],_).
run2(D) :-
	descriptor(D, [yellow, bud_color],_).

run3(D) :-
	write('Description:'),
	read_sentence(WordList),
	descriptor(D, WordList, _).
