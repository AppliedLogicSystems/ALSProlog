/*---------------------------------------------------------------*
 *	readsent.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	Simple "English" lexical analyzer
 *---------------------------------------------------------------*/

read_sentence(WordList)
	:-
	get(InitialChar),
	acceptable_in_word(InitialChar),!,
	read_words(InitialChar, [], BackwardsWordList),
	reverse(BackwardsWordList, WordList).
read_sentence([]).


read_words(Char, CurrentList, FinalList)
	:-
	read_one_word(Char, Word, NextChar),
	continue_read_words(NextChar, [Word | CurrentList], FinalList).


continue_read_words(NextChar, CurrentList, FinalList)
	:-
	not(end_punctuation(NextChar)),
	get(NextNextChar),
	acceptable_in_word(NextNextChar),!,
	read_words(NextNextChar, CurrentList, FinalList).
continue_read_words(NextChar, CurrentList,  CurrentList).


read_one_word(InitialChar, Word, FollowingChar)
	:-
	read_word_chars(InitialChar, [], CharList, FollowingChar),
	reverse(CharList, CorrectCharList),
	name(Word, CorrectCharList).


read_word_chars(CurrentChar, CurrentCharList, FinalCharList, FollowingChar)
	:-
	get0(NextChar),
	cont_read_word_chars(NextChar, [CurrentChar | CurrentCharList], 
			FinalCharList, FollowingChar).


cont_read_word_chars(NextChar, CurrentCharList,FinalCharList, FollowingChar)
	:-
	acceptable_in_word(NextChar),!,
	read_word_chars(NextChar, CurrentCharList,FinalCharList, FollowingChar).
cont_read_word_chars(FollowingChar, FinalCharList, FinalCharList, FollowingChar).


acceptable_in_word(Char)
	:-
	(0'a =< Char, Char =< 0'z) ;
	(0'A =< Char, Char =< 0'Z).


end_punctuation(0'.).
end_punctuation(0'!).
end_punctuation(0'?).
end_punctuation(-1).


reverse(ListA, ListB)
	:-
	reverse(ListA, [], ListB).


reverse([], OutList, OutList).
reverse([Item | InTail], Stack, OutList)
	:-
	reverse(InTail, [Item | Stack], OutList).


run 
	:-
	write('>>'),
	read_sentence(WordList),
	write(sent=WordList),nl.

