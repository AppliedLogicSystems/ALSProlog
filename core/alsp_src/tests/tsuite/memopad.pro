/*
 * memopad.pro	-- hash table example
 *
 * Author: Kevin A. Buettner
 * Created: 5-10-93
 * Description:
 *	This was a little example program which I created for Sam Daniel
 * to illustrate the use of hash tables and how variables may be shared
 * between elements of the hash table.  After writing it, I realized that
 * it tests (in a minimal sort of way) the hash table predicates which
 * I created a while back.
 *
 * This file runs and creates output by merely consulting it.  There is
 * nothing further to do.  Output should be check by eyeballing it against
 * the text of the program.
 */

%%
%% Make the hash table
%%

:- make_hash_table('_memo_pad').

%%
%% Define a W
%%

:- set_memo_pad('W',[A,B]).

%%
%% Define a G in terms of W
%%

:-	get_memo_pad('W',[A,B]),
	set_memo_pad('G',[A^2+B^2, A^2-B^2]).

%%
%% Define an H in terms of G
%%

:-	get_memo_pad('G',[A,B]),
	set_memo_pad('H',[2*B,2*A]).


%%
%% Write out W, G, and H
%%

:-	get_memo_pad('W',W),
	get_memo_pad('G',G),
	get_memo_pad('H',H),
	write('W'=W),nl,
	write('G'=G),nl,
	write('H'=H),nl,
	nl.

%%
%% Perform a substitution on W and write out resulting substitions on W and G
%%

:-	get_memo_pad('W',W),
	get_memo_pad('G',G),
	WS=[4,5],		%% WS is our substitution
	copy_term([W|G],[WS|GS]),
	write('WS'=WS),nl,
	write('GS'=GS),nl,
	nl.

%%
%% Perform a substitution on W and write out resulting substitions on W and H
%%

:-	get_memo_pad('W',W),
	get_memo_pad('H',H),
	WS=[4,5],		%% WS is our substitution
	copy_term([W|H],[WS|HS]),
	write('WS'=WS),nl,
	write('HS'=HS),nl,
	nl.
