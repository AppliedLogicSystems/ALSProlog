/*---------------------------------------------------------------*
 *	georgew.pro
 *	Copyright (c) 1989-90 Applied Logic Systems, Inc.
 *
 *	George Washington's family tree
 *---------------------------------------------------------------*/


parent('Augustine Washington', 'George Washington').
parent('Mary Ball Washington', 'George Washington').
parent('Lawrence Washington', 'Augustine Washington').
parent('Mildred Warner Washington', 'Augustine Washington').
parent('Joseph Ball', 'Mary Ball Washington').
parent('Mary Johnson Ball', 'Mary Ball Washington').
parent('John Dandridge', 'Martha Washington').
parent('Frances Jones Dandridge', 'Martha Washington').
male('George Washington').
male('Augustine Washington'). 
male('Lawrence Washington').
male('Joseph Ball').
male('John Dandridge').
female('Mary Ball Washington').
female('Mildred Warner Washington').
female('Mary Johnson Ball').
female('Martha Washington').
female('Frances Jones Dandridge').
