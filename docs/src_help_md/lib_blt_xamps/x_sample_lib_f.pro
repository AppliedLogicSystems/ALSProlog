/*======================================================================
 |			x_sample_f.pro
 |	Copyright (c) 2019 Applied Logic Systems, Inc.
 |		Group: Sample
 |		DocTitle: sample_A/1
 |		-- Simple sample lib file for use with doctools.pro/file_doc
 *=====================================================================*/
module builtins.

export sample_A/1.
export sample_B/3.

/*!---------------------------------------------------------------------
 |	sample_A/1
 |	sample_A(Item)
 |	sample_A(+)
 |
 |	- Does some sample something
 |		
 |	Accepts any kind of Item and does some sample processing.
 |
 | Examples
 |	?- Item=foo(3), sample_A(Item).
 |	%%% Output message..
 |	yes.
 *!--------------------------------------------------------------------*/
%%% sample_A/1 code here...


/*!---------------------------------------------------------------------
 |	sample_B/3
 |	sample_B(Item1,Item2,Result))
 |	sample_B(+,+,-)
 |
 |	- Does some sample something and returns Result
 |		
 |	Accepts two kinds of Item1 and Item2, and combines them
 |	in some sample way to produce Result. 
 |
 | Examples
 |	?- Item1=foo(19),Item2=[4,5],sample_B(Item1,Item2,Result).
 |	Result = %%% some result
 |	yes.
 *!--------------------------------------------------------------------*/
%%% sample_B/3 code here...

endmod.
