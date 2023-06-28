/*======================================================================
 |			x_blt_sample_f.pro
 |	Copyright (c) 2019 Applied Logic Systems, Inc.
 |		Group: BltSamples
 |		DocTitle: blt_sample_A/1
 |		-- Simple sample builtins file for use with doctools.pro/file_doc
 *=====================================================================*/
module builtins.

export blt_sample_A/1.
export blt_sample_B/3.

/*!---------------------------------------------------------------------
 |	blt_sample_A/1
 |	blt_sample_A(Item)
 |	blt_sample_A(+)
 |
 |	- Does some sample something
 |		
 |	Accepts any kind of Item and does some sample processing.
 |
 | Examples
 |	?- Item=foo(3), blt_sample_A(Item).
 |	%%% Output message..
 |	yes.
 *!--------------------------------------------------------------------*/
%%% blt_sample_A/1 code here...


/*!---------------------------------------------------------------------
 |	blt_sample_B/3
 |	blt_sample_B(Item1,Item2,Result))
 |	blt_sample_B(+,+,-)
 |
 |	- Does some sample something and returns Result
 |		
 |	Accepts two kinds of Item1 and Item2, and combines them
 |	in some sample way to produce Result. 
 |
 | Examples
 |	?- Item1=foo(19),Item2=[4,5],blt_sample_B(Item1,Item2,Result).
 |	Result = %%% some result
 |	yes.
 *!--------------------------------------------------------------------*/
%%% blt_sample_B/3 code here...

endmod.
