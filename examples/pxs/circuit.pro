/*---------------------------------------------------------------*
 *	circuit.pro
 *	Copyright (c) 1986-90 Applied Logic Systems, Inc.
 *
 *	Representation of a simple circuit
 *---------------------------------------------------------------*/

and_Table(on,on,on).
and_Table(on,off,off).
and_Table(off,on,off).
and_Table(off,off,off).

or_Table(on,on,on).
or_Table(on,off,on).
or_Table(off,on,on).
or_Table(off,off,off).

not_Table(on,off). 
not_Table(off,on). 

and_Gate(g1, a, b, e). 
and_Gate(g2, c, d, f). 
  
or_Gate(g3, e, f, h). 
 
input(a). 
input(b). 
input(c). 
input(d). 
output(h).

