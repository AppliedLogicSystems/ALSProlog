/*======================================================================*
 |		drawing.pro
 |	Copyright (c) Applied Logic Systems, Inc.
 |	
 |	A simple graphics drawing package
 |
 | Exported Routines:
 | ------------------
 | start_drawing/0
 |   - initializes the tcl/tk interface, loads 'drawing.tcl', and
 |     creates the canvas
 |
 | draw/8
 | draw(Type,Canvas,AX,AY,BX,BY,Options,ID)
 | draw(+,+,+,+,+,+,+,-)
 |   - the basic routine for drawing on a canvas; 
 |   Type:  one of: line, arc, oval,
 |   Canvas: a Tk canvas
 |   AX,AY,BX,BY - bounding box coordinates for entity Type
 |   Options - described below
 |   ID - the Tk canvas ID for the entity draw
 |
 | The following all call draw/8 with appropriate args; they instantiate
 | Canvas to: '.drawing.cnv.canvas'.
 |
 | line/[4,5]
 | line(AX,AY, BX,BY)    :- line(AX,AY, BX,BY, [])
 | line(AX,AY, BX,BY, Opts)
 |
 | arc/[6,7]
 | arc(AX,AY, BX,BY,Start,Extent)  
 |	:- arc(AX,AY, BX,BY,Start,Extent, [start=Start,extent=Extent])
 | arc(AX,AY, BX,BY,Start,Extent, Opts)
 |
 | oval/[4,5]
 | oval(AX,AY,BX,BY) 	:- oval(AX,AY,BX,BY,[])
 | oval(AX,AY,BX,BY,Opts)
 |
 | rect/[4,5]
 | rect(AX,AY,BX,BY) 	:- rect(AX,AY,BX,BY,[])
 | rect(AX,AY,BX,BY,Opts)
 |
 | poly/[1,2]
 | poly(PairList) :- 	poly(PairList,[])
 | poly(PairList,Opts)
 | 	PairList is of the form: [AX,AY,BX,BY,CX,CY,...]
 |
 | Options:  Opts is a list of "equations" of the form:
 |	Tag = Value
 | where Tag and Value are as appropriate for Tk canvas entities.
 | Common options:
 | Tag		Value
 | ===		=====
 | fill 	<color>
 |	Fill an appropriate area of the entity with color; defaults to
 |	to nothing (transparent/background)
 |
 | outline 	<color>
 |	color specifies a color to use for drawing the entities' outline;  
 |	defaults to black. 
 |
 | stipple 	<bitmap>
 |	Indicates that the entity should be filled in a stipple pattern; 
 |	bitmap specifies the stipple pattern to use; if the fill option 
 |	hasn't been specified, this option has no effect. If bitmap is an 
 |	empty string (the default), then filling is done in a solid fashion. 
 |
 | width 	<outlineWidth>
 |	outlineWidth specifies the width of the outline to be drawn around 
 |	the entity. If the -outline option hasn't been specified, this 
 |	option has no effect; defaults to 1.0.  
 |
 | Other options are as specified for Tk canvas; option equations
 | of the form
 |		Tag = Value
 | are passed through to Tk in the form
 |		-Tag Value
 *======================================================================*/

/*----------------------------------------------------------------------*
 Example:
 -------
 *----------------------------------------------------------------------*/
sample_draw 
	:-
	start_drawing,
	oval(50,60,230,270,[fill=green]),
	line(50,200,210,60,[fill=blue,width=3]),
	arc(55,260,210,65,30,140,[width=2,fill=red]),
	rect(200,120,300,40, [width=6,outline=brown]),
	poly([55,230,85,150,120,310,190,90],[fill=yellow,width=2,outline=purple]).


module simple_drawing.
use tcltk.
use tk_alslib.

export start_drawing/0.
start_drawing
	:-
	init_tk_alslib,
	tcl_call(tcli,[source,'drawing.tcl'],_),
	tcl_call(tcli,[mkdrawing,''],_).
%	tcl_call(shl_tcli,[source,'drawing.tcl'],_),
%	tcl_call(shl_tcli,[mkdrawing,''],_).

pro2tcl_options([], TclOptions, Interp)
	:-
%	(var(Interp) -> Interp = tcli ; true).
	(var(Interp) -> Interp = shl_tcli ; true).

pro2tcl_options([interp=Interp | Options], TclOptions, Interp)
	:-!,
	pro2tcl_options(Options, TclOptions, Interp).

pro2tcl_options([Tag=Value | Options], [Opt,Value | TclOptions], Interp)
	:-
	catenate('-',Tag,Opt),
	pro2tcl_options(Options, TclOptions, Interp).

export draw/8.
draw(Type,Canvas,AX,AY,BX,BY,Options,ID)
	:-
	pro2tcl_options(Options, TclOptions, Interp),
	tcl_call(Interp, [Canvas,create,Type,AX,AY,BX,BY | TclOptions], ID).
	
export line/4.
export line/5.
line(AX,AY, BX,BY)
	:-
	line(AX,AY, BX,BY, []).

line(AX,AY, BX,BY, Opts)
	:-
	draw(line,'.drawing.cnv.canvas',AX,AY,BX,BY,Opts,_).

export arc/6.
export arc/7.
arc(AX,AY, BX,BY,Start,Extent)
	:-
	arc(AX,AY, BX,BY,Start,Extent, []).

arc(AX,AY, BX,BY,Start,Extent, Opts)
	:-
	draw(arc,'.drawing.cnv.canvas',AX,AY,BX,BY,
		[start=Start,extent=Extent | Opts],_).

export oval/4.
export oval/5.
oval(AX,AY,BX,BY)
	:-
	oval(AX,AY,BX,BY,[]).

oval(AX,AY,BX,BY,Opts)
	:-
	draw(oval,'.drawing.cnv.canvas',AX,AY,BX,BY,Opts,_).

export rect/4.
export rect/5.
rect(AX,AY,BX,BY)
	:-
	rect(AX,AY,BX,BY,[]).

rect(AX,AY,BX,BY,Opts)
	:-
	draw(rectangle,'.drawing.cnv.canvas',AX,AY,BX,BY,Opts,_).

export poly/1.
export poly/2.
poly(PairList)
	:-
	poly(PairList,[]).

poly(PairList,Options)
	:-
	pro2tcl_options(Options, TclOptions, Interp),
	Canvas = '.drawing.cnv.canvas',
	append(PairList, TclOptions, TclCallList0),
	tcl_call(Interp, [Canvas,create,polygon | TclCallList0], ID).

tp :-
	poly([4,4,77,67,122,324],[outline=green,width=3.0]).
endmod.
