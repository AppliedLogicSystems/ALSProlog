/*========================================================================*
 |			tables.pro
 |		Copyright (c) 1998 Applied Logic Systems, Inc.
 |
 |		Creation and manipulation of rectangular tables --
 |			Interfaces to tkTable
 |
 *========================================================================*/

tbl1
	:-
	load_table_package,
	create_table(foobar,
		[numrows=9,
		 rowheadings=[row_1,'Row 2',r3,r4,r5,r6,r7,r8,r9],
		 colheadings=['First','Second',third,fourth],
		 numcols = 7,
		 title='My Test'
		 ]).

tt1 :-
	table_set_tag(foobar, title, 
				[background=yellow,
				 foreground=black,
				 relief=groove]).

tt2 :-
	table_set_tag(foobar, z1, 
				[background=green,
				 foreground=black]).

tt3 :-
	table_tag_region(foobar, z1, cell(2,2)).

tt4 :-
	set_foobar_col(1,2,[9,8,7,6,5,4,3,2,1,2,3,4,5]).


module tables.
use tcltk.
use tk_alslib.


export load_table_package/0.
export load_table_package/1.

load_table_package
	:-
	load_table_package(tcli).

load_table_package(Interp)
	:-
	init_tk_alslib(Interp,_),
	builtins:sys_searchdir(SysSearchdir),
	split_path(SysSearchdir, SSDList),
	append(SSDList, [shared], SharedList),
	join_path(SharedList, SharedPath),
	append(SharedList, ['tables.tcl'], TablesTclList),
	join_path(TablesTclList, TablesTclPath),
	tcl_call(Interp, [source,TablesTclPath], _),
	tcl_call(Interp, [load_table_package, SharedPath], X).

:- compiletime, module_closure( create_table, 2, create_table3).
:- compiletime, module_closure( create_table, 3, create_table4).

create_table3(Mod, TableName, Options)
	:-
	create_table4(Mod, TableName, Options, tcli).

create_table4(Mod, TableName, Options, Interp)
	:-
	((dmember(colheadings=ColHeadings, Options),
		ColHeadings \= [])
		->
		length(ColHeadings, NCols),
		NTitleRows = 1,
		Roworigin = -1 
		;
		check_default(Options, numcols, 6, NCols),
		ColHeadings = [],
		NTitleRows = 0,
		Roworigin =  0
	),
	((dmember(rowheadings=RowHeadings, Options),
		RowHeadings \= [])
		->
		length(RowHeadings, NRows),
		NTitleCols = 1,
		Colorigin = -1
		;
		check_default(Options, numrows, 6, NRows),
		RowHeadings = [],
		NTitleCols = 0,
		Colorigin =  0
	),
	check_default(Options, title,		'',			Title),
	check_default(Options, tablefont,	['Times', 9, normal], Tablefont),
	check_default(Options, foreground,	black,		Foreground),
	check_default(Options, background, '#b9b9b9',	Background),
	check_default(Options, selectmode,	extended,	Selectmode),
	check_default(Options, rowstretch,	unset,		Rowstretch),
	check_default(Options, colstretch,	all,		Colstretch),
	check_default(Options, flashmode,	on,			Flashmode),

	OptionsList = [
		numcols,		NCols,
		colheadings, ColHeadings,
		titlerows,	NTitleRows,
		roworigin,	Roworigin,
		numrows,		NRows,
		rowheadings, RowHeadings,
		titlecols,	NTitleCols,
		colorigin,	Colorigin,
		title,		Title,
		tablefont,	Tablefont,
		foreground,	Foreground,
		background, Background,
		selectmode,	Selectmode,
		rowstretch,	Rowstretch,
		colstretch,	Colstretch,
		flashmode,	Flashmode
	],

	catenate(TableName, '_info', TableInfoArrayName),
	tcl_call(Interp, 
				[build_table,TableName,TableInfoArrayName,OptionsList],_),

	catenate(TableName, '_array', TableArrayName),
	catenate(['set_', TableName, '_table'], SetTableArrayPred),
	catenate(['read_', TableName, '_table'], ReadTableArrayPred),
	SetHead =.. [SetTableArrayPred,Row,Col,Val],
	ReadHead =.. [ReadTableArrayPred,Row,Col,Val],
	Mod:assert( (SetHead :-
		tcl_call(Interp, [set_table,TableArrayName,Row,Col,Val], _)
		) ),
	Mod:assert( 
		(ReadHead :-
		tcl_call(Interp, [read_table,TableArrayName,Row,Col], Val)
		) ),

	Mod:assert(basic_table_info(TableName,NRows,NCols)),
	catenate(['set_', TableName, '_row'], SetTableRowPred),
	SetRowHead =.. [SetTableRowPred,RowN,ColStart,ValsList],
	Mod:assert( (
		SetRowHead :-
		Lim is NCols - ColStart,
		tcl_call(Interp, [write_table_row,TableArrayName,RowN,ColStart,Lim,ValsList], _)
		) ),

	catenate(['set_', TableName, '_col'], SetTableColPred),
	SetColHead =.. [SetTableColPred,ColN,RowStart,ValsList],
	Mod:assert( (
		SetColHead :-
		Lim is NRows - RowStart,
write(calling_tcl_call(Interp, [write_table_col,TableArrayName,ColN,RowStart,Lim,ValsList], _)),nl,flush_output,
		tcl_call(Interp, [write_table_col,TableArrayName,ColN,RowStart,Lim,ValsList], _)
		) ).

export table_set_tag/3.
export table_set_tag/4.
table_set_tag(TableName, TagName, Properties)
	:-
	table_set_tag(TableName, TagName, Properties, tcli).

table_set_tag(TableName, TagName, Properties, Interp)
	:-
	check_tag_props(Properties,
					[anchor,background,bg,font,foreground,fg,image,relief],
					TclProps),
	catenate(['.', TableName, '_toplevel.table'], TablePath),
	tcl_call(Interp, [TablePath,tag,configure,TagName | TclProps], _).

check_tag_props([], _, []).
check_tag_props([Tag=Val | Properties], OKTags, [TclTag,TclVal | TclProps])
	:-
	check_tcl_tag(Tag,Val,TclTag,TclVal),
	!,
	check_tag_props(Properties, OKTags, TclProps).
check_tag_props([Tag=Val | Properties], OKTags, TclProps)
	:-
	check_tag_props(Properties, OKTags, TclProps).

check_tcl_tag(Tag,Val,TclTag,TclVal)
	:-
	check_tcl_tag_value(Tag,Val,TclVal),
	catenate('-',Tag,TclTag).


check_tcl_tag_value(anchor,Val,Val)
	:-
	dmember(Val, [center,n,ne,e,se,s,sw,w,nw]).

check_tcl_tag_value(background,Val,Val)
	:-
	colorname(Val).

check_tcl_tag_value(bg,Val,Val)
	:-
	colorname(Val).

check_tcl_tag_value(foreground,Val,Val)
	:-
	colorname(Val).

check_tcl_tag_value(fg,Val,Val)
	:-
	colorname(Val).

check_tcl_tag_value(relief,Val,Val)
	:-
	dmember(Val, [raised,sunken,flat,groove,ridge]).

check_tcl_tag_value(image,Val,Val)
	:-!.

check_tcl_tag_value(font,Font,TclFont)
	:-
	check_font(Font, TclFont).

	%% extend???
check_font([Family,Size,Style], [Family,Size,Style]).

	%% needs real specification:
colorname(Val).


cell_index(R,C,Index)
	:-
	number_codes(R,RCs),
	number_codes(C,CCs),
	append(RCs,[0', | CCs], ICs),
	atom_codes(Index, ICs).

export table_tag_region/3.
export table_tag_region/4.
table_tag_region(TableName, TagName, Region)
	:-
	table_tag_region(TableName, TagName, Region, tcli).

table_tag_region(TableName, TagName, Region, Interp)
	:-
	catenate(['.', TableName, '_toplevel.table'], TablePath),
	apply_region_tag(Region, TablePath, TagName, Interp).

apply_region_tag([], TablePath, TagName, Interp).
apply_region_tag([H | T], TablePath, TagName, Interp)
	:-
	apply_region_tag(T, TablePath, TagName, Interp),
	apply_region_tag(T, TablePath, TagName, Interp).

apply_region_tag(cell(R,C), TablePath, TagName, Interp)
	:-
	cell_index(R,C,Index),
	tcl_call(Interp,[TablePath,tag,cell,TagName,Index], _).

apply_region_tag(row(R), TablePath, TagName, Interp)
	:-
	tcl_call(Interp,[TablePath,tag,row,TagName,R], _).

apply_region_tag(col(C), TablePath, TagName, Interp)
	:-
	tcl_call(Interp,[TablePath,tag,col,TagName,C], _).

endmod.
