/*===================================================================*
 |			tables_demo.pro
 |		Copyright (c) 1998-9 Applied Logic Systems, Inc.
 |
 |		Demos of the use of routines (from tables.pro) for
 |		creation and manipulation of rectangular tables --
 |			Interfaces to tkTable
 |
 *===================================================================*/

:- consult('./tables.pro').

use tk_alslib.
use tcltk.

export td/1.

td(1)
	:-
	load_table_package,
	create_table(demotable,
		[title='My Test',
		 rowheadings=[row_0,row_1,'Row 2',r3,r4,r5,r6,r7,r8,r9],
		 height=5,
		 colheadings=['Zeroth','First','Second',third,fourth,fifth],
		 width=4
		 ]).

td(2) :-
	table_set_tag(demotable, title, 
				[background=yellow,
				 foreground=black,
				 relief=groove]).

td(3) :-
	table_set_tag(demotable, z1, 
				[background=green,
				 foreground=black]).

td(4) :-
	table_tag_region(demotable, z1, cell(2,2)).

td(5) :-
	set_demotable_col(1,1,[9,8,7,6,5,4,3,2,1,2,3,4,5]).

td(6) :-
	set_demotable_row(2,0,[-88,33.2,bad]).

td(7) :-
	read_demotable_table(2,2,X),
	printf(user_output, 'Value of cell %t,%t = %t\n', [2,2,X],[quoted(true)]).

tr
	:-
	load_table_package,
	create_table_r(demotable2,
		[rowheadings=[r_0,r_1,r_2,r_3],
		 lowerpane=true,
		 colheadings=['Z0','First','Second',third,fourth],
		 title='My Second Test'
		 ],R),
	write(r=R),nl.

td(x1)
	:-
	load_table_package,
	create_table_r(demotable,
		[title='My Test',
		 rowheadings=[row_0,row_1,'Row 2',r3,r4,r5,r6,r7,r8,r9],
		 height=5,
		 colheadings=['Zeroth','First','Second',third,fourth,fifth],
		 width=4,
		 menu=true,
		 upperpane=true,
		 lowerpane=true
		 ],tcli, [], R),
	R = [
	  TablePath,DataArray,InfoArray,TableWin,TableMenu,TblUpper,TblLower],
	  demox1_menu(TableMenu),
	  demox1_upper(TblUpper),
	  demox1_lower(TblLower).

demox1_menu(TableMenu)
	:-
	catenate(TableMenu,'.file',FileM),
	tcl_call(tcli,[menu,FileM,'-relief',raised,'-tearoff',0],_),
	tcl_call(tcli,
		[TableMenu,add,cascade,'-label','File','-menu',FileM],_),
	NewFileCmd='prolog call user demoxx -atom file -atom new',
	tcl_call(tcli,[FileM, add,command,'-label','New','-command',NewFileCmd],_),
	OpenFileCmd='prolog call user demoxx -atom file -atom open',
	tcl_call(tcli,[FileM, add,command,'-label','Open','-command',OpenFileCmd],_).

demoxx(B,C)
	:-
	write(menu_command=cmd(B,C)),nl.

demox1_upper(TblUpper)
	:-
	catenate(TblUpper,'.l1',LL),
	tcl_call(tcli,[label,LL,'-text','Label on Upper Pane'],_),
	tcl_call(tcli,[pack,LL,'-in',TblUpper],_).

demox1_lower(TblLower)
	:-
	catenate(TblLower,'.b2',BB),
	Btn='prolog call user btncmd',
	tcl_call(tcli,[button,BB,'-text','Lower Pane Button','-command',Btn],_),
	tcl_call(tcli,[pack,BB,'-in',TblLower],_).


btncmd :- 
	write('Button was pressed'),nl,
	(yoff ->
		table_set_tag(demotable, title, 
				[background=yellow,
				 foreground=black,
				 relief=groove]),
		retract(yoff)
		;
		table_set_tag(demotable, title, 
				[background=white,
				 foreground=black,
				 relief=groove]),
		assert(yoff)
	).

