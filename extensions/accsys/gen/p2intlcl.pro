/*-------------------------------------------------------------*
                    p2i.pro
               defStruct Type definitions generated from file:
                    p2i.typ
               by ALS defStruct Type Generator
               Macros written to file: p2i.mac
 *-------------------------------------------------------------*/

module utilities.
use pro2intf.
endmod.

module pro2intf.
use utilities.


%--- p2i defStruct ---

export accessP2I/3.
export setP2I/3.
accessP2I(inFile,_A,_B) :- arg(1,_A,_B).
setP2I(inFile,_A,_B) :- mangle(1,_A,_B).

accessP2I(inStream,_A,_B) :- arg(2,_A,_B).
setP2I(inStream,_A,_B) :- mangle(2,_A,_B).

accessP2I(baseName,_A,_B) :- arg(3,_A,_B).
setP2I(baseName,_A,_B) :- mangle(3,_A,_B).

accessP2I(hincs,_A,_B) :- arg(4,_A,_B).
setP2I(hincs,_A,_B) :- mangle(4,_A,_B).

accessP2I(fPrefix,_A,_B) :- arg(5,_A,_B).
setP2I(fPrefix,_A,_B) :- mangle(5,_A,_B).

accessP2I(dPred,_A,_B) :- arg(6,_A,_B).
setP2I(dPred,_A,_B) :- mangle(6,_A,_B).

accessP2I(tPred,_A,_B) :- arg(7,_A,_B).
setP2I(tPred,_A,_B) :- mangle(7,_A,_B).

accessP2I(curONum,_A,_B) :- arg(8,_A,_B).
setP2I(curONum,_A,_B) :- mangle(8,_A,_B).

accessP2I(curDispNum,_A,_B) :- arg(9,_A,_B).
setP2I(curDispNum,_A,_B) :- mangle(9,_A,_B).

accessP2I(initFcns,_A,_B) :- arg(10,_A,_B).
setP2I(initFcns,_A,_B) :- mangle(10,_A,_B).

accessP2I(outFile,_A,_B) :- arg(11,_A,_B).
setP2I(outFile,_A,_B) :- mangle(11,_A,_B).

accessP2I(outStream,_A,_B) :- arg(12,_A,_B).
setP2I(outStream,_A,_B) :- mangle(12,_A,_B).

accessP2I(cFileList,_A,_B) :- arg(13,_A,_B).
setP2I(cFileList,_A,_B) :- mangle(13,_A,_B).

accessP2I(pFileList,_A,_B) :- arg(14,_A,_B).
setP2I(pFileList,_A,_B) :- mangle(14,_A,_B).

accessP2I(pfStreams,_A,_B) :- arg(15,_A,_B).
setP2I(pfStreams,_A,_B) :- mangle(15,_A,_B).

accessP2I(module,_A,_B) :- arg(16,_A,_B).
setP2I(module,_A,_B) :- mangle(16,_A,_B).

accessP2I(fmax,_A,_B) :- arg(17,_A,_B).
setP2I(fmax,_A,_B) :- mangle(17,_A,_B).

accessP2I(smax,_A,_B) :- arg(18,_A,_B).
setP2I(smax,_A,_B) :- mangle(18,_A,_B).

accessP2I(cmax,_A,_B) :- arg(19,_A,_B).
setP2I(cmax,_A,_B) :- mangle(19,_A,_B).

export makeP2I/1.
makeP2I(_A) :-
        _A=..
            [p2i,nil,nil,nil,[],nil,nil,nil,0,0,[],nil,nil,[],[],[],builtins,
                100000,100000,100000].

export makeP2I/2.
makeP2I(_A,_B) :-
        struct_lookup_subst(
            [inFile,inStream,baseName,hincs,fPrefix,dPred,tPred,curONum,
                curDispNum,initFcns,outFile,outStream,cFileList,pFileList,
                pfStreams,module,fmax,smax,cmax],
            [nil,nil,nil,[],nil,nil,nil,0,0,[],nil,nil,[],[],[],builtins,
                100000,100000,100000],
            _B,_C),
        _A=..[p2i|_C].

export xmakeP2I/2.
xmakeP2I(p2i(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S),
    [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S]).

endmod.

module utilities.
typeProperties(p2i,
    [inFile,nil,inStream,nil,baseName,nil,hincs,[],fPrefix,nil,dPred,nil,
        tPred,nil,curONum,0,curDispNum,0,initFcns,[],outFile,nil,outStream,
        nil,cFileList,[],pFileList,[],pfStreams,[],module,builtins,fmax,
        100000,smax,100000,cmax,100000]).
noteOptionValue(p2i,_A,_B,_C) :- setP2I(_A,_C,_B).
endmod.
/*=======================================================================*
 |		pro2intf
 |      Copyright (c) 1991-4 Applied Logic Systems, Inc.
 |
 |	-- C Interface generator for compact-style interfaces
 |
 | Date : 2 Oct 1991
 | Author : P. Raman and Ron DiNapoli
 | Modifications by K.Bowen: July,1994
 *=======================================================================*/

/*-----------------------------------------------------------------------*
 | OS command line:
 |  	alspro interf -g gen_interf -p Name Prefix Options
 |
 |	The arguments after the -p flag are described below.
 |		Name	is the base name for input and output files 
 |	Valid Options are
 |		-pre Prefix	is the prefix for C interface functions
 |		-f Num 	max number of function interfaces per output file
 |		-s Num	max number of types/structure interfaces per output file
 |		-c Num	max number of constant interfaces per output file
 |		-t Pred  translation predicate (for use with compact int. gen.)
 |				user:Pred(+NameStr,-Name) is called.
 |		-d Pred  pattern generation predicate for duplicate elimination.
 |				user:Pred(+Flag,+Object,-Pattern) is called with Flag bound to
 |				func, gvar, ctype or const, and Object with input object.
 |		-D Flag	asserts Flag; currently the following flags are recognized
 |				debug	- verbose mode
 |				doswin	- generate DOS windows specific interface
 |				compact	- generate compact interface  (Added 8/12/92 by Ron)
 |						  (this is now the default -- raman 10/26/92)
 |		-mod Module for the prolog interface predicates
 |
 | INPUT	:
 |
 |	Input file	: 'Name'.src
 |
 |	Input syntax :
 |
 |	The following input terms are processed, and any unrecognized
 |	terms are ignored.
 |
 |		ret(Cast)/PType = FuncName(Cast/PType, ... )/File
 |		als_type(Name,PType)
 |		als_const(Name,KeyWord)
 |		als_struct(Name,[FieldName=PType,...])
 |		als_gvar(Name,Cast,PType)/File
 |	where
 |		File	is the file where the function was found
 |		Decln	is a symbol that declares the return value
 |		Cast	is a symbol that is the type cast in C
 |		PType	is a list of KeyWords that spells out the type of item
 |		KeyWord is one of ptr, func, array, int, long, short, char, float,
 |				double, unsigned_int, unsigned_long and far
 |
 |
 | OUTPUT	:
 |
 |	Output files : 'Name'0.c through 'Name'n.c
 |
 |	Interface init func : 'Name'_init()
 |
 *-----------------------------------------------------------------------*/

module pro2intf.

:-dynamic(debug/0).
:-dynamic(doswin/0).

export gen_interf/0.
export pro2intf/0.

gen_interf 
	:-
	pro2intf.

pro2intf 
	:-
	makeP2I(State),
	get_cmdline_vals(CmdLine),
	process_options(CmdLine,State),
	check_defaults(CmdLine,State),
	gen_compact_interf(State).

	%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Command Line Options
	%%%%%%%%%%%%%%%%%%%%%%%%%
process_options([], _).
process_options([First | Rest], State) 
	:-
	dispatch_option(First, State),
	process_options(Rest, State).

dispatch_option(['-null_switch' ,BaseName], State) 
	:-!,
	setP2I(baseName, State, BaseName),
	inSuffix(Suffix),
	filePlusExt(BaseName, Suffix, InFile),
	setP2I(inFile, State, InFile).

dispatch_option(['-f' ,Str], State) 
	:-			% -f Num
	atomread(Str, Num),
	integer(Num),
	!,
	setP2I(fmax, State, Num).

dispatch_option(['-s' ,Str], State) 
	:-			% -s Num
	atomread(Str, Num),
	integer(Num),
	!,
	setP2I(smax, State, Num).

dispatch_option(['-c' ,Str], State) 
	:-			% -c Num
	atomread(Str, Num),
	integer(Num),
	!,
	setP2I(cmax, State, Num).

dispatch_option(['-fpre' ,FPrefix], State) 
	:-!,
	setP2I(fPrefix, State, FPrefix).

dispatch_option(['-t' ,UserPred], State) 
	:-!,
	setP2I(tPred, State, UserPred).

dispatch_option(['-T' ,TranslateExpr], State) 
	:-!,
	setup_translate(TranslateExpr, State).

dispatch_option(['-d' ,DupPred], State) 
	:-!,
	setP2I(dPred, State, DupPred).

dispatch_option(['-mod' ,Mod], State) 
	:-!,
	setP2I(module, State, Mod).

dispatch_option(['-D' ,Flag], State) 
	:-!,	
	assert(Flag).

	%% This version of the -D switch is supported because this is
	%% the way it is used in C-land;
dispatch_option([Switch], State) 
	:-	
	sub_atom(Switch,1,2,'-D'),
	atom_length(Switch,LS),
	LenTail is LS - 2,
	sub_atom(Switch,3,LenTail,Flag),
	!,
	assert(Flag).

dispatch_option(['-hinc', HIncs], State) 
	:-!,
	atomread(HIncs, TheHIncs),
	setP2I(hincs, State, TheHIncs).

dispatch_option(Switch, State) 
	:-
	printf(error_stream,
		'pro2intf: Unknown command line switch: %t .. skipping\n',
		[Switch]).

check_defaults([],State) :-!.
check_defaults(CmdLine,State)
	:-
	(dmember(['-hinc', _], CmdLine) -> 
		true ;
		accessP2I(baseName, State, BaseName),
		setP2I(hincs, State, [BaseName])
	).
		%% Add more here as necessary

setup_translate(InExpr, State)
	:-
	atomread(InExpr, Expr),
	(Expr = (Head :- Body) ->
		THead = Head
		;
		THead = Expr
	),
	assert(Expr),
	functor(THead, UserPred, _),
	setP2I(tPred, State, UserPred).

%%----------------------------------------------
%%	TOP LEVEL OF COMPACT INTERFACE GENERATOR
%%----------------------------------------------

gen_compact_interf(State) 
	:-
	get_protos(FList,TList,CList,GVList, State),
	gen_compact_interf(FList,TList,CList,GVList,State),
	debug('Done generating interface\n'),
	!.
gen_compact_interf(State)
	:-
	accessP2I(baseName,State,BaseName),
	printf(error_stream,
		'>>>gen_compact_interf/3 on base name = %t failed<<<\n',
		[BaseName]).
	
gen_compact_interf(FList,TList,CList,GVList,State)
	:-
	openOutputFile(State),
	gen_compact_func_interf(FList,AllFuncsList,State),
	gen_table_init(State),
	gen_type_interf(TList,State),
	gen_constants_interf(CList,State),
	gen_global_variable_interf(GVList,State),
	write_compact_interf_init(State),
	closeOutputFile(State),
	gen_dot_pro_files(AllFuncsList,GVList,State).
	
%

get_protos(FList,TList,CList,GList, State)
	:-
	openInputFile(State, InStream),
	get_protos(FList,TList,CList,GList, InStream,State),
	closeInputFile(State).

get_protos(FList,TList,CList,GList, InStream,State)
	:-
	read(InStream, Item),
	!,
	dispatch(Item,FList,TList,CList,GList, InStream,State).
get_protos([], [],[],[], _,State).


dispatch( end_of_file, [], [], [], [], _,State) :- !.

		%% Function spec:
dispatch(RetSpec=FSpec, [RetSpec=FSpec|FList], TList, CList, GList, Stream,State)
	:- !,			
	get_protos(FList, TList, CList, GList, Stream,State).

		%% Struct spec:
dispatch(als_struct(Name,Fields), FList,
		 [als_struct(Name,Fields)|TList], CList, GList, Stream,State)
	:- !,
	get_protos(FList, TList, CList, GList, Stream,State).

		%% Type spec:
dispatch(als_type(Name,PType), FList,
		 [als_type(Name,PType)|TList], CList, GList, Stream,State)
	:- !,
	get_protos(FList, TList, CList, GList, Stream,State).

		%% Const spec:
dispatch(als_const(Name,Type), FList,TList, 
		 [als_const(Name,Type)|CList], GList, Stream,State)
	:- !,
	get_protos(FList, TList, CList, GList, Stream,State).

		%% Gvar spec:
dispatch(als_gvar(Name,Cast,PType)/File,
		 FList, TList, CList, [als_gvar(Name,Cast,PType)/File|GList], Stream,State)
	:- !,			
	get_protos(FList, TList, CList, GList, Stream,State).

		%% Ignore all other specs:
dispatch( _, FList, TList, CList, GList, Stream,State)
	:- 
	get_protos(FList, TList, CList, GList, Stream,State).

%%-----------------------------------------
%%	COMPACT FUNCTION INTERFACE
%%-----------------------------------------

gen_compact_func_interf([],[],State)
	:- !.
gen_compact_func_interf(FList,ListOfListsOfLists,State)
	:-
	debug('Generating COMPACT function interface:\n'),
	reverse(FList,List1),
	filter_dups(List1,func,[],List2),
	sort_by_arity(List2,ListOfLists),
	sort_by_arguments(ListOfLists,ListOfListsOfLists),
	length(FList,RawNumFuncs),
	length(List2,NumUniqueFuncs),
	length(ListOfLists,NumArities),
	debug("\tNumber of functions: initial=%t, final=%t, arities=%t\n",
		[RawNumFuncs,NumUniqueFuncs,NumArities]),
	gen_compact_func_interf1(ListOfListsOfLists,0,State),
	accessP2I(outStream, State, OutStream),
	gen_pi_pdefines(ListOfListsOfLists,OutStream),
	debug('\nDone generating COMPACT function interface\n').

gen_compact_func_interf1([],_,State).
gen_compact_func_interf1([ArityGroup|RestGroups],Count,State)
	:-
	gen_arity_group_interf(ArityGroup,Count,NewCount,State),
	!,
	gen_compact_func_interf1(RestGroups,NewCount,State).


gen_arity_group_interf([],Count,Count,State).
gen_arity_group_interf([func_group(TL,FList,DFN,CDFN,Arity)|Rest],Count,NewCount,State)
	:-
	accessP2I(fmax, State, FMax),
	length(FList,NumFuncs),
	NextCount is Count + NumFuncs,
	NextCount > FMax,
	Count > 0,
	!,
	switchOutputFile(State),
	write_dispatch_header(TL,DFN,CDFN,Arity,State),
	write_dispatch_body(FList,State),
	write_dispatch_trailer(TL,State),
	gen_arity_group_interf(Rest,NumFuncs,NewCount,State).
gen_arity_group_interf([func_group(TL,FList,DFN,CDFN,Arity)|Rest],Count,NewCount,State)
	:-
	length(FList,NumFuncs),
	NextCount is Count + NumFuncs,
	write_dispatch_header(TL,DFN,CDFN,Arity,State),
	write_dispatch_body(FList,State),
	write_dispatch_trailer(TL,State),
	debug('.'), 
	gen_arity_group_interf(Rest,NextCount,NewCount,State).

increment_Num(Slot,State,NextNum)
	:-
	accessP2I(Slot, State, CurNum),
	NextNum is CurNum + 1,
	setP2I(Slot, State, NextNum).

write_dispatch_header([RetType|ArgTypes],DispatchFuncName,CDispatchFuncName,Arity,State)
	:-
	accessP2I(curDispNum,State,DN),
	increment_Num(curDispNum,State,_),
	accessP2I(outStream, State, OutStream),
	accessP2I(fPrefix, State, FPrefix),
	printf(OutStream,'static int %t_dispatch%t(void)\n{\n',[FPrefix,DN]),
	sprintf(CDFNList,'%t_dispatch%t',[FPrefix,DN]),
	sprintf(DFNList,'%t%d',[FPrefix,DN]),
	name(CDispatchFuncName,CDFNList),
	name(DispatchFuncName,DFNList),
	gen_dispatch_vars(RetType,ArgTypes,Arity,OutStream),
	set_up_args(RetType,ArgTypes,OutStream),
	begin_switch(OutStream).

write_dispatch_body(FuncList,State)
	:-
	accessP2I(outStream, State, OutStream),
	writeDispatchBody(FuncList,0,OutStream).

write_dispatch_trailer(TypeList,State)
	:-
	accessP2I(outStream, State, OutStream),
	emit_default(OutStream),
	printf(OutStream,'\t}\n',[]),
	emit_unify(TypeList,OutStream),
	printf(OutStream,'}\n\n',[]).

    %---------------------------------------------------
    %	predicates called from write_dispatch_header/3
    %---------------------------------------------------

gen_dispatch_vars(RetType,ArgTypes,FinalArity,OutStream)
	:-
	gen_ret_var_declaration(RetType,'retval',OutStream),
	gen_ret_arg_declaration(RetType,OutStream),
	gen_arg_declaration(integer,1,OutStream),	%	Generate declaration for the dispatch arg
	genDispatchVars(ArgTypes,2,RetType,Arity,OutStream),
	( (RetType = void) ->
		FinalArity is Arity - 1;
		FinalArity = Arity
	).

genDispatchVars([],Num,void,Num,OutStream)
	:- !, 
	nl(OutStream).
genDispatchVars([],Num,_,Num,OutStream)
	:-
	gen_arg_declaration(integer,Num,OutStream),	% Generate a declaration for the return arg
	nl(OutStream).
genDispatchVars([Type|Rest],Num,RetType,FinalArity,OutStream)
	:-
	gen_arg_declaration(Type,Num,OutStream),
	( (Type = void) ->
		NextNum = Num;
		NextNum is Num + 1
	),
	genDispatchVars(Rest,NextNum,RetType,FinalArity,OutStream).

gen_ret_arg_declaration(void,OutStream)
	:- !.
gen_ret_arg_declaration(_,OutStream)
	:-
	printf(OutStream,'\tPWord rval; int rtype;\n',[]).

gen_ret_var_declaration(void,_,OutStream).
gen_ret_var_declaration(aggreg,VarName,OutStream)
	:-
	printf(OutStream,'\tunsigned long %s;\n',[VarName]).
gen_ret_var_declaration(integer,VarName,OutStream)
	:-
	printf(OutStream,'\tunsigned long %s;\n',[VarName]).
gen_ret_var_declaration(float,VarName,OutStream)
	:-
	printf(OutStream,'\tdouble %s;\n',[VarName]).
gen_ret_var_declaration(farptr,VarName,OutStream)
	:-
	printf(OutStream,'\tchar far * %s;\n',[VarName]).


gen_arg_declaration(void,_,OutStream).
gen_arg_declaration(integer,Num,OutStream)
	:-
	printf(OutStream,'\tPWord arg%d; int type%d;\n',[Num,Num]).
gen_arg_declaration(float,Num,OutStream)
	:-
	printf(OutStream,'\tPWord arg%d; int type%d;\n',[Num,Num]),
	printf(OutStream,'\tdouble float%d;\n',[Num]).
gen_arg_declaration(string,Num,OutStream)
	:-
	printf(OutStream,'\tPWord arg%d; int type%d;\n',[Num,Num]).
gen_arg_declaration(farptr,Num,OutStream)
	:-
	printf(OutStream,'\tPWord arg%d; int type%d;\n',[Num,Num]),
	printf(OutStream,'\tchar far *farptr%d;\n',[Num]).
gen_arg_declaration(farstr,Num,OutStream)
	:-
	printf(OutStream,'\tPWord arg%d; int type%d;\n',[Num,Num]),
	printf(OutStream,'\tchar far *farstr%d;\n',[Num]).


begin_switch(OutStream)
	:-
	printf(OutStream,'\tswitch(arg1)\n\t{\n',[]).

set_up_args(RetType,Args,OutStream)
	:-
	emit_getan([integer],1,_,OutStream),		% Emit arg1,type1 for dispatch num
	emit_getan(Args,2,LastArgNum,OutStream),
	emit_return_arg_getan(RetType,LastArgNum,OutStream),
	nl(OutStream),
	nl(OutStream).

emit_getan([],LastArg,LastArg,OutStream).
emit_getan([void|Rest],ArgNum,LastArgNum,OutStream)
	:- !,
	emit_getan(Rest,ArgNum,LastArgNum,OutStream).
emit_getan([Type|Rest],ArgNum,LastArgNum,OutStream)
	:-
	printf(OutStream,'\tPI_getan(&arg%d,&type%d,%d);\n',[ArgNum,ArgNum,ArgNum]),
	emit_type_check(Type,ArgNum,OutStream),
	NextArg is ArgNum + 1,
	emit_getan(Rest,NextArg,LastArgNum,OutStream).

emit_type_check(void,_,OutStream).
emit_type_check(integer,Num,OutStream)
	:-
	printf(OutStream,'\tif (type%d != PI_INT)\n',[Num]),
	printf(OutStream,'\t\tif (!CI_get_integer(&arg%d,type%d))\n',[Num,Num]),
	printf(OutStream,'\t\t\tPI_FAIL;\n',[]).
emit_type_check(float,Num,OutStream)
	:-
	printf(OutStream,'\tif (!CI_get_double(&float%d,arg%d,type%d))\n',[Num,Num,Num]),
	printf(OutStream,'\t\tPI_FAIL;\n',[]).
emit_type_check(string,Num,OutStream)
	:-
	printf(OutStream,'\tif (type%d == PI_SYM)\n',[Num]),
	printf(OutStream,'\t\targ%d = (unsigned long) PI_getsymname(0,arg%d,0);\n',[Num,Num]),
	printf(OutStream,'\telse if (!CI_get_integer(&arg%d,type%d))\n',[Num,Num]),
	printf(OutStream,'\t\tPI_FAIL;\n',[]).
emit_type_check(farptr,Num,OutStream)
	:-
	printf(OutStream,'\tif (!CI_get_farptr(&farptr%d,arg%d,type%d))\n',[Num,Num,Num]),
	printf(OutStream,'\t\tPI_FAIL;\n',[]).
emit_type_check(farstr,Num,OutStream)
	:-
	printf(OutStream,'\tif (type%d == PI_SYM )\n',[Num]),
	printf(OutStream,'\t\tfarstr%d = (char _Far *) PI_getsymname(0,arg%d,0);\n',[Num,Num]),
	printf(OutStream,'\telse if (!CI_get_farptr(&farstr%d,arg%d,type%d))\n',[Num,Num,Num]),
	printf(OutStream,'\t\tPI_FAIL;\n',[]).

emit_return_arg_getan(void,_,OutStream)
	:- !.
emit_return_arg_getan(_,Num,OutStream)
	:-
	printf(OutStream,'\tPI_getan(&arg%d,&type%d,%d);\n',[Num,Num,Num]).

    %-----------------------------------------------------
    %	predicates called from write_dispatch_body/1
    %-----------------------------------------------------

writeDispatchBody([],_,OutStream).
writeDispatchBody([Func|Rest],Label,OutStream)
	:-
	Func = (ret(Cast)/Type=FuncDesc),
	emit_case_label(Label,OutStream),
	emit_return_var(Type,Cast,OutStream),
	emit_function_call(FuncDesc,OutStream),
	emit_function_args(FuncDesc,OutStream),
	NewLabel is Label + 1,
	writeDispatchBody(Rest,NewLabel,OutStream).

emit_case_label(Label,OutStream)
	:-
	printf(OutStream,'\t\tcase %d:\n',[Label]).

emit_return_var([void],_,OutStream)
	:-
	!,
	printf(OutStream,'\t\t\t',[]).
emit_return_var([struct|_],Cast,OutStream)
	:-
	!,
	printf(OutStream,'\t\t\tPI_allocuia(&rval,&rtype,sizeof(%t));\n',[Cast]),
	printf(OutStream,'\t\t\tretval=(unsigned long)PI_getuianame(0,rval,0);\n',[]),
	printf(OutStream,'\t\t\t*(%t *)retval = ',[Cast]).
emit_return_var([array(_)|_],Cast,OutStream)
	:-
	!,
	printf(OutStream,'\t\t\tPI_allocuia(&rval,&rtype,sizeof(%t));\n',[Cast]),
	printf(OutStream,'\t\t\tretval=(unsigned long)PI_getuianame(0,rval,0);\n',[]),
	printf(OutStream,'\t\t\t*(%t *)retval = ',[Cast]).
emit_return_var([far,ptr|_],Cast,OutStream)
	:-
	!,
	printf(OutStream,'\t\t\tretval = (char far *) ',[]).
emit_return_var([far,array(_)|_],Cast,OutStream)
	:-
	!,
	printf(OutStream,'\t\t\tretval = (char far *) ',[]).
emit_return_var(_,Cast,OutStream)
	:-
	printf(OutStream,'\t\t\tretval = (unsigned long) ',[Cast]).

emit_function_call(Func/_,OutStream)
	:-
	Func =.. [FuncName|_],
	write(OutStream,FuncName),
	put_byte(OutStream, 0'( ).

emit_function_args(Func/_,OutStream)
	:-
	Func =.. [_|Args],
	emitFunctionArgs(Args,2,OutStream),
	printf(OutStream,');\n\t\t\tbreak;\n',[]).

emitFunctionArgs([],_,OutStream).
emitFunctionArgs([_/[void]],_,OutStream)
	:- !.
emitFunctionArgs([Cast/TypeList|Rest],ArgNum,OutStream)
	:-
	classifyArgType(TypeList,Type),
	comply_with_ANSI(TypeList,Cast,ANSICast),
	emitFunctionArg(Type,ANSICast,ArgNum,Rest,OutStream), 
	NextArg is ArgNum + 1,
	emitFunctionArgs(Rest,NextArg,OutStream).

emitFunctionArg(void,_,_,_,OutStream).
emitFunctionArg(integer,Cast,Num,Comma,OutStream)
	:-
	printf(OutStream,'(%t arg%d)',[Cast,Num]),
	put_comma(Comma,OutStream).
emitFunctionArg(float,Cast,Num,Comma,OutStream)
	:-
	printf(OutStream,'(%t float%d)',[Cast,Num]),
	put_comma(Comma,OutStream).
emitFunctionArg(string,Cast,Num,Comma,OutStream)
	:-
	printf(OutStream,'(%t arg%d)',[Cast,Num]),
	put_comma(Comma,OutStream).
emitFunctionArg(farptr,Cast,Num,Comma,OutStream)
	:-
	printf(OutStream,'(%t farptr%d)',[Cast,Num]),
	put_comma(Comma,OutStream).
emitFunctionArg(farstr,Cast,Num,Comma,OutStream)
	:-
	printf(OutStream,'(%t farstr%d)',[Cast,Num]),
	put_comma(Comma,OutStream).

put_comma([],OutStream).
put_comma([_|_],OutStream)
	:-
	put_byte(OutStream,0', ).

%-------------------------------------------------------------------------
%	The ANSI standard does not allow a cast to be a data structure.
%	All casts must be 'scalar'.  To accomodate this, any time we are
%	casting to something which is a plain struct, we tack on a *( at 
%	the beginning and a *) at the end. 
%
%	The end result will be instead of having a cast like:
%		(Point )
%	we would have...
%		*(Point *) 
%-------------------------------------------------------------------------

comply_with_ANSI([struct | _],Cast,ANSICast)
	:-
	!,
	name(Cast,CastList),
	append("*(",CastList,TempCastList),
	append(TempCastList,"*)",ANSICastList),
	name(ANSICast,ANSICastList).
comply_with_ANSI([array(_) | _],Cast,ANSICast)
	:-
	!,
	name(Cast,CastList),
	append("*(",CastList,TempCastList),
	append(TempCastList,"*) &",ANSICastList),
	name(ANSICast,ANSICastList).
comply_with_ANSI(_,Cast,ANSICast)
	:-
	sprintf(ANSICastList,'(%t )',[Cast]),
	name(ANSICast,ANSICastList).

    %-----------------------------------------------------
    %	predicates called from write_dispatch_trailer/1.
    %-----------------------------------------------------

emit_default(OutStream)
	:-
	printf(OutStream,'\t\tdefault:\n\t\t\tPI_FAIL;\n',[]).

emit_unify([void|_],OutStream)
	:-
	!,
	printf(OutStream,'\tPI_SUCCEED;\n',[]).
emit_unify([aggreg|Rest],OutStream)
	:-
	!,
	calc_return_arg_num(Rest,2,RetArg),
	printf(OutStream,'\tif (PI_unify(arg%d,type%d,rval,rtype))\n',[RetArg,RetArg]),
	printf(OutStream,'\t\tPI_SUCCEED;\n',[]),
	printf(OutStream,'\tPI_FAIL;\n',[]).
emit_unify([farptr|Rest],OutStream)
	:-
	!,
	calc_return_arg_num(Rest,2,RetArg),
	printf(OutStream,'\tCI_makefar(&rval,&rtype,(unsigned short *)&retval);\n',[]),
	printf(OutStream,'\tif (PI_unify(arg%d,type%d,rval,rtype))\n',[RetArg,RetArg]),
	printf(OutStream,'\t\tPI_SUCCEED;\n',[]),
	printf(OutStream,'\tPI_FAIL;\n',[]).
emit_unify(TypeList,OutStream)
	:-
	calc_return_arg_num(TypeList,1,RetArg),
/*	printf(OutStream,'\tPI_makedouble(&rval,&rtype,(double) retval);\n',[]), */
	printf(OutStream,'\tmake_number(&rval,&rtype,(double) retval);\n',[]),
	printf(OutStream,'\tif (PI_unify(arg%d,type%d,rval,rtype))\n',[RetArg,RetArg]),
	printf(OutStream,'\t\tPI_SUCCEED;\n',[]),
	printf(OutStream,'\tPI_FAIL;\n',[]).


calc_return_arg_num([],RetArg,RetArg).
calc_return_arg_num([void|Rest],Count,RetArg)
	:-
	!,
	calc_return_arg_num(Rest,Count,RetArg).
calc_return_arg_num([_|Rest],Count,RetArg)
	:-
	NextCount is Count + 1,
	calc_return_arg_num(Rest,NextCount,RetArg).

%---------------------------------------------------------------------------------
%	sort_by_arity/2 takes a list of functions prototypes and returns a list
%	of groups (lists) of function prototypes... 
%	Each sublist is a list of function prototypes of the same arity.  
%	The sublists (arity groups) will NOT be in ascending or descending order 
%	by arity.
%---------------------------------------------------------------------------------

sort_by_arity(InList,OutLists)
	:-
	sortByArity(InList,[],OutLists).
	
sortByArity([],OutLists,OutLists).
sortByArity([ReturnArg=FuncArgs/File | RestFuncs], Accum, OutLists)
	:-
/*
	FuncArgs =.. [_ | Args],
	length(Args,Arity),
*/
	functor(FuncArgs,_,Arity),
	insertInAccum(Arity,ReturnArg=FuncArgs/File,Accum,NewAccum),
	!,
	sortByArity(RestFuncs,NewAccum,OutLists).
	
insertInAccum(Arity,Proto,Accum,NewAccum)
	:-
	add_to_arity_group(Arity,Accum,[],LeftGroups,TheGroup,RightGroups),
	append(TheGroup,[Proto],NewGroup),
	append(LeftGroups,[NewGroup],Temp),
	append(Temp,RightGroups,NewAccum).
	
add_to_arity_group(_,[],LeftGroups,LeftGroups,[],[])
	:- !.

add_to_arity_group(Arity,[NextGroup|RestGroups],Left,Left,NextGroup,RestGroups)
	:-
	NextGroup = [_ = Func/_ | _ ],
	Func =.. [_ | Args],
	length(Args,Arity), !.

add_to_arity_group(Arity,[NextGroup|RestGroups],Left,LeftGroups,TheGroup,RightGroups)
	:-
	add_to_arity_group(Arity,RestGroups,[NextGroup|Left],LeftGroups,TheGroup,RightGroups).


%-------------------------------------------------------------------------------------
%	sort_by_arguments/2 takes the list of lists generated by sort_by_arity
%	and further divides each sublist (I call it an 'arity group') by argument 
%	type.  Each arity group is broken down into a list of lists which
%	is of the following form:
%
%	[	[ TypeList1, ListOfFuncs1], ... , [TypeListN, ListOfFuncsN]   ]
%
%	TypeListN is a list of the argument types of each function in the list 
%	that follows; i.e., all of the functions in ListOfFuncs1 have arguments
%	whose types correspond to the types in TypeList1, and are in the order
%	of the types in TypeList1 (The return type of the function is the first
%	type listed in TypeListN, then the first argument is the second type listed
%	in TypeListN, and so on...).  
%
%	See external documentation for more information...
%-------------------------------------------------------------------------------------

sort_by_arguments(InList,OutList)
	:-
	sortByArgs(InList,[],OutList).


sortByArgs([],OutList,OutList).
sortByArgs([ArityGroup|RestGroups],Accum,OutList)
	:-
	process_arity_group(ArityGroup,SortedArityGroup),
	sortByArgs(RestGroups,[SortedArityGroup|Accum],OutList).


process_arity_group(Group,SortedGroup)
	:-
	processArityGroup(Group,[],SortedGroup).


processArityGroup([],Sorted,Sorted).
processArityGroup([Func|RestFuncs], Accum, Sorted)
	:-
	place_func_in_accum(Func,Accum,NewAccum),
	processArityGroup(RestFuncs,NewAccum,Sorted).
	

place_func_in_accum(Func,Accum,NewAccum)
	:-
	analyze_func_args(Func,TypeList),
	placeFuncInAccum(TypeList,Func,Accum,[],NewAccum).

placeFuncInAccum(TypeList,Func,[],BeenThrough,NewAccum)
	:-
	!,
	append(BeenThrough,[func_group(TypeList,[Func],_,_,_)], NewAccum).
placeFuncInAccum(TypeList,Func,[func_group(TypeList,FuncList,_,_,_) | Rest], BeenThrough, NewAccum)
	:-
	!,
	append(FuncList,[Func],NewFuncList),
	append(BeenThrough,[func_group(TypeList,NewFuncList,_,_,_)],Temp),
	append(Temp,Rest,NewAccum).
placeFuncInAccum(TypeList,Func,[One|Rest],BeenThrough,NewAccum)
	:-
	placeFuncInAccum(TypeList,Func,Rest,[One|BeenThrough],NewAccum).


analyze_func_args(_/RTL=Func/_,TypeList)
	:-
	classifyReturnType(RTL,RetType),
	Func =.. [_|Args],
	classifyFuncArgs(Args,[RetType],TypeList).

classifyFuncArgs([],TypeList,TypeList).
classifyFuncArgs([_/ATL|Rest],Accum,TypeList)
	:-
	classifyArgType(ATL,ArgType),
	append(Accum,[ArgType],NewAccum),
	classifyFuncArgs(Rest,NewAccum,TypeList).


classifyArgType([far,ptr,char],farstr)
	:- !.
classifyArgType([far,ptr,unsigned_char],farstr)
	:- !.
classifyArgType([far,array(_),char],farstr)
	:- !.
classifyArgType([far,array(_),unsigned_char],farstr)
	:- !.
classifyArgType([far,ptr|_],farptr)
	:- !.
classifyArgType([far,array(_)|_],farptr)
	:- !.
classifyArgType(TypeList,Type)
	:-
	remove_qualifiers(TypeList,NewList),
	classifyType(NewList,Type).


classifyReturnType(TypeList,integer)
	:-
	classifyType(TypeList,string),
	!.
classifyReturnType([struct|_],aggreg)
	:- !.
classifyReturnType([array(_)|_],aggreg)
	:- !.
classifyReturnType([far,ptr|_],farptr)
	:- !.
classifyReturnType([far,array(_)|_],farptr)
	:- !.
classifyReturnType(TypeList,Type)
	:-
	remove_qualifiers(TypeList,NewList),
	classifyType(NewList,Type).

%-------------------------------------------------------------------------------------
%	remove any far/near/pascal/huge qualifiers from input list.
%-------------------------------------------------------------------------------------

remove_qualifiers([], []).
remove_qualifiers([Qual|Rest], Ret)
	:-
	qualifier(Qual),
	!,
	remove_qualifiers(Rest, Ret).
remove_qualifiers([Qual|Rest], [Qual|Ret])
	:-
	remove_qualifiers(Rest, Ret).
	
qualifier(far).
qualifier(near).
qualifier(huge).
qualifier(pascal).

%---------------------------------------
% classifyType/2
%---------------------------------------

classifyType(TypeList,Type)
	:-
	classifyThisType(TypeList,Type),!.

classifyThisType([],unknown).
classifyThisType([ptr,char], string).
classifyThisType([ptr,unsigned_char],string).
classifyThisType([array(_),char],string).
classifyThisType([array(_),unsigned_char],string).
classifyThisType([ptr|_],integer).
classifyThisType([array(_)|_],integer).
classifyThisType([char|_],integer).
classifyThisType([unsigned_char|_],integer).
classifyThisType([short|_],integer).
classifyThisType([unsigned_short|_],integer).
classifyThisType([int|_],integer).
classifyThisType([unsigned_int|_],integer).
classifyThisType([long|_],integer).
classifyThisType([unsigned_long|_],integer).
classifyThisType([float|_],float).
classifyThisType([double|_],float).
classifyThisType([extended|_],float).
classifyThisType([struct|_],integer).
classifyThisType([void|_],void).
classifyThisType(List,undefined)
	:- printf(error_stream,'%% UNKNOWN TYPE LIST in classifyType/2:%t\n',[List]).

%---------------------------------------------------------
%	Generating extern declarations, and
%	Generating PI_PDEFINES:
%---------------------------------------------------------

gen_externs([],OutStream)
	:- !.
gen_externs([ArityGroup|RestGroups],OutStream)
	:-
	arityGroupExterns(ArityGroup,OutStream),
	gen_externs(RestGroups,OutStream).

arityGroupExterns([],OutStream).
arityGroupExterns([func_group(_,_,_,CDFN,_) | Rest],OutStream)
	:-
	printf(OutStream,'extern %t();\n',[CDFN]),
	arityGroupExterns(Rest,OutStream).

gen_pi_pdefines(FuncList,OutStream)
	:-
	%%gen_externs(FuncList,OutStream),
	printf(OutStream,'\n\nPI_BEGIN\n',[]),
	genPI_PDEFINES(FuncList,OutStream),
	printf(OutStream,'PI_END\n\n',[]).
	
genPI_PDEFINES([],OutStream).
genPI_PDEFINES([ArityGroup|RestGroups],OutStream)
	:-
	arityGroupPI_PDEFINES(ArityGroup,OutStream),
	genPI_PDEFINES(RestGroups,OutStream).

arityGroupPI_PDEFINES([],OutStream).
arityGroupPI_PDEFINES([func_group(_,_,DFN,CDFN,Arity) | Rest],OutStream)
	:-
	printf(OutStream,'\tPI_PDEFINE(\"%t\",%d,%t,\"_%t\")\n',[DFN,Arity,CDFN,CDFN]),
	arityGroupPI_PDEFINES(Rest,OutStream).

gen_table_init(State)
	:-
	accessP2I(outStream, State, OutStream),
	accessP2I(baseName, State, BaseName),
	accessP2I(initFcns, State, OldInitFcns),
	printf(OutStream,'void %t_compact_init(void)\n',[BaseName]),
	printf(OutStream,'{\n\tPI_INIT;\n}\n',[]),
	catenate(BaseName,'_compact_init',InitFcn),
	append(OldInitFcns, [InitFcn], NewInitFcns),
	setP2I(initFcns, State, NewInitFcns).
	

%-----------------------------------------------------
%	.pro "header" file generation...
%-----------------------------------------------------

gen_dot_pro_files(AllFuncsList,GVList,State)
	:-
	debug('Generating .pro HEADER files\n'),
	genDotProFiles(AllFuncsList,State),
	debug('Adding global variable predicates...\n'),
	accessP2I(fPrefix, State, FPrefix),
	genGVdotProFiles(GVList,FPrefix,0,State),
	close_dot_pro_files(State).

genDotProFiles([],State).
genDotProFiles([ArityGroup|RestAGroups],State)
	:-
	arity_group_DPF(ArityGroup,State),
	genDotProFiles(RestAGroups,State).

%-------------------------------------------------------------------------------
%	arity_group_DPF/1:	Place all funcs in specified Arity Group in
%						their appropriate .pro files.
%-------------------------------------------------------------------------------

arity_group_DPF([],State).
arity_group_DPF([func_group(_,FuncList,DFN,_,Arity) | RestFuncs],State)
	:-
	HeadArity is Arity - 1, 		% account for dispatch num arg.
	func_group_DPF(FuncList,DFN,0,HeadArity,State),
	arity_group_DPF(RestFuncs,State).

%-------------------------------------------------------------------------------
%	func_group_DPF/4:	Place all funcs in specified Func Group in
%						their appropriate .pro files.
%-------------------------------------------------------------------------------

func_group_DPF([],_,_,_,State)
	:- !.
func_group_DPF([_=Func/File | Rest],DispatchFuncName,DispatchNum,0,State)
	:-
	!,
	Func =.. [FuncName|_],
	translate_func_name(FuncName,IntfName,State),
	switchToProFile(File,ProFileStream,State),
	printf(ProFileStream,'export %t/0.\n',[IntfName]),
	printf(ProFileStream,'%t :-\n\t%t(',[IntfName,DispatchFuncName]),
	emit_dot_pro_args(0,DispatchNum,ProFileStream),
	NextNum is DispatchNum + 1,
	printf(ProFileStream,').\n',[]),
	func_group_DPF(Rest,DispatchFuncName,NextNum,0,State).
func_group_DPF([_=Func/File | Rest],DispatchFuncName,DispatchNum,Arity,State)
	:-
	Func =.. [FuncName|_],
	translate_func_name(FuncName,IntfName,State),
	switchToProFile(File,ProFileStream,State),
	printf(ProFileStream,'export %t/%d.\n',[IntfName,Arity]),
	printf(ProFileStream,'%t(',[IntfName]),
	emit_dot_pro_args(Arity,-1,ProFileStream),
	printf(ProFileStream,') :-\n\t%t(',[DispatchFuncName]),
	emit_dot_pro_args(Arity,DispatchNum,ProFileStream),
	NextNum is DispatchNum + 1,
	printf(ProFileStream,').\n',[]),
	func_group_DPF(Rest,DispatchFuncName,NextNum,Arity,State).

genGVdotProFiles([],_,_,State)
	:- !.
genGVdotProFiles([ als_gvar(Name,_,_)/File | Rest ], Prefix, Num,State)
	:-
	switchToProFile(File,ProFileStream,State),
	translate_func_name(Name,IntfName,State),
	printf(ProFileStream,'export %t/1.\n',[IntfName]),
	printf(ProFileStream,'%t(Value) :- \n\t%t_gv(%d,Value).\n',[IntfName,Prefix,Num]),
	NextNum is Num + 1,
	genGVdotProFiles(Rest,Prefix,NextNum,State).

%-------------------------------------------------------------------------------
%	translate_func_name/2:	Translate the name of the C function being
%							interfaced into an appropriate Prolog functor.
%							User may pass a predicate which does this,
%							or user the default, which is to make the
%							functor all lowercase letters.
%-------------------------------------------------------------------------------

translate_func_name(FuncName,TranslatedName,State)
	:-
	name(FuncName,FNL),
	accessP2I(tPred, State, UserTransName), 	% User has specified a translator
	!,
	UserCall =.. [UserTransName,FNL,TranslatedName],
	(call(UserCall) ->
		true
		;
		user:call(UserCall)
	).

translate_func_name(FuncName,TranslatedName,State)
	:-
	name(FuncName,FuncNameList),
	make_lowercase(FuncNameList,[],TranslatedName).

make_lowercase([],Accum,TranslatedName)
	:-
	!,
	reverse(Accum,TranslatedNameList),
	name(TranslatedName,TranslatedNameList).
make_lowercase([NextChar|RestChars],Accum,TranslatedName)
	:-
	make_lower(NextChar,LowerChar),
	make_lowercase(RestChars,[LowerChar|Accum],TranslatedName).

make_lower(UpperChar,LowerChar)
	:-
	UpperChar =< 0'Z,
	UpperChar >= 0'A,
	!,
	Diff is 0'a - 0'A,
	LowerChar is UpperChar + Diff.
make_lower(LowerChar,LowerChar).


%---------------------------------------------------------------------
%	switchToProFile/1:	Given the atomic name of the C header file,
%						switch standard output to the corresponding
%						.pro file being generated.
%---------------------------------------------------------------------

switchToProFile(FileName,FileStream, State)
	:-
	get_pro_filename(FileName,File,State),
	accessP2I(pfStreams, State, ProStreamStack),
	fin_switchProFile(File,ProStreamStack,FileStream, State).

fin_switchProFile(File,ProStreamStack,FileStream, State)
	:-
	dmember((File,FileStream), ProStreamStack),
	!.
fin_switchProFile(File,ProStreamStack,FileStream, State)
	:-
	open(File, write, FileStream, []),
	setP2I(pfStreams, State, [(File,FileStream) | ProStreamStack]),
	accessP2I(module, State, Module),
	printf(FileStream,'module %t.\n\n',[Module]).
/*
	(dmember((File,FileStream), ProStreamStack) ->
		true
		;
		open(File, write, FileStream, []),
		setP2I(pfStreams, State, [(File,FileStream) | ProStreamStack]),
		printf(FileStream,'module builtins.\n\n',[])
	).
*/

%---------------------------------------------------------------------
%	get_pro_filename/2:	Generate the name of the appropriate .pro file 
%		Previously: given the name of the C header file it corresponds to.
%	Now: use BaseName
%---------------------------------------------------------------------

get_pro_filename(CFile,ProFile,State)
	:-
	accessP2I(baseName, State, BaseName),
	filePlusExt(BaseName,pro,ProFile).

/*
	(filePlusExt(CFN,_,CFile) -> 
		true 
		; 
		CFN = CFile
	),
	filePlusExt(CFN,pro,ProFile).
*/

%-----------------------------------------------------------------
%	close_dot_pro_files/0:	Closes all .pro files left open.
%-----------------------------------------------------------------

close_dot_pro_files(State)
	:-
	accessP2I(pfStreams, State, PfStreams),
	accessP2I(module, State, Module),
	close_dot_pro_streams(PfStreams, Module),
	setP2I(pfStreams, State, []).

close_dot_pro_streams([],_).
close_dot_pro_streams([(File,Stream) | PfStreams],Module)
	:-
	printf(Stream, '\nendmod.     \%\%  %t\n',[Module]),
	close(Stream),
	close_dot_pro_streams(PfStreams,Module).

%-----------------------------------------------------------------
%	emit_dot_pro_args/2:	Emit generic variable names for the
%							.pro glue routines.
%-----------------------------------------------------------------

emit_dot_pro_args(Arity,Num,OutStream)
	:-
	Num < 0,
	!,
	emit_VARS(1,Arity,OutStream).
emit_dot_pro_args(Arity,Num,OutStream)
	:-
	printf(OutStream,'%d',[Num]),
	((Arity > 0) ->
		printf(OutStream,',',[])
	;	true
	),
	emit_VARS(1,Arity,OutStream).

emit_VARS(_,0,OutStream)
	:- !.
emit_VARS(Arity,Arity,OutStream)
	:-
	!,
	NextVar is 0'A + Arity - 1,
	printf(OutStream,'%c',[NextVar]).
emit_VARS(CurVar,Arity,OutStream)
	:-
	ThisVar is 0'A + CurVar - 1,
	printf(OutStream,'%c,',[ThisVar]),
	NextVar is CurVar + 1,
	emit_VARS(NextVar,Arity,OutStream).

%---------------------------------------
%	write_compact_interf_init
%---------------------------------------

write_compact_interf_init(State)
	:-
	accessP2I(baseName, State, BaseName),
	accessP2I(curONum, State, Count),
	accessP2I(outStream, State, OutStream),
	printf(OutStream, 'void %t_init(void)\n{\n',[BaseName]),
	accessP2I(initFcns, State, InitFcns),
%	accessP2I(cmpctFCnt, State, LCFNum),
%	!,
%	emit_compact_inits(Count,LCFNum,BaseName,OutStream),
	emit_compact_inits(InitFcns,OutStream),
	printf(OutStream,'\n}\n',[]).
/*
	printf(OutStream,
			'\t if (verbose) PI_app_printf("%t interface loaded\\n");\n}\n',
			[BaseName]).
*/

/*
write_compact_interf_init(FD,State).

emit_compact_inits(LCFNum,LCFNum,BaseName,OutStream)
	:-
	!,
	printf(OutStream,'\t%t_compact_init();\n',[BaseName]).
emit_compact_inits(N,LCFNum,BaseName,OutStream)
	:-
	N1 is N - 1,
	emit_compact_inits(N1,LCFNum,BaseName,OutStream),
	printf(OutStream,'\t%t%t_init();\n',[BaseName,N]).
*/
emit_compact_inits([],OutStream).
emit_compact_inits([InitFcn | InitFcns],OutStream)
	:-
	printf(OutStream,'\t%t();\n',[InitFcn]),
	emit_compact_inits(InitFcns,OutStream).

%====================================================
% GLOBAL VARIABLE INTERFACE GENERATOR
%====================================================

gen_global_variable_interf(GVList,State)
	:-
	filter_dups(GVList, gvar, [], GVList0),
	GVList0 \= [],
	!,
	length(GVList,RawNumGVars),
	length(GVList0,NumUniqueGVars),
	debug('Number of C-globals: initial=%t, final=%t\n',
		[RawNumGVars,NumUniqueGVars]),
	switchOutputFile(OutStream,State),

	accessP2I(fPrefix, State, FPrefix),
	gen_gv_cfunc_header(FPrefix,OutStream),
	gen_gv_cfunc_cases(GVList0,0,OutStream),
	gen_gv_cfunc_trailer(OutStream),
	gen_gv_cfunc_pipdefine(FPrefix,OutStream),
	gen_gv_init(FPrefix,State,OutStream).

gen_global_variable_interf(_,State).


gen_gv_cfunc_header(Prefix,OutStream)
	:-
	printf(OutStream, 'static int %t_gv_dispatch(void)\n',[Prefix]),
	printf(OutStream, '{\n',[]),
	printf(OutStream, '\tPWord dispatchVal,dispatchType;\n',[]),
	printf(OutStream, '\tPWord argVal; int argType;\n',[]),
	printf(OutStream, '\tPWord retVal; int retType;\n\n',[]),
	printf(OutStream, '\tPI_getan(&dispatchVal,&dispatchType,1);\n',[]),
	printf(OutStream, '\tPI_getan(&argVal,&argType,2);\n\n',[]),
	printf(OutStream, '\tif (dispatchType != PI_INT)  PI_FAIL;\n\n',[]),
	printf(OutStream, '\tswitch( dispatchVal )\n',[]),
	printf(OutStream, '\t{\n',[]).

gen_gv_cfunc_trailer(OutStream)
	:-
	printf(OutStream, '\t\tdefault:\n\t\t\tPI_FAIL;\n',[]),
	printf(OutStream, '\t}\n\n',[]),
	printf(OutStream, 'if (!PI_unify(argVal,argType,retVal,retType))\n',[]),
	printf(OutStream, '\t\tPI_FAIL;\n\n',[]),
	printf(OutStream, '\tPI_SUCCEED;\n}\n\n',[]).

gen_gv_cfunc_pipdefine(Prefix,OutStream)
	:-
	printf(OutStream, 'PI_BEGIN\n',[]),
	printf(OutStream, '\tPI_PDEFINE(\"%t_gv\",2,%t_gv_dispatch,\"_%t_gv_dispatch\")\n',
				[Prefix,Prefix,Prefix]),
	printf(OutStream, 'PI_END\n\n',[]).

gen_gv_init(Prefix,State,OutStream)
	:-
	accessP2I(baseName, State, BaseName),
	accessP2I(curONum, State, Count),
	accessP2I(initFcns, State, OldInitFcns),
	printf(OutStream, 'void %t%t_init(void)\n',[BaseName,Count]),
	printf(OutStream, '{\n\tPI_INIT;\n}\n\n',[]),
	catenate([BaseName,Count,'_init'],InitFcn),
	append(OldInitFcns, [InitFcn], NewInitFcns),
	setP2I(initFcns, State, NewInitFcns).

gen_gv_cfunc_cases([],_,OutStream).
gen_gv_cfunc_cases([als_gvar(Name,Cast,PType)/_ | RestGVars], Label,OutStream)
	:-
	printf(OutStream, '\t\tcase %d:\n',[Label]),
	gen_gv_case_body(PType,Cast,Name,OutStream),
	NextLabel is Label + 1,
	gen_gv_cfunc_cases(RestGVars,NextLabel,OutStream).

gen_gv_case_body([ struct | _ ],Cast,Name,OutStream)
	:-
	!,
	printf(OutStream, '\t\tPI_makedouble(&retVal,&retType,(double) (long) &%t);\n',[Name]),
	printf(OutStream, '\t\tbreak;\n',[]).
gen_gv_case_body([void |_ ],_,_,OutStream)
	:- !.  	% This should never happen.
gen_gv_case_body(_,Cast,Name,OutStream)
	:-
	printf(OutStream, '\t\tPI_makedouble(&retVal,&retType,(double) (long) %t);\n',[Name]),
	printf(OutStream, '\t\tbreak;\n',[]).

%%------------------------------------------------
%%	INTERFACE to C TYPES and C STRUCTURES
%%------------------------------------------------

gen_type_interf( [], State)
	:- !.
gen_type_interf( TList, State)
	:-
	debug('Generating ctypes interface:\n'),
	filter_dups( TList, ctype, [], NList ),
	length(TList,INum),
	length(NList,Num),
	( doswin -> 		/* als_type/3 is handled as special case */
		NList0 = [als_type(farptr,'_Far *',[far,ptr])|NList]
	;
		NList0 = NList
	),
	append([als_type(int,[int]),
			als_type(long,[long]),
			als_type(char,[char]),
			als_type(short,[short]),
			als_type(float,[float]),
			als_type(double,[double]),
			als_type(ptr,'void *',[ptr]) %,				/* als_type/3	*/
		   ], NList0, NList1),
	debug('\tNumber of ctypes: initial=%t, final=%t\n',[INum,Num]),
	gen_type_interf1( NList1, State).


gen_type_interf1( NList, State)
	:-
	switchOutputFile(OutStream,State),
	write_type_header(OutStream),
	gen_type_interf2(NList, 0, [], OutStream, State).

gen_type_interf2( [], _, SetupList, OutStream, State)
	:- !,
	write_type_footer(SetupList, OutStream, State).

gen_type_interf2( TList, Count, SetupList, OutStream, State)
	:-
	accessP2I(smax, State, Limit),
	Count == Limit,
	!,
	write_type_footer( SetupList, OutStream, State),
	gen_type_interf1( TList, State).

gen_type_interf2([Expr | TList], Count, SetupList, OutStream, State)
	:-
	disp_gen_type_interf2(Expr, TList, Count, SetupList, OutStream, State).

disp_gen_type_interf2(als_struct(Name,Fields), TList, 
					  Count, SetupList, OutStream, State)
	:- !,
	catenate(fields,Name,Name1),
	printf(OutStream, 'CI_BEGARRAY(%t)\n',[Name1]),
	gen_fields( Fields, Name, OutStream),
	printf(OutStream,'  {0}\nCI_ENDARRAY\n\n',[]),
	NCount is Count + 1,
	gen_type_interf2(TList, NCount, [ struct(Name,Name1) | SetupList], 
					 OutStream, State).

disp_gen_type_interf2(als_type(Alias,CType,PType), TList, 
					  Count, SetupList, OutStream, State)
	:-!,											%% als_type/3
	type_code(PType,TypeCode),
	NCount is Count + 1,
	gen_type_interf2(TList, NCount, [ type(Alias,CType,TypeCode) | SetupList], 
					 OutStream, State).

disp_gen_type_interf2(als_type(CType,PType), TList, 
					  Count, SetupList, OutStream, State)
	:-
	(PType = [array(0) | _] ; PType = [struct]),
	!,
	disp_gen_type_interf2(als_type(CType,'void *',PType), TList,
					  Count, SetupList, OutStream, State).

disp_gen_type_interf2(als_type(CType,PType), TList, 
					  Count, SetupList, OutStream, State)
	:-												%% als_type/2
	type_code(PType,TypeCode),
	NCount is Count + 1,
	gen_type_interf2(TList, NCount, [ type(CType,CType,TypeCode) | SetupList], 
					 OutStream, State).

gen_fields( [], _ , OutStream).

gen_fields( [ FName=TypeList | Fields ], SName , OutStream)
	:-
	remove_qualifiers(TypeList, NewList),
	dispatch_field(NewList, NewList, TypeList, FName, SName , OutStream),
	gen_fields( Fields, SName , OutStream).

dispatch_field( [bit(_) | _], _,  _, _, _, OutStream)
	:- !.

dispatch_field( [array(Size)|Rest], NoQualList, TypeList, FName, SName, OutStream)
	:- !,
	field_code_structname(TypeList, NoQualList, TypeCode, TypeStr),
	printf(OutStream,
			'  CI_ARRAYFIELD("%t",%t,%t,%t,"%t",%t',
			[FName,FName,SName,TypeCode,TypeStr,Size]),
	multi_dimensional_array(Rest, OutStream).

dispatch_field( Type, NoQualList, TypeList, FName, SName, OutStream)
	:-
	field_code_structname(TypeList, NoQualList, TypeCode, TypeStr),
	printf(OutStream,
			'  CI_FIELD("%t",%t,%t,%t,"%t"),\n',
			[FName,FName,SName,TypeCode,TypeStr]).

multi_dimensional_array([array(Size)|Rest], OutStream)
	:- !,
	printf(OutStream, '*%t', [Size]),
	multi_dimensional_array(Rest, OutStream).

multi_dimensional_array(_, OutStream)
	:-
	printf(OutStream,'),\n',[]).



type_code([far,ptr|_], 		14) :- !.
type_code([far,array(_)|_], 14) :- !.
type_code([array(_)|_],		5)  :- !.
type_code(TypeList, Code)
	:-
	remove_qualifiers(TypeList,NewList),
	type_code0(NewList, Code),
	!.
type_code(TypeList, 0).


field_code_structname([far,ptr|_], _, 14, '') :- !.
field_code_structname( _, [array(_)|Rest], Code, Name)
	:-
	!,
	field_code_structname([], Rest, Code, Name).
field_code_structname( _, TypeList, Code, '' )
	:-
	type_code0(TypeList, Code),
	!.
field_code_structname(_, [Type|_], 0, Type).


type_code0([int],			1).
type_code0([unsigned_int],	1).
type_code0([long], 			3).
type_code0([unsigned_long], 3).
type_code0([ptr|_],			5).
type_code0([char],			6).
type_code0([unsigned_char], 6).
type_code0([short],			8).
type_code0([unsigned_short],8).
type_code0([float],			12).
type_code0([double],		13).

%%-----------------------------------------------
%%	INTERFACE to C CONSTANTS
%%-----------------------------------------------

gen_constants_interf( [], State)
	:- !.
gen_constants_interf( CList, State)
	:-
	debug('Generating constants interface:\n'),
	switchOutputFile( OutStream, State),
	write_const_header(OutStream,State),
	filter_dups( CList, const, [], CList1 ),
	length(CList,ICNum),
	length(CList1,CNum),
	debug('\tNumber of constants: initial=%t final=%t\n',[ICNum,CNum]),
	gen_const_interf(CList1,0,OutStream, State),
	write_const_footer(OutStream,State).


gen_const_interf( [], Count,OutStream, State)
	:- !.
gen_const_interf( CList, Count,OutStream, State)
	:-
	accessP2I(cmax, State, Limit),
	Count == Limit,
	!,
	write_const_footer(OutStream,State),
	switchOutputFile( NewOutStream, State),
	write_const_header(NewOutStream,State),
	gen_const_interf( CList, 0, NewOutStream, State).

gen_const_interf( [ als_const(Name,Type) |CList], Count,OutStream, State)
	:-
	dispatch_const( Type, Name, OutStream),
	NCount is Count + 1,
	gen_const_interf( CList, NCount, OutStream, State).

dispatch_const( int, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_INTCONST("%t",%t)\n', [Name, Name]).
dispatch_const( short, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_SHORTCONST("%t",%t)\n', [Name, Name]).
dispatch_const( char, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_CHARCONST("%t",%t)\n', [Name, Name]).
dispatch_const( long, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_LONGCONST("%t",%t)\n', [Name, Name]).
dispatch_const( ptr, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_PTRCONST("%t",%t)\n', [Name, Name]).
dispatch_const( str, Name , OutStream)
	:- 
	!,
	printf(OutStream, '  CI_STRINGCONST("%t",%t)\n', [Name, Name]).
dispatch_const( float, Name , OutStream)
	:-
	!,
	printf(OutStream, '  CI_FLOATCONST("%t",%t)\n', [Name, Name]).
dispatch_const( double, Name , OutStream)
	:-
	!,
	printf(OutStream, '  CI_DOUBLECONST("%t",%t)\n', [Name, Name]).
dispatch_const( _, Name , OutStream).

%%-------------------------------------------------------
%%	MISCELLANEOUS ROUTINES
%%-------------------------------------------------------

%
% filter_dups provides a hook to call a user specified
% pattern generator to be used in detecting duplicates.
% The name of the pattern generator predicate is specified
% in the command line with -dPredName option (after -p flag)
% in which case PredName/3 is called with the following arguments
%
%	PredName(+Flag,+Object,-PatternReturn)
%
% where Flag is one of: func, ctype, const, gvar
% Object is the Input Object, and the predicate 
% returns the Pattern constructed in PatternReturn.
%

filter_dups( [], Type, _, []).
filter_dups( [Spec | Rest], Type, Hist, Out)
	:-
	get_pattern(Type, Spec, Pattern),
	filter_dups(Pattern, Type, Spec, Rest, Hist, Out).

filter_dups(Pattern, Type, Spec, Rest, Hist, Out)
	:-
	dmember(Pattern, Hist),
	!,
	filter_dups(Rest, Type, Hist, Out).
filter_dups(Pattern, Type, Spec, Rest, Hist, [Spec | Out])
	:-
	filter_dups(Rest, Type, [Pattern | Hist], Out).

:-dynamic('$user_dup_detect'/1).  			% user specified pattern generator

get_pattern(Type, Spec, Pattern)
	:-
	'$user_dup_detect'(DupDetectName),		% user specified pattern generator
	!,
	UserCall =.. [DupDetectName, Type, Spec, Pattern],
	user:UserCall.
get_pattern(func, R=F/_, Pattern)
	:-										% defaults
	!,
	functor(F, Pattern, _).
get_pattern(gvar, als_gvar(Name,_,_)/_, Name) :-	!.
get_pattern(ctype, als_struct(Name,_), Name) :-	!.
get_pattern(ctype, als_type(Name,_), Name) :-	!.
get_pattern(const, als_const(Name,_), Name) :-	!.


endmod.

/*=========================================================*
 |      intfout.pro
 |      Copyright (c) 1991-94 Applied Logic Systems, Inc.
 |
 | Date:  3 Oct 1991
 | Author: P. Raman
 | Modification History:
 |
 | Purpose 	: file control, and header and footer information
 *=========================================================*/

module pro2intf.

	% file suffixes

inSuffix('src').

outSuffix('c').

	
	% input file handling

openInputFile(State, InStream) :-
	accessP2I(inFile, State, InFile),
	open(InFile, read, InStream, []),
	setP2I(inStream, State, InStream),
	debug("Reading from file %t ...",[InFile]).

closeInputFile(State) 
	:-
	accessP2I(inStream, State, InStream),
	close(InStream),
	accessP2I(inFile, State, InFile),
	debug("Done reading from %t\n",[InFile]).

	% output file handling

openOutputFile(State) :-
	openOutputFile(_,State).

openOutputFile(OutStream,State) 
	:-
	accessP2I(curONum,State,Count),
	increment_Num(curONum,State,_),
	accessP2I(baseName, State, BaseName),
	outSuffix(Suffix),
	catenate([BaseName,Count],NewBase),
	filePlusExt(NewBase,Suffix,File),

	accessP2I(cFileList, State, CFileList),
	append(CFileList, [File], NewCFileList),
	setP2I(cFileList, State, NewCFileList),

	open(File,write,OutStream,[]),
	setP2I(outFile, State, File),
	setP2I(outStream, State, OutStream),
	debug("Writing to file %t\n",[File]),
	accessP2I(inFile, State, InFile),
	gen_file_header(OutStream,InFile,File,
			printf(OutStream,'\t\t--by ALS Interface Generator\n\n',[])
			),
	printf(OutStream,'#include "defs.h"\n',[]),
	printf(OutStream,'#include "cinterf.h"\n',[]),
	accessP2I(hincs,State,Hincs),
	print_incs(Hincs,OutStream),
	nl(OutStream).

print_incs([],OutStream).
print_incs([HFile | Hincs],OutStream)
	:-
	printf(OutStream,'#include "%t.h"\n\n',[HFile]),
	print_incs(Hincs,OutStream).

switchOutputFile(State)
	:-
	switchOutputFile(_,State).

switchOutputFile(OutStream,State)
	:-
	accessP2I(outStream, State, CurOutStream),
	close(CurOutStream),
	openOutputFile(OutStream,State).

closeOutputFile(State) 
	:- 
	accessP2I(outStream, State, CurOutStream),
	close(CurOutStream).
%

write_type_header(OutStream).

write_type_footer(SetupList,OutStream,State) 
	:-
	accessP2I(baseName,State,BaseName),
	accessP2I(curONum,State,Count),
	accessP2I(initFcns, State, OldInitFcns),
	printf(OutStream,'void %t%t_init(void)\n{\n',[BaseName,Count]),
	catenate([BaseName,Count,'_init'],InitFcn),
	append(OldInitFcns, [InitFcn], NewInitFcns),
	setP2I(initFcns, State, NewInitFcns),
	emit_setup(SetupList,OutStream),
	printf(OutStream,'}\n',[]).

emit_setup([],_).

emit_setup([ struct(Name,FieldArray) | SetupList ],OutStream ) 
	:- !,
	printf(OutStream,'  CI_STRUCT("%t",%t,%t)\n',[Name,Name,FieldArray]),
	emit_setup(SetupList,OutStream).

emit_setup([ type(Name,CType,TypeCode) | SetupList ],OutStream ) 
	:-
	printf(OutStream,'  CI_CTYPEDEF("%t",%t,%t)\n',[Name,CType,TypeCode]),
	emit_setup(SetupList,OutStream).

%

write_const_header(OutStream, State) :-
	accessP2I(baseName, State, BaseName),
	accessP2I(curONum, State, Count),
	accessP2I(initFcns, State, OldInitFcns),
	printf(OutStream,'void %t%t_init(void)\n{\n',[BaseName,Count]),
	catenate([BaseName,Count,'_init'],InitFcn),
	append(OldInitFcns, [InitFcn], NewInitFcns),
	setP2I(initFcns, State, NewInitFcns).


write_const_footer(OutStream, State) 
	:-
	printf(OutStream,'}\n',[]).

%

debug(Msg) :-
	debug(Msg,[]).

debug(Fmt,List) :-
	debug,
	!,
	printf(user,Fmt,List),
	flush_output(user).
debug(Fmt,List).

endmod.
