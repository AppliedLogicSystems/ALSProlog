/*=====================================================================
 | 			headers.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Utilities for writing file headers
 *====================================================================*/
module builtins.

export gen_file_header/3.
export gen_file_header/4.

/*!-------------------------------------------------------------
 |	gen_file_header/3
 |	gen_file_header(OutStream,SourceFile,TargetFile)
 |	gen_file_header(+,+,+)
 |
 |	- output a header suitable for a generated file
 |
 *!------------------------------------------------------------*/
gen_file_header(OutStrm, SrcFile, TgtFile)
	:-
	gen_file_header(OutStrm, SrcFile, TgtFile, true).

/*!-------------------------------------------------------------
 |	gen_file_header/4
 |	gen_file_header(OutStream,SourceFile,TargetFile,ExtraCall)
 |	gen_file_header(+,+,+,+)
 |
 |	- output a decorated header suitable for a generated file
 |
 |	OutStream is a write stream, normally to file TargetFile; 
 |	gen_file_header/3 outputs a header of the following format
 |	on OutStream:
 |
 |	/*================================================================
 |	          fooout
 |	          --Generated from: fooin
 |	          Date: 94/4/17   Time: 9:52:53
 |	 *===============================================================*/
 |
 |	In gen_file_header/4, the argument ExtraCall is called just before
 |	the printing of the lower comment line.  Thus, if ExtraCall were
 |
 |			printf(user_output,'          -- by zipper_foo\n',[]),
 |
 |	the output would look like:
 |	
 |	/*================================================================
 |	          fooout
 |	          --Generated from: fooin
 |	          Date: 94/4/17   Time: 9:52:53
 |	          -- by zipper_foo
 |	 *===============================================================*/
 |	
 *!------------------------------------------------------------*/

gen_file_header(OutStrm, SrcFile, TgtFile, Extra)
	:-
	comment_start(CS),
	comment_end(CE),

	printf(OutStrm,'%t\n',[CS]),
	printf(OutStrm,'          %t\n',[TgtFile]),
	printf(OutStrm,'          --Generated from: %t\n',[SrcFile]),
	date(Date), time(Time),
	printf(OutStrm,'          Date: %t   Time: %t\n',[Date,Time]),
	(nonvar(Extra) ->
		(call(Extra) -> true ; true)
		; true ),
	printf(OutStrm,'%t\n\n',[CE]).


comment_start(
	'/*================================================================ '
		).

comment_end(
	' *===============================================================*/'
		).

endmod.
