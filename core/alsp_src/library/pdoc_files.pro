/*==================================================================
 |		pdoc_files.pro
 |	Copyright (c) 2003-2004 Applied Logic Systems, Inc.
 |
 |   Doc Tools for maintaining  code documentation.
 |	-- setting up description of source files
 *==================================================================*/


module prologdoc.

build_files_desc(Data, General, FileDescLines)
	:-
	get_files_list(Data, InitFilesList),
	sort(InitFilesList, FilesList),

	dmember(destdir=DestDir, General),
	path_elements(DestDir, DestDirElts),
	append(DestDirElts, [ 'files-desc.html'], FilePathElts),
	path_elements(FilePath, FilePathElts),
	open(FilePath, write, OS),
	unwind_protect( write_files_desc(Data, General, FilesList, OS, FileDescLines), close(OS)).


write_files_desc(Data, General, FilesList, OS, FileDescLines)
	:-
%	get_mods_preds_list(Data, ModsPredsList),

	dmember(keywords=KeyWords, General),
	dmember(doctitle=DocTitle, General),
	dmember(datetime=DateTime, General),
	codesweep([
	'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">',
	'<!--NewPage-->',
	'<HTML>',
	'<HEAD>',
	'<!-- Generated by prologdoc () on %t -->' 			+ [ DateTime ],
	'<TITLE>',
	'Index (%t)'							+ [ DocTitle ],
	'</TITLE>',
	'<LINK REL ="stylesheet" TYPE="text/css" HREF="stylesheet.css" TITLE="Style">',
	'<SCRIPT type="text/javascript">',
	'function windowTitle()',
	'{',
	'    parent.document.title="Index (%t)";'			+ [ DocTitle ],
	'}',
	'</SCRIPT>',
	'</HEAD>',
	'<BODY BGCOLOR="white" onload="windowTitle();">',
	'<!-- ========= START OF TOP NAVBAR ======= -->',
	'<A NAME="navbar_top"><!-- --></A>',
	'<A HREF="#skip-navbar_top" title="Skip navigation links"></A>',
	'<TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0" SUMMARY="">',
	'<TR>',
	'<TD COLSPAN=3 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">',
	'<A NAME="navbar_top_firstrow"><!-- --></A>',
	'<TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3" SUMMARY="">',
	'  <TR ALIGN="center" VALIGN="top">',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="overview-summary.html"><FONT CLASS="NavBarFont1"><B>Overview</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <FONT CLASS="NavBarFont1">Module</FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <FONT CLASS="NavBarFont1">Predicate</FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#FFFFFF" CLASS="NavBarCell1"> &nbsp;<FONT CLASS="NavBarFont1"><B>Index</B></FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="index-kwic.html"><FONT CLASS="NavBarFont1"><B>KWIC Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1Rev">    <A HREF="files-desc.html"><FONT CLASS="NavBarFont1Rev"><B>Files</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="help-prologdoc.html"><FONT CLASS="NavBarFont1"><B>Help</B></FONT></A>&nbsp;</TD>',
	'  </TR>',
	'</TABLE>',
	'</TD>',
	'<TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>',
	'</EM>',
	'</TD>',
	'</TR>',
	'<TR>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'&nbsp;PREV&nbsp;',
	'&nbsp;NEXT</FONT></TD>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'  <A HREF="index.html" target="_top"><B>FRAMES</B></A>  &nbsp;',
	'&nbsp;<A HREF="index-all.html" target="_top"><B>NO FRAMES</B></A>  &nbsp;',
	'&nbsp;<SCRIPT type="text/javascript">',
	'  <!--',
	'  if(window==top) {',
	'    document.writeln(\'<A HREF="allmodules-noframe.html"><B>All Modules</B></A>\');',
	'  }',
	'  //-->',
	'</SCRIPT>',
	'<NOSCRIPT>',
	'  <A HREF="allmodules-noframe.html"><B>All Modules</B></A>',
	'</NOSCRIPT>',
	'',
	'</FONT></TD>',
	'</TR>',
	'</TABLE>',
	'<A NAME="skip-navbar_top"></A>',
	'<!-- ========= END OF TOP NAVBAR ========= -->',
	'<HR><CENTER><H1>Source Files</H1></CENTER>'
	  	], OS),
	write_file_desc_contents(FilesList, OS, FileDescLines),
	codesweep([
	'<!-- ======= START OF BOTTOM NAVBAR ====== -->',
	'<A NAME="navbar_bottom"><!-- --></A>',
	'<A HREF="#skip-navbar_bottom" title="Skip navigation links"></A>',
	'<TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0" SUMMARY="">',
	'<TR>',
	'<TD COLSPAN=3 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">',
	'<A NAME="navbar_bottom_firstrow"><!-- --></A>',
	'<TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3" SUMMARY="">',
	'  <TR ALIGN="center" VALIGN="top">',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="overview-summary.html"><FONT CLASS="NavBarFont1"><B>Overview</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <FONT CLASS="NavBarFont1">Module</FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <FONT CLASS="NavBarFont1">Predicate</FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#FFFFFF" CLASS="NavBarCell1"> &nbsp;<FONT CLASS="NavBarFont1"><B>Index</B></FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="index-kwic.html"><FONT CLASS="NavBarFont1"><B>KWIC Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1Rev">    <A HREF="files-desc.html"><FONT CLASS="NavBarFont1Rev"><B>Files</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="help-prologdoc.html"><FONT CLASS="NavBarFont1"><B>Help</B></FONT></A>&nbsp;</TD>',
	'  </TR>',
	'</TABLE>',
	'</TD>',
	'<TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>',
	'</EM>',
	'</TD>',
	'</TR>',
	'<TR>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'&nbsp;PREV&nbsp;',
	'&nbsp;NEXT</FONT></TD>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'  <A HREF="index.html" target="_top"><B>FRAMES</B></A>  &nbsp;',
	'&nbsp;<A HREF="index-all.html" target="_top"><B>NO FRAMES</B></A>  &nbsp;',
	'&nbsp;<SCRIPT type="text/javascript">',
	'  <!--',
	'  if(window==top) {',
	'    document.writeln(\'<A HREF="allmodules-noframe.html"><B>All Modules</B></A>\');',
	'  }',
	'  //-->',
	'</SCRIPT>',
	'<NOSCRIPT>',
	'  <A HREF="allmodules-noframe.html"><B>All Modules</B></A>',
	'</NOSCRIPT>',
	'',
	'</FONT></TD>',
	'</TR>',
	'</TABLE>',
	'<A NAME="skip-navbar_bottom"></A>',
	'<!-- ======== END OF BOTTOM NAVBAR ======= -->',
	'<HR>',
	'</BODY>',
	'</HTML>'
	  	], OS).


write_file_desc_contents(FilesList, OS, FileDescLines)
	:-
	write_files_summary(FilesList, OS, FileDescLines),
	write_files_details(FilesList, OS).

write_files_summary(FilesList, OS, FileDescLines)
	:-
	codesweep([
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+2">',
	'<B>Summary</B></FONT></TD>',
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">'
	  	], OS),
	files_summary(FilesList, OS, FileDescLines),
	codesweep([
	'</TR>',
	'</TABLE>',
	'',
	'<P>',
	'&nbsp;'
	  	], OS).

files_summary([], OS, []).
files_summary([FileDesc | FilesList], OS, [FileName-FileDescLine | RestDescLines])
	:-
	FileDesc = FileName - (Details, FileDocList),
	sprintf(atom(FileRef), '#_%t_', [FileName]),
	(Details = [] -> 
		FileDescLine = ''
		;
		get_file_desc_line(Details, FileDescLine)
	),

	codesweep([
	'<TR>',
	'<TD WIDTH="20\%"><B><A HREF="%t">%t</A></B></TD>'	+ ['',FileRef, FileName],
	'<TD>%t</TD></TR>'						+ [FileDescLine]
	  	], OS),

	files_summary(FilesList, OS, RestDescLines).

get_file_desc_line([First | RestDetails], FileDescLine)
	:-
	get_file_desc_line(RestDetails, First, FileDescLine).

get_file_desc_line([], First, First).
get_file_desc_line([Next | Details], First, FileDescLine)
	:-
	get_file_desc_line(Next, Details, First, FileDescLine).

get_file_desc_line('', Details, First, FileDescLine)
	:-
	fin_get_file_desc_line(Details, First, FileDescLine).

get_file_desc_line(_, RestDetails, First, FileDescLine)
	:-
	get_file_desc_line(RestDetails, First, FileDescLine).

fin_get_file_desc_line([], First, First).
fin_get_file_desc_line([FileDescLine | Details], First, FileDescLine).



write_files_details([], OS).
write_files_details([FileDesc | FilesList], OS)
	:-
	write_file_detail(FileDesc, OS),
	write_files_details(FilesList, OS).

write_file_detail(FileDesc, OS)
	:-
	FileDesc = FileName - (Detail, FileDocList),
printf(user_output, 'FILE: %t\n', [FileName]),
	codesweep([
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+1">',
	'<A NAME="_%t_"><B>%t</B></FONT></TD>'			+ [FileName,FileName],
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">',
	'<TD>'
	  	], OS),
	file_detail(Detail, OS),
	printf(OS, '\n<br>File contents:&nbsp;&nbsp;&nbsp;', []),
	file_contents(FileDocList, OS),
	codesweep([
	'</TD>',
	'</TR>',
	'</TABLE>',
	'',
	'<P>',
	'&nbsp;'
	  	], OS).

file_detail([], OS).
file_detail([Line | Detail], OS)
	:-
	printf(OS, '%t<BR>\n', [Line]),
	file_detail(Detail, OS).

file_contents([], OS).
file_contents([Doc | FileDocList], OS)
	:-
	do_doc_content(Doc, OS),
	file_contents(FileDocList, OS).

do_doc_content(Doc, OS)
	:-
	get_doc_preddesc(Doc, PredDesc),
	get_doc_module(Doc, Module),
	PredDesc = P/A,
	catenate(P, A, XPD),
	get_doc_file(Doc, FileName),
	!,
	printf(OS, '<a href="./%t/%t.html"> %t </a>&nbsp;&nbsp;&nbsp;', [Module,XPD,PredDesc]).
do_doc_content(Doc, OS)
	:-
	get_doc_preddesc(Doc, PredDesc),
	printf(OS, 'Missing: %t &nbsp;&nbsp;&nbsp;', [PredDesc]).

endmod.
