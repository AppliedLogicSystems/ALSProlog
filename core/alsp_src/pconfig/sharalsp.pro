/*=======================================================================*
 |		sharalsp.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Predicates to set up command lines to 'shar' to either package
 |	up a portion of the ALS Prolog source tree, or to packaged
 |	up a portion of a build directory for ALS Prolog.
 |	
 | Author:	Ken Bowen
 | Date:	August, 1994
 *=======================================================================*/
module sharalsp.

architecture_dirs([hppa,i386,m68k,m88k,/*mips,*/port,powerpc,sparc,vax]).

/*-----------------------------------------------------------------------*
 |	sharalsp/0.
 |	sharalsp
 |
 |	Sets up command line to 'shar' to create shar archives
 |	out of the ALS Prolog source directory (alsp_src at ALS)
 |	in a temporary directory, and also sets up a shell script
 |	which, when executed, causes the shar files to be mailed.
 |	
 |	This is typically invoked by the 'sharme' script which
 |	resides at the top level of the ALS Prolog source tree
 |	(alsp_src).  That script simply determines the path to use,
 |	and passes it to the call to ALS Prolog invoking sharalsp/0.
 |	The various specific command line arguments are described
 |	below.  Any other command line args are interpreted as the names 
 |	of architecture directories to include;  If none are given, only the 
 |	portable (port) directory is included;  If the args include "all", 
 |	then all architecture directories are included.  [Note that if a 
 |	new architecture directory is created, it must be included in the 
 |	definition of "all_archs"
 |
 |	The default command line to shar forces it to break up the source tree
 |	into files of maximum 500K size; this has been empirically
 |	determined to be small enough to squeeze through all mail
 |	gateways we have encountered so far. The size can be adjusted with
 |	the "-size" command line switch.
 |
 |	The name of the mailer script is of the form
 |		sYYMMDD.als
 |	It is printed following all the messages from shar.
 |
 |	Command line arguments:
 |
 |	-alsp	Path to the ALS Prolog source directory to process; 
 |		normally supplied by the sharme script;
 |	
 |	-to	Address to which the shar files should be mailed;
 |		[Default is "null"]
 |	
 |	-from	FromAddress with which to mark the shar files;
 |		[Default is "build-master@als.com"]
 |	
 |	-tgt	Directory in which to create the shar files and the
 |		mailer script; created if it does not exist {provided
 |		this program is running with appropriate permissions}
 |		[Default is ../tmp]
 |
 |	-size	Maximum size (in K bytes) to use for the shar archive files;
 |		[Default is 500]
 *-----------------------------------------------------------------------*/

export sharalsp/0.
sharalsp
	:-
	get_cmdline_vals(SwitchVals),
	to_dirs_andtmp(SwitchVals, ALSPDIR, MailTo, FWho,
			InitArchDirsList,Size, TmpDir, _),
	(exists_file(TmpDir) ->
		true
		;
		make_subdir(TmpDir,511)
	),
	setup_shar_dirs(InitArchDirsList,ALSPDIR,SharArgs), 
	printf(user,'Specified architecture dirs: %t\n',[InitArchDirsList]),

	shar_cmd(TmpDir, Size, FWho, SharArgs, Cmd),
	system(Cmd),

	setup_mailer(TmpDir, MailTo, MailerFile),
	printf(user, '\n\n--------\nMailer file is: %t\n', [MailerFile]).

setup_shar_dirs(InitArchDirsList,ALSPDIR,SharArgs)
	:-
	pathPlusFile(ALSPDIR,'*',Pattern),
	files(Pattern, Files),
	directory(Pattern,1,SubdirList0),
	list_delete(SubdirList0,'.',SDL1),
	list_delete(SDL1,'..',SubdirList),
	architecture_dirs(AllADirs),
	list_diff(SubdirList, AllADirs, NonADirs),
	append(Files,NonADirs, L1),
	(dmember(all, InitArchDirsList) ->
		UseArchDirs = AllADirs
		;
		(dmember(port, InitArchDirsList) ->
			UseArchDirs = InitArchDirsList
			;
			UseArchDirs = [port | InitArchDirsList]
		)
	),
	append(L1,UseArchDirs,SharArgs).

shar_cmd(TmpDir, Size, FWho, SharArgs, Cmd)
	:-
	sprintf(Head,'shar -L%t -o %t/als -z -s %t -n als',[Size,TmpDir,FWho]),
%	sprintf(Head,' | shar -L%t -o %t/als -z -s %t -n als',[Size,TmpDir,FWho]),
	spaced_string(SharArgs, SS),
	append(Head,SS, CmdS),
%	append("tar -cf - ", SS, CH),
%	append(CH, Head, CmdS),
	name(Cmd,  CmdS).

spaced_string([], []).
spaced_string([Item | Items], [0'  | SS0] )
	:-
	bufwrite(ICs, Item),
	append(ICs, SSTail, SS0),
	spaced_string(Items, SSTail).

to_dirs_andtmp(SwitchVals, ALSPDIR, MailTo, FWho,
		InitArchDirsList, Size, TmpDir, RemSVals)
	:-
	dmember(['-alsp',ALSPDIR], SwitchVals),
	list_delete(SwitchVals, ['-alsp',ALSPDIR], SW1),
	(dmember(['-to',MailTo], SW1) ->
		list_delete(SW1, ['-to',MailTo], SW2)
		;
		MailTo=null,
		SW2 = SW1
	),
	(dmember(['-from',FWho], SW2) ->
		list_delete(SW2, ['-from',FWho], SW3)
		;
		FWho='build-master@als.com',
		SW3 = SW2
	),
	(dmember(['-tgt',TmpDir], SW3) ->
		list_delete(SW3, ['-tgt',TmpDir], SW4)
		;
		extendPath(ALSPDIR,'../tmp',TmpDir),
		SW4 = SW3
	),
	(dmember(['-size',Size], SW4) ->
		list_delete(SW4, ['-size',Size], SW5)
		;
		Size=500,
		SW5 = SW4
	),
	bagOf(Dir, member(['-null_switch',Dir], SW5), InitArchDirsList),
	list_diff(SW5, InitArchDirsList, RemSVals).
	
	%% fixup to use figure this out, if possible:

setup_mailer(TmpDir, MailTo, MailerFile)
	:-
	pathPlusFile(TmpDir, 'als.*', Pattern),
	files(Pattern, SharFileList),
	date(YY/MM/DD),
	pre_zero(YY,YY0),
	pre_zero(MM,MM0),
	pre_zero(DD,DD0),
	catenate([s,YY0,MM0,DD0,'.als'],FN),
	pathPlusFile(TmpDir,FN,MailerFile),
	open(MailerFile,write,Out,[]),
	shar_mailer_lines(SharFileList,MailTo,Out),
	close(Out).

pre_zero(N,N)
	:-
	N > 9, !.

pre_zero(N,ST)
	:-
	catenate(0,N,ST).
	 
shar_mailer_lines([],MailTo,Out).
shar_mailer_lines([File | SharFileList],MailTo,Out)
	:-
	printf(Out, 'cat %t | mail -s "shar %t" %t\n',[File,File,MailTo]),
	shar_mailer_lines(SharFileList,MailTo,Out).


/*-----------------------------------------------------------------------*
 |	sharbuild/0
 |	sharbuild
 |
 |	Similar to sharalsp, in that it sets up a command line to
 |	shar.  However, sharbuild/0 is concerned with packaging up
 |	a build directory, in which an ALS Prolog has been created,
 |	for shipment back to ALS.  Here are the differences:
 |
 |	- It ignores any directories which appear on its command line, and
 |	uses 
 |		[bld-port, bld-natv, bld-wins, 'cfg.pro', 'config.*'] 
 |	as the arguments to shar.
 |
 |	- It, together with the invoking shell script, try to determine the
 |	who is running this program, on what machine, and in which domain;  
 |	it passes this info in the from field in the shar call.
 *-----------------------------------------------------------------------*/

export sharbuild/0.

sharbuild
	:-
	get_cmdline_vals(SwitchVals),
	to_dirs_andtmp(SwitchVals, BLDDIR, MailTo, CmdLineFWho,
					_, Size, TmpDir, RemSVals),
	(exists_file(TmpDir) ->
		true
		;
		make_subdir(TmpDir,511)
	),
	SharArgs = [bld-port, bld-natv, bld-wins, 'cfg.pro', 'config.*'],
	(dmember([-host, Host], RemSVals) ->
		true
		;
		Host = unk_host
	),
	(dmember([-domain, Domain], RemSVals) ->
		true
		;
		Domain = unk_domain
	),
	(username(User) ->
		true
		;
		User = unk_user
	),
	find_user(User,CmdLineFWho,TheUser),
	catenate([TheUser,'@',Host,'.',Domain], FWho),

	mk_info_file(TheUser,Host,Domain,CmdLineFWho),

	shar_cmd(TmpDir, Size, FWho, SharArgs, Cmd),
printf(user_output,'Executing: %t\n',[Cmd]),
	system(Cmd),

	setup_mailer(TmpDir, MailTo, MailerFile),
	printf(user, '\n\n--------\nMailer file is: %t\n', [MailerFile]).

find_user(User,_,User)
	:-
	User \= unk_user,
	!.

find_user(_,CmdLineFWho,CmdLineFWho)
	:-
	CmdLineFWho \= 'build-master@als.com',
	!.

find_user(_,_,unk_user).

mk_info_file(TheUser,Host,Domain,CmdLineFWho)
	:-
	date(Date), time(Time),
	open(bldinfo,write,OUT,[]),
	printf(OUT,'ALS build: %t %t\n',[Date,Time]),
	printf(OUT,'User=%t  Host=%t  Domain=%t  CLFW=%t\n',
		[TheUser,Host,Domain, CmdLineFWho]),
	close(OUT).

endmod.
