/*=======================================================================*
 |		atr_args.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc
 |
 |	-- Parse an abstract assembly language argument (VAX)
 |
 | Author: Kevin A. Buettner
 | Created: 4/10/90	(68k version)
 | Revision History:
 |	Revised: 6/30/90, kev	-- VAX version
 |
 | Module Name:	args
 | Uses Modules:
 |	expressions	(aexp.pro)
 |
 | Exported Procedures:
 |	aarg(Arg,InList,OutList)
 |		-- get Arg from InList with remaining
 |		   tail OutList.
 |
 | VAX Addressing Modes (from Appendix F of VAX Architecture Handbook):
 |	R		-- register
 |			== reg(R)
 |	(R)		-- register deferred
 |			== reg_deferred(R)
 |	(R)+		-- autoincrement
 |			== autoincrement(R)
 |	-(R)		-- autodecrement
 |			== autodecrement(R)
 |	D(R)		-- byte, word, or longword displacement register
 |			   deferred
 |			== disp(D,R,_)
 |	B^D(R)		-- byte displacement
 |			== disp(D,R,byte)
 |	W^D(R)		-- word displacement
 |			== disp(D,R,word)
 |	L^D(R)		-- longword displacement
 |			== disp(D,R,long)
 |	G		-- byte, word, or longword displacement off PC
 |			== pcdisp(G,_)
 |	B^G		-- byte displacement off PC
 |			== pcdisp(G,byte)
 |	W^G		-- word displacement off PC
 |			== pcdisp(G,word)
 |	L^G		-- longword displacement off PC
 |			== pcdisp(G,long)
 |	G^G		-- general addressing (absolute or relative PC)
 |			== general(G)
 |	#cons		-- autoincrement of PC (immediate or literal)
 |			== imm(Cons,_)
 |	S^#cons		-- short literal
 |			== imm(Cons,short)
 |	I^#cons		-- immediate
 |			== imm(Cons,imm)
 |	(R)[Rx]		-- register deferred indexed
 |	(R)+[Rx]	-- autoincrement indexed
 |	-(R)[Rx]	-- autodecrement indexed
 |	D(R)[Rx]	-- byte, word, or lonword displaceent indexed
 |	B^D(R)[Rx]	-- byte displacement indexed
 |	W^D(R)[Rx]	-- word displacement indexed
 |	L^D(R)[Rx]	-- longword displacement indexed
 |	G[Rx]		-- byte, word, or longword displacement off PC
 |			   indexed
 |	B^G[Rx]		-- byte displacement off PC indexed
 |	W^G[Rx]		-- word displacement off PC indexed
 |	L^G[Rx]		-- longword displacement off PC indexed
 |	G^location[Rx]	-- general (absolute or PC-relative) indexed
 |	@(R)[Rx]	-- byte displacement deferred indexed with 0
 |			   displacement
 |	@(R)+[Rx]	-- autoincrement deferred indexed
 |	@#location[Rx]	-- absolute indexed
 |	@D(R)[Rx]	-- byte, word, or longword displacement deferred
 |			   indexed
 |	@B^D(R)[Rx]	-- byte displacement deferred indexed
 |	@W^D(R)[Rx]	-- word displacement deferred indexed
 |	@L^D(R)[Rx]	-- longword displacement deferred indexed
 |	@G[Rx]		-- byte, word, or longword displacement off PC
 |			   deferred indexed
 |	@B^G[Rx]	-- byte displacement off PC deferred indexed
 |	@W^G[Rx]	-- word displacement off PC deferred indexed
 |	@L^G[Rx]	-- longword displacement off PC deferred indexed
 |	@(R)		-- byte displacement deferred with 0 displacement
 |			== deferred(disp(0,R,byte))
 |	@(R)+		-- autoincrement deferred
 |			== deferred(autoincrement(R))
 |	@#location	-- absolute
 |			== absolute(Location)
 |	@D(R)		-- byte, word, longword displacement deferred
 |			== deferred(disp(D,R,_))
 |	@B^D(R)		-- byte displacement deferred
 |			== deferred(disp(D,R,byte))
 |	@W^D(R)		-- word displacement deferred
 |			== deferred(disp(D,R,word))
 |	@L^D(R)		-- longword displacement deferred
 |			== deferred(disp(D,R,long))
 |	@G		-- byte, word, or longword displacement off PC deferred
 |			== deferred(pcdisp(G,_))
 |	@B^G		-- byte displacement off PC deferred
 |			== deferred(pcdisp(G,byte))
 |	@W^G		-- word displacement off PC deferred
 |			== deferred(pcdisp(G,word))
 |	@L^G		-- longword displacement off PC deferred
 |			== deferred(pcdisp(G,long))
 |	^M<rx,rx..>	== regMask(RegList)
 |
 |	Indexed modes will be represented as follows
 |		index(B,Rx)
 |	where B is the base mode and Rx is the index register.
 |	
 *=======================================================================*/

module args.
    use	expressions.
    export aarg/3.

aarg(Label) -->
		[number(Num),ident(BorF)],
		{bORf(BorF,Num,Label)},
		!.

bORf(b,Num,blab(Num)).
bORf(f,Num,flab(Num)).

aarg(Arg) -->
		ea1(EA),
		index(EA,Arg).

index(EA,index(EA,Rx)) -->
		[lbrac],
		register(Rx),
		!,
		[rbrac].

index(EA,EA) -->
		[].



ea1(EA) -->	[at],
		ea(BaseEA),
		{isdeferrable(BaseEA,EA)},
		!.
ea1(EA) -->	ea(EA).

isdeferrable(EA,deferred(EA)) :- isdeferrable(EA), !.
isdeferrable(imm(Addr,imm),abs(Addr)) :- !.
isdeferrable(reg_deferred(R),deferred(disp(0,R,byte))) :- !.
isdeferrable(_,error('Illegal deferred mode')).

isdeferrable(disp(_,_,_)).
isdeferrable(pcdisp(_,_)).
isdeferrable(autoincrement(_)).

%% Register direct
ea(reg(Reg)) -->
		register(Reg), !.

register(Reg) --> [ident(Reg)], {reg(Reg)}.

reg(r0).
reg(r1).
reg(r2).
reg(r3).
reg(r4).
reg(r5).
reg(r6).
reg(r7).
reg(r8).
reg(r9).
reg(r10).
reg(r11).
reg(r12).
reg(r13).
reg(r14).


%% Autodecrement
ea(autodecrement(Reg)) --> 
		[minus,lparen,ident(Reg),rparen], 
		{reg(Reg)}, !.

%% Immediate
ea(Imm) -->	immSize(Size),
		[pound],
		!,
		imm(Imm,Size).

imm(imm(Val,Size),Size) -->
		expression(Val),
		!.
imm('Illegal immediate value expression',_) --> [].


immSize(Size) -->
		[ident(SorI), carat],
		{immSize1(SorI,Size)},
		!.
immSize(_) -->	[].

immSize1('S',short).
immSize1('I',imm).


%% Stuff following a left paren
ea(Mode) --> [lparen], lpstuff(Mode).

%% Arugment Register following a left paren
lpstuff(Mode) --> 
		[ident(Reg)], 
		{reg(Reg)}, !, 
		lps_reg(Mode,Reg).

%% Right Paren following left paren and a register
lps_reg(Mode,Reg) --> 
		[rparen], !,
		lps_reg_rparen(Mode,Reg).


%% Error
lps_reg(error('Error in Register Deferred Argument'),_) -->
		[].

lps_reg_rparen(autoincrement(Reg),Reg) --> 
		[plus], !.
lps_reg_rparen(reg_deferred(Reg),Reg) --> 
		[].

%% Optional Size followed by an Expression followed by whatever
ea(Mode) -->
		dispSize(Size),
		expression(E),
		whatever(Mode,E,Size),
		!.

dispSize(Size) -->
		[ident(S),carat],
		{translateSize(S,Size)},
		!.
dispSize(_) --> [].

translateSize('B',byte).
translateSize('W',word).
translateSize('L',long).

%% Expression is a displacement for an indirect addressing mode
whatever(Mode,Exp,Size) -->
		[lparen,ident(Reg),rparen],
		{ reg(Reg), wind(Exp,Reg,Size,Mode) },
		!.

wind(0,Reg,Size,reg_deferred(Reg)).
wind(Exp,Reg,Size,disp(Exp,Reg,Size)).

%% Expression stands on its own as a relative address
whatever(pcdisp(Exp,Size),Exp,Size) --> [], !.

%% General addressing mode
ea(general(G)) -->
		[ident('G'),carat],
		expression(G),
		!.

%% register masks

ea(regmask([Reg|Rest])) -->
		[carat,ident('M'),langle],
		register(Reg),
		regList(Rest),
		!.

regList([]) --> [rangle].
regList([Reg|Rest]) --> 
		[comma], 
		register(Reg), 
		regList(Rest).

%

ea(error('Unrecognized Argument')) --> [].

endmod.
