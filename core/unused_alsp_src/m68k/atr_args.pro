/*===================================================================*
 |		atr_args.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc
 |
 |	-- Parse an abstract assembly language argument (M68000)
 |
 | Author: Kevin A. Buettner
 | Created: 4/10/90
 | Revision History:
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
 |
 | M68020 Addressing Modes:
 |
 |*	Dn		-- Data Register Direct
 |*	An		-- Address Register Direct
 |*	(An)		-- Address Register Indirect
 |*	(An)+		-- Address Register Indirect with Postincrement
 |*	-(An)		-- Address Register Indirect with Predecrement
 |*	(d16,An)	-- Address Register Indirect with Displacement
 |*	d16(An)		-- 68000 syntax for above
 |*	(d8,An,Xn.SIZE*SCALE)
 |*			-- Address Register Indirect with Index (8-bit displ)
 |*	d8(An,Xn.SIZE)	-- 68000
 |	(bd,An,Xn.SIZE*SCALE)
 |			-- Address Register Indirect with Index (base displ)
 |	([bd,An],Xn.SIZE*SCALE,od)
 |			-- Memory Indirect Post-Indexed
 |	([bd,An,Xn.SIZE*SCALE],od)
 |			-- Memory Indirect Pre-Indexed
 |*	(d16,PC)	-- Program Counter Indirect with Displacement
 |*	LABEL(PC)	-- 68000 version of Program Counter Indirect with displ
 |*	(d8,PC,Xn.SIZE*SCALE)
 |*			-- PC Indirect with Index (8 bit displacement)
 |*	LABEL(PC,XN.SIZE)
 |*			-- 68000 version of PC Indirect with Index
 |	(bd,PC,XN.SIZE*SCALE)
 |			-- PC Indirect with Index (base displacement)
 |	([bd,PC],Xn.SIZE*SCALE,od)
 |			-- PC Memory Indirect Post-Indexed
 |	([bd,PC,Xn.SIZE*SCALE],od)
 |			-- PC Memory Indirect Pre-Indexed
 |*	(xxx).w		-- absolute short
 |*	(xxx).l		-- aboslute long
 |*	#xxx		-- immediate
 |
 |---------------
 | Modes Supported and Values Returned:
 |
 |	Dn		-- Data Register Direct
 |			   dreg(Dn)
 |	An		-- Address Register Direct
 |			   areg(An)
 |	(An)		-- Address Register Indirect
 |			   indir(An)
 |	(An)+		-- Address Register Indirect with Postincrement
 |			   postincr(An)
 |	-(An)		-- Address Register Indirect with Predecrement
 |			   predecr(An)
 |	d16(An)		-- Address Register Indirect with Displacement
 |			   indir(Offset,An)
 |	d8(An,Xn.SIZE)	-- Address Register Indirect with Index
 |			   indir(Offset,An,index(Xn,Size,Scale))
 |	([bd,An],Xn.SIZE*SCALE,od)
 |			-- Memory Indirect Post-Indexed
 |			   memindir(post,Bd,BaseReg,Index,Od)
 |	([bd,An,Xn.SIZE*SCALE],od)
 |			-- Memory Indirect Pre-Indexed
 |			   memindir(pre,Bd,BaseReg,Index,Od)
 |	d16(pc)		-- PC Indirect with Displacement
 |			   pc_indir(Offset)
 |	d8(pc,Xn.SIZE)	-- PC Indirect with Index
 |			   pc_indir(Offset,index(Xn,Size,Scale))
 |	([bd,PC],Xn.SIZE*SCALE,od)
 |			-- PC Memory Indirect Post-Indexed
 |			   pc_memindir(post,Bd,Index,Od)
 |	([bd,PC,Xn.SIZE*SCALE],od)
 |			-- PC Memory Indirect Pre-Indexed
 |			   pc_memindir(pre,Bd,Index,Od)
 |	xxx.w		-- absolute short
 |			   abs_w(xxx)
 |	xxx.l		-- absolute long
 |			   abs(XXX)
 |	xxx		-- expression with which base assembler may do whatever
 |			   it wishes
 |			   abs(XXX)
 |	#xxx		-- immediate
 |			   imm(XXX)
 |
 |	EA1:EA2		-- pair(EA1,EA2)
 |	
 |	EA {offset:width}
 |			-- bitfield(EA,Offset,Width)
 |
 |	Nf		-- where N is an integer; this is a forward local
 |			   label reference
 |			   flab(N)
 |	Nb		-- where N is an integer; this is a backward local
 |			   label reference
 |			   blab(N)
 |
 |	If the argument parser fails to match any of these formats,
 |	it will return error('Message').
 *===================================================================*/

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
		ea(EA),
		moreArg(Arg,EA).

moreArg(pair(EA,EA2),EA) --> 
		[colon], !,
		ea(EA2).
moreArg(bitfield(EA,Offset,Width),EA) -->
		[lcurly], 
		((ea(Offset),Offset=dreg(_)) ; ea(imm(Offset))), 
		[colon],
		((ea(Width),Width=dreg(_)) ; ea(imm(Width))), 
		[rcurly], !.
moreArg(EA,EA) -->
		[].


%% Register direct (both address and data)
ea(Reg) -->
		register(Reg), !.

register(Reg) --> [ident(RegName)], {reg(RegName,Reg)}.

reg(RegName,areg(RegName)) :- areg(RegName),!.
reg(RegName,dreg(RegName)) :- dreg(RegName),!.
reg(RegName,fpreg(RegName)) :- fpreg(RegName), !.
reg(ccr,reg(ccr)).

%
% fp0-fp7 really don't belong in <ea>; but this is just a parser and all we
% really want to do is add a fp direct mode.  Note that register types are
% enforced later in instr/4.
%

areg(a0).
areg(a1).
areg(a2).
areg(a3).
areg(a4).
areg(a5).
areg(a6).
areg(a7).

dreg(d0).
dreg(d1).
dreg(d2).
dreg(d3).
dreg(d4).
dreg(d5).
dreg(d6).
dreg(d7).


% '040 / '881  fp0 - fp7
fpreg(fp0).
fpreg(fp1).
fpreg(fp2).
fpreg(fp3).
fpreg(fp4).
fpreg(fp5).
fpreg(fp6).
fpreg(fp7).


%% Address Register Indirect with Predecrement
ea(predecr(Reg)) --> 
		[minus,lparen,ident(Reg),rparen], 
		{areg(Reg)}, !.

%% Immediate
ea(imm(Val)) --> 
		[pound], 
		!,
		imm(Val).

imm(Val) -->
		expression(Val),
		!.
imm('Illegal immediate value expression') --> [].


%% Stuff following a left paren
ea(Mode) --> [lparen], lpstuff(Mode).

%% Argument Register following a left paren
lpstuff(Mode) --> 
		[ident(AReg)], 
		{areg(AReg)}, !, 
		lps_areg(Mode,AReg).

lpstuff(indir(0,suppress_a,Index)) -->
		index(Index),
		[rparen], !.

%% Left square bracket following a left paren
lpstuff(Mode) -->
		[lbrac],
		lbrstuff(Mode).


%% Right Paren following left paren and an address register
lps_areg(Mode,AReg) --> 
		[rparen], !,
		lps_areg_rparen(Mode,AReg).

%% Comma following left paren and an address register -- must be indexed
lps_areg(indir(0,AReg,Index),AReg) --> 
		[comma],
		index(Index),
		[rparen], !.

%% Error
lps_areg(error('Error in Address Register Indirect Argument'),_) -->
		[].

lps_areg_rparen(postincr(AReg),AReg) --> 
		[plus], !.
lps_areg_rparen(indir(AReg),AReg) --> 
		[].

% What follows a left square bracket is a memory indirect mode
lbrstuff(Mode) -->
		expression(Bd), {number(Bd), !},
		lbr_bd(Mode,Bd).

lbrstuff(Mode) -->
		[ident(BaseReg)],
		{basereg(BaseReg)},
		lbr_areg(Mode,null_bd,BaseReg).

lbrstuff(Mode) -->
		index(Index),
		[rbrac], {!},
		preind_cont(Mode,null_bd,suppress_a,Index).

lbrstuff(error('Error in Memory Indirect operand'),_) -->
		[].

lbr_bd(Mode,Bd) -->
		[comma], {!},
		lbr_bdA(Mode,Bd).
lbr_bd(Mode,Bd) -->
		[rbrac], {!},
		postind_cont(Mode,Bd,suppress_a).
lbr_bd(error('Error in Memory Indirect operand, after bd'),_) -->
		[].

lbr_bdA(Mode,Bd) -->
		[ident(BaseReg)],
		{basereg(BaseReg)},
		lbr_areg(Mode,Bd,BaseReg).
lbr_bdA(Mode,Bd) -->
		index(Index),
		[rbrac], {!},
		preind_cont(Mode,Bd,suppress_a,Index).
lbr_bdA(error('Error in Memory Indirect operand, after bd'),_) -->
		[].

lbr_areg(Mode,Bd,BaseReg) -->
	[dot], {!, fail}.

lbr_areg(Mode,Bd,BaseReg) -->
	[comma],
	index(Index), 
	[rbrac], {!},
	preind_cont(Mode,Bd,BaseReg,Index).

lbr_areg(Mode,Bd,BaseReg) -->
	[rbrac], {!},
	postind_cont(Mode,Bd,BaseReg).


lbr_areg(error('Error in Memory Indirect operand, after An'),_) -->
	[].

preind_cont(Mode,Bd,BaseReg,Index) -->
	[comma],
	expression(Od),
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(pre,Bd,Index,Od)
	  ;
		Mode = memindir(pre,Bd,BaseReg,Index,Od)) }.

preind_cont(Mode,Bd,BaseReg,Index) -->
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(pre,Bd,Index,null_od)
	  ;
		Mode = memindir(pre,Bd,BaseReg,Index,null_od)) }.

preind_cont(error('Error in Memory Indirect Pre-Indexed operand, after indirect'),_,_,_) -->
	[].

postind_cont(Mode,Bd,BaseReg) -->
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(post,Bd,suppress_x,null_od)
	  ;
		Mode = memindir(post,Bd,BaseReg,suppress_x,null_od)) }.

	
postind_cont(Mode,Bd,BaseReg) -->
	[comma],
	index(Index), {!},
	postind_cont2(Mode,Bd,BaseReg,Index).

postind_cont(Mode,Bd,BaseReg) -->
	[comma],
	expression(Od),
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(post,Bd,suppress_x,Od)
	  ;
		Mode = memindir(post,Bd,BaseReg,suppress_x,Od)) }.

postind_cont(error('Error in Memory Indirect Post-Indexed operand, after indirect'),_,_) -->
	[].

postind_cont2(Mode,Bd,BaseReg,Index) -->
	[rbrac],
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(post,Bd,Index,null_od)
	  ;
		Mode = memindir(post,Bd,BaseReg,Index,null_od)) }.

postind_cont2(Mode,Bd,BaseReg,Index) -->
	[comma],
	expression(Od),
	[rparen],
	{ (isPC(BaseReg) -> 
		Mode = pc_memindir(post,Bd,Index,Od)
	   ;
		Mode = memindir(post,Bd,BaseReg,Index,Od)) }.

postind_cont2(error('Error in Memory Indirect Post-Indexed operand, after index'),_,_,_) -->
	[].


basereg(Reg) :- areg(Reg) ; isPC(Reg).

%% index in the indexed modes -- This is the full 020 format.
index(index(Reg,Size,Scale)) --> 
		[ident(Reg)],
		{reg(Reg,_)},
		isize(Size),
		iscale(Scale).

isize(Size) --> [dot], !, sizetok(Size).
isize(w) -->	[].

sizetok(Size) -->
		[ident(STok)],
		{isSize(STok,Size)},
		!.

:- functor(_,'W',0), functor(_,'L',0), functor(_,'PC',0).

isSize('W',w).
isSize(w,w).
isSize('L',l).
isSize(l,l).

iscale(Scale) -->
		[star], !,
		[number(Scale)],
		{isLegalScale(Scale)},
		!.
iscale(1) -->	[].

isLegalScale(1).
isLegalScale(2).
isLegalScale(4).
isLegalScale(8).

%% Expression followed by whatever
ea(Mode) -->
		expression(E),
		whatever(Mode,E).

%% dot follows expression.  Look for size designator.
whatever(Mode,Exp) --> 
		[dot],
		wabs(Mode,Exp),
		!.

wabs(abs_w(Exp),Exp) -->
		[ident(W)],
		{isW(W)},
		!.
wabs(abs(Exp),Exp) -->
		[ident(L)],
		{isL(L)},
		!.
wabs(error('Illegal size designator on absolute address'),_) -->
		[].

%% Expression is a displacement for an indirect addressing mode
whatever(Mode,Exp) -->
		[lparen],
		!,
		wind(Mode,Exp).

%% Expression is a displacement for an address register indirect mode
wind(Mode,Exp) -->
		[ ident(AReg) ],
		{ areg(AReg) },
		!,
		waind(Mode,Exp,AReg).

%% We have Exp(AReg)
waind(Mode,Exp,AReg) -->
		[ rparen ],
		{ waind1(Exp,AReg,Mode) },
		!.

%% optimize for Exp=0
waind1(0,AReg,indir(AReg)) :- !.
waind1(Exp,AReg,indir(Exp,AReg)).

%% We have an address register indexed mode
waind(indir(Exp,AReg,Index),Exp,AReg) -->
		[ comma ],
		index(Index),
		[rparen],
		!.
%% We have an error
waind(error('Illegal token in Address Register Indirect argument'), _,_) --> 
		[].

%% Expression is displacment for a Program Counter Indirect Mode
wind(Mode,Exp) --> 
		[ ident(PC) ],
		{ isPC(PC) },
		!,
		wpcind(Mode,Exp).

%% Simple Program Counter Indirect
wpcind(pc_indir(Exp), Exp) -->
		[ rparen ],
		!.

%% PC Indirect with Index
wpcind(pc_indir(Exp,Index),Exp) -->
		[ comma ],
		index(Index),
		[ rparen],
		!.

%% Error
wpcind(error('Illegal token in PC-Indirect argument'),_) -->
		[].

%% Expression stands on its own as an absolute address
whatever(abs(Exp),Exp) --> [].

isW(w).
isW('W').

isL(l).
isL('L').

isPC(pc).
isPC('PC').

ea(error('Unrecognized Argument')) --> [].

endmod.
