/*===================================================================*
 |		atr_inst.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |	-- 68020 abstract assembly language instruction database
 |
 |
 | Author: Kevin A. Buettner
 | Created: 4/4/90
 | Revision History:
 |	12/28/94 - C. Houpt	--	Added externdata and externcode
 |							directives to support Mac.
 |-----------------------------------------------------------------
 | Exported Procedures:
 |	instruction(Opcode,Args,OutOpcode,InTab,OutTab)
 |		-- succeeds if Opcode and Args are of appropriate type
 |		   giving the new OutOpCode.  InTab and OutTab are the
 |		   input and output database arguments.
 |	opcode(Opcode,InList,OutList)
 |		-- Parses InList leaving InList and giving Opcode as
 |		   the return value.  This is a dcg rule.
 *===================================================================*/
module instructions.
use labels.

export instruction/5.
export opcode/3.
/*
 * An opcode is considered to be an identifier possibly followed by a dot
 * followed by another identifier.
 *
 */

opcode(Opcode) --> [ident(ID)], checkdot(Opcode,ID),!.
opcode(error('Identifier expected as opcode')) --> [].

checkdot(Opcode,ID) --> [dot], !, checksuffix(Opcode,ID).
checkdot(opcode(ID,_),ID) --> [].

checksuffix(opcode(ID,Suffix),ID) -->
		[ident(Suffix)],
		{sbwldpX(Suffix),suffix_matches(ID,Suffix)},
		!.
checksuffix(error('Illegal suffix on instruction'),ID) --> [].

sbwldpX(l).
sbwldpX(w).
sbwldpX(b).
sbwldpX(s).
% For the FPCP suffixes below, we really should check that they only attach
%	to FPCP instrs.
sbwldpX(Suff) :- dpX(Suff).


dpX(d).		% '040 and '881 == double (64 bits)
dpX(p).		% '040 and '881 == packed (decimal)
dpX('X').	% '040 and '881 == extended (80 bits)

suffix_matches(Suff,Op) :-
	dpX(Suff), !, fpcp(Op).
suffix_matches(_,_).


/*
 * The instruction is valid if it can be found in the instr database below
 * with appropriate arguements and attributes.
 */

instruction(opcode(OP,Attr),Args,opcode(OP2,Attr2),InTab,OutTab) :-
    instr(OP,Attr,Args,OP2),
    newSuffix(Attr,Attr2),
    db_mod(OP,Args,InTab,OutTab),
    !.

newSuffix(V,V) :- var(V), !.
newSuffix(s,b) :- !.
newSuffix(X,X).

db_mod(global,[abs(Label)],InTab,OutTab) :-
    !,
    global_label(Label,InTab,OutTab).

db_mod(externcode, [abs(Label)], InTab, OutTab) :-
	!,
	extern_label(Label,InTab,OutTab).

db_mod(externdata, [abs(Label)], InTab, OutTab) :-
	!,
	extern_label(Label,InTab,OutTab).

db_mod(_,_,T,T).

%% Pseudo-ops
instr(text,pseudo,[],text).
instr(data,pseudo,[],data).
instr(global,pseudo,[abs(_)],global).
instr(long,pseudo,[abs(_)],long).
instr(even,pseudo,[],even).
%% ceh
instr(externdata, pseudo, [abs(_)], externdata).
instr(externcode, pseudo, [abs(_)], externcode).
instr(globaldata,pseudo,[abs(_)],globaldata).

%% Add Decimal with Extend (should be Add Binary Coded Decimal)
instr(abcd,b,[predecr(_),predecr(_)],abcd).
instr(abcd,b,[dreg(_),dreg(_)],abcd).

%% Add
instr(add,BWL,[QImm,A2],addq)		:- quick(QImm), alt(A2).
instr(add,BWL,[imm(Imm),A2],addi)	:- dalt(A2).
instr(add,BWL,[Any,dreg(_)],add)	:- any(Any).
instr(add,WL, [Any,areg(_)],adda)	:- lw(WL), any(Any).
instr(add,BWL,[dreg,A2],add)		:- altmem(A2). 

%% Add Address
instr(adda,WL,[Any,areg(_)],adda)	:- lw(WL), any(Any).

%% Add Immediate
instr(addi,BWL,[imm(Imm),A2],addi)	:- dalt(A2).

%% Add Quick
instr(addq,BWL,[QImm,A2],addq)		:- quick(QImm), alt(A2).

%% Add Extended
instr(addx,BWL,[dreg(_),dreg(_)],addx).
instr(addx,BWL,[predecr(_),predecr(_)],addx).

%% AND Logical
instr(and,b,[imm(_),reg(ccr)],andi_ccr).
instr(and,BWL,[imm(_),A2],andi)		:- dalt(A2).
instr(and,BWL,[A1,dreg(_)],and)		:- daddr(A1).
instr(and,BWL,[dreg(_),A2],and)		:- altmem(A2).

%% AND Immediate
instr(andi,BWL,[imm(_),A2],andi)	:- dalt(A2).

%% AND Immediate to CCR
instr(andi,Attr,[imm(_),reg(ccr)],andi_ccr).

%% Arithmetic Shift Left
instr(asl,BWL,[dreg(_),dreg(_)],asl).
instr(asl,BWL,[QImm,dreg(_)],asl)	:- quick(QImm).
instr(asl,w,[A1],asl)			:- altmem(A1).

%% Arithmetic Shift Right
instr(asr,BWL,[dreg(_),dreg(_)],asr).
instr(asr,BWL,[QImm,dreg(_)],asr)	:- quick(QImm).
instr(asr,w,[A1],asr)			:- altmem(A1).

%% Branch Conditionally
instr(bcc,BWL,[Lab],bcc)		:- label(Lab).
instr(bcs,BWL,[Lab],bcs)		:- label(Lab).
instr(beq,BWL,[Lab],beq)		:- label(Lab).
instr(bge,BWL,[Lab],bge)		:- label(Lab).
instr(bgt,BWL,[Lab],bgt)		:- label(Lab).
instr(bhi,BWL,[Lab],bhi)		:- label(Lab).
instr(ble,BWL,[Lab],ble)		:- label(Lab).
instr(bls,BWL,[Lab],bls)		:- label(Lab).
instr(blt,BWL,[Lab],blt)		:- label(Lab).
instr(bmi,BWL,[Lab],bmi)		:- label(Lab).
instr(bne,BWL,[Lab],bne)		:- label(Lab).
instr(bpl,BWL,[Lab],bpl)		:- label(Lab).
instr(bvc,BWL,[Lab],bvc)		:- label(Lab).
instr(bvs,BWL,[Lab],bvs)		:- label(Lab).

%% Test a Bit and Change
instr(bchg,BL,[dreg(_),dreg(_)],bchg)	:- bl(BL).
instr(bchg,b,[dreg(_),A2],bchg)		:- dalt(A2).
instr(bchg,BL,[imm(_),dreg(_)],bchg)	:- bl(BL).
instr(bchg,b,[imm(_),A2],bchg)		:- dalt(A2).

%% Test a Bit and Clear
instr(bclr,BL,[dreg(_),dreg(_)],bclr)	:- bl(BL).
instr(bclr,b,[dreg(_),A2],bclr)		:- dalt(A2).
instr(bclr,BL,[imm(_),dreg(_)],bclr)	:- bl(BL).
instr(bclr,b,[imm(_),A2],bclr)		:- dalt(A2).

/* Later
instr(bfchg,Attr,[A1,A2],bfchg).
instr(bfclr,Attr,[A1,A2],bfclr).
instr(bfexts,Attr,[A1,A2],bfexts).
instr(bfextu,Attr,[A1,A2],bfextu).
instr(bfffo,Attr,[A1,A2],bfffo).
instr(bfins,Attr,[A1,A2],bfins).
instr(bfset,Attr,[A1,A2],bfset).
instr(bftst,Attr,[A1,A2],bftst).
*/

instr(bfextu,Unsized,[bitfield(EA,Offset,Width),dreg(_)],bfextu) :-
		dalt(EA),
		bitfield_spec(Offset),
		bitfield_spec(Width).

bitfield_spec(N) :- number(N).
bitfield_spec(dreg(_)).


%% Breakpoint
instr(bkpt,unsized,[QImm],bkpt)		:- quick(QImm).

%% Branch Always
instr(bra,BWL,[Lab],bra)		:- label(Lab).

%% Test a Bit and Set
instr(bset,BL,[dreg(_),dreg(_)],bset)	:- bl(BL).
instr(bset,b,[dreg(_),A2],bset)		:- dalt(A2).
instr(bset,BL,[imm(_),dreg(_)],bset)	:- bl(BL).
instr(bset,b,[imm(_),A2],bset)		:- dalt(A2).

%% Branch to Subroutine
instr(bsr,BWL,[Lab],bsr)		:- label(Lab).

%% Test a Bit
instr(btst,BL,[dreg(_),dreg(_)],btst)	:- bl(BL).
instr(btst,b,[dreg(_),A2],btst)		:- daddr(A2).
instr(btst,BL,[imm(_),dreg(_)],btst)	:- bl(BL).
instr(btst,b,[imm(_),A2],btst)		:- daddr(A2).

%% CALL Module
instr(callm,unsized,[A1],callm)		:- conaddr(A1).

/* Later
instr(cas,Attr,[A1,A2],cas).
instr(cas2,Attr,[A1,A2],cas2).
*/

%% Check Register Against Bounds
instr(chk,WL,[A1,dreg(_)],chk)		:- lw(WL), daddr(A1).

%% Check Register Against Bounds
instr(chk2,BWL,[A1,A2],chk2) 		:- conaddr(A1), reg(A2).

%% Clear an Operand
instr(clr,BWL,[A1],clr)			:- dalt(A1).

%% Compare (and variants)
instr(cmp,BWL,[A1,dreg(_)],cmp) 	:- any(A1).
instr(cmp,WL, [A1,areg(_)],cmpa)	:- any(A1), lw(WL).
instr(cmp,BWL,[imm(_), A2],cmpi)	:- daddr(A2).
instr(cmp,BWL,[postincr(_),postincr(_)],cmpm).

%% Compare address
instr(cmpa,WL,[A1,areg(_)],cmpa)	:- any(A1), lw(WL). 

%% Compare immediate
instr(cmpi,BWL,[imm(_),A2],cmpi)	:- daddr(A2).

%% Compare Memory
instr(cmpm,BWL,[postincr(_),postincr(_)],cmpm).

%% Compare Register Against Bounds
instr(cmp2,BWL,[A1,A2],cmp2)		:- conaddr(A1), reg(A2).

%% Test Condition, Decrement, and Branch
instr(dbcc,w,[dreg(_),Lab],dbcc)	:- label(Lab).
instr(dbcs,w,[dreg(_),Lab],dbcs)	:- label(Lab).
instr(dbeq,w,[dreg(_),Lab],dbeq)	:- label(Lab).
instr(dbf,w,[dreg(_),Lab],dbf)		:- label(Lab).
instr(dbra,w,[dreg(_),Lab],dbra)	:- label(Lab).
instr(dbge,w,[dreg(_),Lab],dbge)	:- label(Lab).
instr(dbgt,w,[dreg(_),Lab],dbgt)	:- label(Lab).
instr(dbhi,w,[dreg(_),Lab],dbhi)	:- label(Lab).
instr(dble,w,[dreg(_),Lab],dble)	:- label(Lab).
instr(dbls,w,[dreg(_),Lab],dbls)	:- label(Lab).
instr(dblt,w,[dreg(_),Lab],dblt)	:- label(Lab).
instr(dbmi,w,[dreg(_),Lab],dbmi)	:- label(Lab).
instr(dbne,w,[dreg(_),Lab],dbne)	:- label(Lab).
instr(dbpl,w,[dreg(_),Lab],dbpl)	:- label(Lab).
instr(dbt,w,[dreg(_),Lab],dbt)		:- label(Lab).
instr(dbvc,w,[dreg(_),Lab],dbvc)	:- label(Lab).
instr(dbvs,w,[dreg(_),Lab],dbvs)	:- label(Lab).

%% Signed Divide
instr(divs,w,[A1,dreg(_)],divs)		:- daddr(A1).
instr(divs,l,[A1,dreg(_)],divs)		:- daddr(A1).
instr(divs,l,[A1,pair(dreg(_),dreg(_))],divs)
					:- daddr(A1).
instr(divsl,l,[A1,pair(dreg(_),dreg(_))],divsl) 
					:- daddr(A1).

%% Unsigned Divide
instr(divu,w,[A1,dreg(_)],divu)		:- daddr(A1).
instr(divu,l,[A1,dreg(_)],divu)		:- daddr(A1).
instr(divu,l,[A1,pair(dreg(_),dreg(_))],divu)
					:- daddr(A1).
instr(divul,l,[A1,pair(dreg(_),dreg(_))],divul) 
					:- daddr(A1).

%% Exclusive OR Logical (and variants)
instr(eor,BWL,[dreg(_),A2],eor)		:- dalt(A2).
instr(eor,BWL,[imm(_),A2],eori)		:- dalt(A2).
instr(eor,b,[imm(_),reg(ccr)],eori_ccr).

%% Exclusive OR Immediate
instr(eori,BWL,[imm(_),A2],eori)	:- dalt(A2).

%% Exclusive OR Immediate to Condition Code Register
instr(eori,BWL,[imm(_),reg(ccr)],eori_ccr).

%% Exchange Registers
instr(exg,l,[dreg(_),dreg(_)],exg).
instr(exg,l,[areg(_),areg(_)],exg).
instr(exg,l,[dreg(_),areg(_)],exg).

%% Sign Extend
instr(ext,WL,[dreg(_)],ext)		:- lw(WL).

%% Sign Extend Byte to Long
instr(extb,l,[dreg(_)],extb).

%% Take Illegal Instruction Trap
instr(illegal,unsized,[],illegal).

%% Jump
instr(jmp,unsized,[A1],jmp) 		:- conaddr(A1).

%% Jump to Subroutine
instr(jsr,unsized,[A1],jsr)		:- conaddr(A1).

%% Load Effective Address
instr(lea,l,[A1,areg(_)],lea)		:- conaddr(A1).

%% Link and Allocate
instr(link,WL,[areg(_),imm(_)],link)	:- wl(WL).

%% Logical Shift Left
instr(lsl,BWL,[dreg(_),dreg(_)],lsl).
instr(lsl,BWL,[QImm,dreg(_)],lsl)	:- quick(QImm).
instr(lsl,w,[A1],lsl)			:- altmem(A1).

%% Logical Shift Right
instr(lsr,BWL,[dreg(_),dreg(_)],lsr).
instr(lsr,BWL,[QImm,dreg(_)],lsr)	:- quick(QImm).
instr(lsr,w,[A1],lsr)			:- altmem(A1).

%% Move Data from Source to Destination (with variants)
instr(move,l,[MQ,dreg(_)],moveq) 	:- mquick(MQ).
instr(move,WL,[A1,areg(_)],movea)	:- lw(WL), any(A1).
instr(move,BWL,[A1,A2],move)		:- any(A1), dalt(A2).
instr(move,w,[reg(ccr),A2],move_from_ccr) :- dalt(A2).
instr(move,w,[A1,reg(ccr)],move_to_ccr)	:- any(A1).

%% Move Data from Source to Address Register
instr(movea,WL,[A1,areg(_)],movea)	:- lw(WL), any(A1).

%% Move Multiple Registers
instr(movem,WL,[imm(_),A2],movem)	:- conalt(A2), lw(WL).
instr(movem,WL,[imm(_),predecr(_)],movem) :- lw(WL).
instr(movem,WL,[A1,imm(_)],movem)	:- conaddr(A1), lw(WL).
instr(movem,WL,[postincr(_),imm(_)],movem) :- lw(WL).

%% Move Peripheral Data
instr(movep,WL,[dreg(_),indir(_,_)],movep) :- wl(WL).
instr(movep,WL,[indir(_,_),dreg(_)],movep) :- wl(WL).

%% Move Quick
instr(moveq,l,[MQ,dreg(_)],moveq)	:- mquick(MQ).

%% Signed Multiply
instr(muls,WL,[A1,dreg(_)],muls)	:- daddr(A1), wl(WL).
instr(muls,l,[A1,pair(dreg(_),dreg(_))],muls) :- daddr(A1).

%% Unsigned Multiply
instr(mulu,WL,[A1,dreg(_)],mulu)	:- daddr(A1), wl(WL).
instr(mulu,l,[A1,pair(dreg(_),dreg(_))],mulu) :- daddr(A1).

%% Negate Decimal with Extend
instr(nbcd,b,[A1],nbcd)			:- dalt(A1).

%% Negate
instr(neg,BWL,[A1],neg)			:- dalt(A1).

%% Negate with Extend
instr(negx,BWL,[A1],negx)		:- dalt(A1).

%% No Operation
instr(nop,unsized,[],nop).

%% Logical Complement
instr(not,BWL,[A1],not)			:- dalt(A1).

%% Inclusive Or Logical (with variants)
instr(or,BWL,[A1,dreg(_)],or)		:- daddr(A1).
instr(or,BWL,[dreg(_),A2],or)		:- altmem(A2).
instr(or,BWL,[imm(_),A2],ori)		:- dalt(A2).
instr(or,b,[imm(_),reg(ccr)],ori_ccr).

%% Inclusive Or Immediate
instr(ori,BWL,[imm(_),A2],ori)		:- dalt(A2).

%% Inclusive Or Immediate to CCR
instr(ori,b,[imm(_),reg(ccr)],ori_ccr).

%% Pack
instr(pack,unsized,[predecr(_),predecr(_),imm(_)],pack).
instr(pack,unsized,[dreg(_),dreg(_),imm(_)],pack).

%% Push Effective Address
instr(pea,l,[A1],pea)			:- conaddr(A1).

%% Reset External Device (Privileged Instruction)
instr(reset,unsized,[],reset).

%% Rotate Left (Without Extend)
instr(rol,BWL,[dreg(_),dreg(_)],rol).
instr(rol,BWL,[QImm,dreg(_)],rol)	:- quick(QImm).
instr(rol,w,[A1],rol)			:- altmem(A1).

%% Rotate Right (Without Extend)
instr(ror,BWL,[dreg(_),dreg(_)],ror).
instr(ror,BWL,[QImm,dreg(_)],ror)	:- quick(QImm).
instr(ror,w,[A1],ror)			:- altmem(A1).

%% Rotate Left With Extend
instr(roxl,BWL,[dreg(_),dreg(_)],roxl).
instr(roxl,BWL,[QImm,dreg(_)],roxl)	:- quick(QImm).
instr(roxl,w,[A1],roxl)			:- altmem(A1).

%% Rotate Right With Extend
instr(roxr,BWL,[dreg(_),dreg(_)],roxr).
instr(roxr,BWL,[QImm,dreg(_)],roxr)	:- quick(QImm).
instr(roxr,w,[A1],roxr)			:- altmem(A1).

%% Return and Deallocate Parameters
instr(rtd,unsized,[imm(_)],rtd).

%% Return from Module
instr(rtm,unsized,[A1],rtm)		:- reg(A1).

%% Return and Restore Condition Codes
instr(rtr,unsized,[],rtr).

%% Return from Subroutine
instr(rts,unsized,[],rts).

%% Subtract Decimal with Extend
instr(sbcd,b,[dreg(_),dreg(_)],sbcd).
instr(sbcd,b,[predecr(_),predecr(_)],sbcd).

%% Set According to Condition
instr(scc,b,[A1],scc)			:- dalt(A1).
instr(scs,b,[A1],scs)			:- dalt(A1).
instr(seq,b,[A1],seq)			:- dalt(A1).
instr(sf,b,[A1],sf)			:- dalt(A1).
instr(sge,b,[A1],sge)			:- dalt(A1).
instr(sgt,b,[A1],sgt)			:- dalt(A1).
instr(shi,b,[A1],shi)			:- dalt(A1).
instr(sle,b,[A1],sle)			:- dalt(A1).
instr(sls,b,[A1],sls)			:- dalt(A1).
instr(slt,b,[A1],slt)			:- dalt(A1).
instr(smi,b,[A1],smi)			:- dalt(A1).
instr(sne,b,[A1],sne)			:- dalt(A1).
instr(spl,b,[A1],spl)			:- dalt(A1).
instr(st,b,[A1],st)			:- dalt(A1).
instr(svc,b,[A1],svc)			:- dalt(A1).
instr(svs,b,[A1],svs)			:- dalt(A1).

%% Subtract Binary (With Variants)
instr(sub,BWL,[QImm,A2],subq)		:- quick(QImm), alt(A2).
instr(sub,BWL,[imm(Imm),A2],subi)	:- dalt(A2).
instr(sub,BWL,[Any,dreg(_)],sub)	:- any(Any).
instr(sub,WL, [Any,areg(_)],suba)	:- lw(WL), any(Any).
instr(sub,BWL,[dreg(_),A2],sub)		:- altmem(A2). 

%% Add Address
instr(suba,WL,[Any,areg(_)],suba)	:- lw(WL), any(Any).

%% Add Immediate
instr(subi,BWL,[imm(Imm),A2],subi)	:- dalt(A2).

%% Add Quick
instr(subq,BWL,[QImm,A2],subq)		:- quick(QImm), alt(A2).

%% Add Extended
instr(subx,BWL,[dreg(_),dreg(_)],subx).
instr(subx,BWL,[predecr(_),predecr(_)],subx).

%% Swap Register Halves
instr(swap,w,[dreg(_)],swap).

%% Test and Set an Operand
instr(tas,b,[A1],tas) 			:- dalt(A1).

%% Trap
instr(trap,unsized,[imm(_)],trap).

%% Trap on Condition
instr(trapcc,unsized,[],trapcc).
instr(trapcs,unsized,[],trapcs).
instr(trapeq,unsized,[],trapeq).
instr(trapf,unsized,[],trapf).
instr(trapge,unsized,[],trapge).
instr(trapgt,unsized,[],trapgt).
instr(traphi,unsized,[],traphi).
instr(traple,unsized,[],traple).
instr(trapls,unsized,[],trapls).
instr(traplt,unsized,[],traplt).
instr(trapmi,unsized,[],trapmi).
instr(trapne,unsized,[],trapne).
instr(trappl,unsized,[],trappl).
instr(trapt,unsized,[],trapt).
instr(trapvc,unsized,[],trapvc).
instr(trapvs,unsized,[],trapvs).

instr(trapcc,WL,[imm(_)],trapcc)	:- wl(WL).
instr(trapcs,WL,[imm(_)],trapcs)	:- wl(WL).
instr(trapeq,WL,[imm(_)],trapeq)	:- wl(WL).
instr(trapf, WL,[imm(_)],trapf)		:- wl(WL).
instr(trapge,WL,[imm(_)],trapge)	:- wl(WL).
instr(trapgt,WL,[imm(_)],trapgt)	:- wl(WL).
instr(traphi,WL,[imm(_)],traphi)	:- wl(WL).
instr(traple,WL,[imm(_)],traple)	:- wl(WL).
instr(trapls,WL,[imm(_)],trapls)	:- wl(WL).
instr(traplt,WL,[imm(_)],traplt)	:- wl(WL).
instr(trapmi,WL,[imm(_)],trapmi)	:- wl(WL).
instr(trapne,WL,[imm(_)],trapne)	:- wl(WL).
instr(trappl,WL,[imm(_)],trappl)	:- wl(WL).
instr(trapt, WL,[imm(_)],trapt)		:- wl(WL).
instr(trapvc,WL,[imm(_)],trapvc)	:- wl(WL).
instr(trapvs,WL,[imm(_)],trapvs)	:- wl(WL).


%% Trap on Overflow
instr(trapv,unsized,[],trapv).

%% Test an Operand
instr(tst,BWL,[A1],tst)			:- any(A1).	%% not quite right

%% Unlink
instr(unlk,unsized,[areg(_)],unlk).

%% Unpack BCD
instr(unpk,unsized,[predecr(_),predecr(_),imm(_)],unpk).
instr(unpk,unsized,[dreg(_),dreg(_),imm(_)],unpk).

export fpcp/1.
% FPCP instructions
fpcp(fmove).
fpcp(fmovem).

fpcp(fadd).
fpcp(fsub).
fpcp(fmul).
fpcp(fdiv).
fpcp(fmod).

fpcp(fabs).
fpcp(fneg).
fpcp(fsqrt).

fpcp(fint).
fpcp(fintrz).
fpcp(fgetman).

fpcp(fsin).
fpcp(fsinh).
fpcp(fcos).
fpcp(fcosh).
fpcp(ftan).
fpcp(ftanh).
fpcp(fasin).
fpcp(facos).
fpcp(fatan).
fpcp(fetox).
fpcp(ftentox).
fpcp(flogn).
fpcp(flog10).

fpcp(fcmp).
fpcp(ftst).
fpcp(fbeq).
fpcp(fblt).
fpcp(fbgt).
fpcp(fble).
fpcp(fbge).
fpcp(fbne).


% For simplicity, we don't support .p (packed) below - though it appears in some places
% e.g. dpX/2

instr(fmove,BWLSDEP,[A1,fpreg(_)],fmove)	:- any(A1).
instr(fmove,BWLSDEP,[fpreg(_),A2],fmove)	:- dalt(A2).

instr(fmovem,'X',[imm(_),A2],fmovem)		:- dalt(A2).
instr(fmovem,'X',[dreg(_),A2],fmovem)		:- dalt(A2).
instr(fmovem,'X',[A1,imm(_)],fmovem)		:- any(A1).
instr(fmovem,'X',[A1,dreg(_)],fmovem)		:- any(A1).

instr(fadd,'X',[fpreg(_),fpreg(_)],fadd).
instr(fadd,BWLSDEP,[A1,fpreg(_)],fadd)		:- any(A1).

instr(fsub,'X',[fpreg(_),fpreg(_)],fsub).
instr(fsub,BWLSDEP,[A1,fpreg(_)],fsub)		:- any(A1).

instr(fmul,'X',[fpreg(_),fpreg(_)],fmul).
instr(fmul,BWLSDEP,[A1,fpreg(_)],fmul)		:- any(A1).

instr(fdiv,'X',[fpreg(_),fpreg(_)],fdiv).
instr(fdiv,BWLSDEP,[A1,fpreg(_)],fdiv)		:- any(A1).

instr(fmod,'X',[fpreg(_),fpreg(_)],fmod).
instr(fmod,BWLSDEP,[A1,fpreg(_)],fmod)		:- any(A1).

instr(fabs,'X',[fpreg(_),fpreg(_)],fabs).
instr(fabs,'X',[fpreg(_)],fabs).
instr(fabs,BWLSDEP,[A1,fpreg(_)],fabs)	:- any(A1).

instr(fneg,'X',[fpreg(_),fpreg(_)],fneg).
instr(fneg,'X',[fpreg(_)],fneg).
instr(fneg,BWLSDEP,[A1,fpreg(_)],fneg)	:- any(A1).

instr(fsqrt,'X',[fpreg(_),fpreg(_)],fsqrt).
instr(fsqrt,'X',[fpreg(_)],fsqrt).
instr(fsqrt,BWLSDEP,[A1,fpreg(_)],fsqrt)	:- any(A1).

instr(fint,'X',[fpreg(_),fpreg(_)],fint).
instr(fint,'X',[fpreg(_)],fint).
instr(fint,BWLSDEP,[A1,fpreg(_)],fint)	:- any(A1).

instr(fintrz,'X',[fpreg(_),fpreg(_)],fintrz).
instr(fintrz,'X',[fpreg(_)],fintrz).
instr(fintrz,BWLSDEP,[A1,fpreg(_)],fintrz)	:- any(A1).

instr(fgetman,'X',[fpreg(_),fpreg(_)],fgetman).
instr(fgetman,'X',[fpreg(_)],fgetman).
instr(fgetman,BWLSDEP,[A1,fpreg(_)],fgetman)	:- any(A1).


instr(fsin,'X',[fpreg(_),fpreg(_)],fsin).
instr(fsin,'X',[fpreg(_)],fsin).
instr(fsin,BWLSDEP,[A1,fpreg(_)],fsin)	:- any(A1).

instr(fsinh,'X',[fpreg(_),fpreg(_)],fsinh).
instr(fsinh,'X',[fpreg(_)],fsinh).
instr(fsinh,BWLSDEP,[A1,fpreg(_)],fsinh)	:- any(A1).

instr(fcos,'X',[fpreg(_),fpreg(_)],fcos).
instr(fcos,'X',[fpreg(_)],fcos).
instr(fcos,BWLSDEP,[A1,fpreg(_)],fcos)	:- any(A1).

instr(fcosh,'X',[fpreg(_),fpreg(_)],fcosh).
instr(fcosh,'X',[fpreg(_)],fcosh).
instr(fcosh,BWLSDEP,[A1,fpreg(_)],fcosh)	:- any(A1).

instr(ftan,'X',[fpreg(_),fpreg(_)],ftan).
instr(ftan,'X',[fpreg(_)],ftan).
instr(ftan,BWLSDEP,[A1,fpreg(_)],ftan)	:- any(A1).

instr(ftanh,'X',[fpreg(_),fpreg(_)],ftanh).
instr(ftanh,'X',[fpreg(_)],ftanh).
instr(ftanh,BWLSDEP,[A1,fpreg(_)],ftanh)	:- any(A1).

instr(fasin,'X',[fpreg(_),fpreg(_)],fasin).
instr(fasin,'X',[fpreg(_)],fasin).
instr(fasin,BWLSDEP,[A1,fpreg(_)],fasin)	:- any(A1).

instr(facos,'X',[fpreg(_),fpreg(_)],facos).
instr(facos,'X',[fpreg(_)],facos).
instr(facos,BWLSDEP,[A1,fpreg(_)],facos)	:- any(A1).

instr(fatan,'X',[fpreg(_),fpreg(_)],fatan).
instr(fatan,'X',[fpreg(_)],fatan).
instr(fatan,BWLSDEP,[A1,fpreg(_)],fatan)	:- any(A1).

instr(fetox,'X',[fpreg(_),fpreg(_)],fetox).
instr(fetox,'X',[fpreg(_)],fetox).
instr(fetox,BWLSDEP,[A1,fpreg(_)],fetox)	:- any(A1).

instr(ftentox,'X',[fpreg(_),fpreg(_)],ftentox).
instr(ftentox,'X',[fpreg(_)],ftentox).
instr(ftentox,BWLSDEP,[A1,fpreg(_)],ftentox)	:- any(A1).

instr(flogn,'X',[fpreg(_),fpreg(_)],flogn).
instr(flogn,'X',[fpreg(_)],flogn).
instr(flogn,BWLSDEP,[A1,fpreg(_)],flogn)	:- any(A1).

instr(flog10,'X',[fpreg(_),fpreg(_)],flog10).
instr(flog10,'X',[fpreg(_)],flog10).
instr(flog10,BWLSDEP,[A1,fpreg(_)],flog10)	:- any(A1).


instr(fbeq,WL,[Lab],fbeq)			:- label(Lab).
instr(fblt,WL,[Lab],fblt)			:- label(Lab).
instr(fbgt,WL,[Lab],fbgt)			:- label(Lab).
instr(fble,WL,[Lab],fble)			:- label(Lab).
instr(fbge,WL,[Lab],fbge)			:- label(Lab).
instr(fbne,WL,[Lab],fbne)			:- label(Lab).

instr(fcmp,'X',[fpreg(_),fpreg(_)],fcmp).
instr(fcmp,BWLSDEP,[A1,fpreg(_)],fcmp) 		:- any(A1).

instr(ftst,'X',[fpreg(_)],ftst).
instr(ftst,BWLSDEP,[A1],ftst) 			:- any(A1).



%%
%% wl/1		-- matches word or long attributes with a bias towards word
%%		   (will use word as a default value if none supplied)

wl(w) :- !.
wl(l).

%%
%% lw/1		-- matches word or long attributes with a bias on long
%%

lw(l) :- !.
lw(w) :- !.

%%
%% bl/1		-- matches byte or long attributes
%%

bl(l) :- !.
bl(b) :- !.

%%
%% label/1	-- check to see if we have an addressing suitable as a label
%%

label(llab(_)) :- !.
label(abs(_)).

%%
%% alt/1	-- checks to see if the addressing mode is an alterable one
%%
alt(areg(_)) :- !.
alt(M) :- dalt(M).

%%
%% dalt/1	-- checks to see if the addressing mode is a data
%%		   alterable one
dalt(dreg(_)) :- !.
dalt(M) :- altmem(M).

%%
%% altmem/1	-- checks to see if the addressing mode is an alterable memory
%%		   one
altmem(indir(_)) :- !.
altmem(postincr(_)) :- !.
altmem(predecr(_)) :- !.
altmem(indir(_,_)) :- !.
altmem(indir(_,_,_)) :- !.
altmem(memindir(_,_,_,_,_)) :- !.
altmem(abs_w(_)) :- !.
altmem(abs(_)).

%%
%% quick/1	-- checks to see if the addressing mode is an immediate
%%		   of value appropriate to be suitable for an addq or subq
quick(imm(I)) :- integer(I), 1 =< I, I =< 8.

%%
%% daddr/1	-- checks to see if the argument has a data addressing mode
%%		   mode. (Address registers and control registers are 
%%		   excluded).
%%		

daddr(pc_indir(_)) :- !.
daddr(pc_indir(_,_)) :- !.
daddr(pc_memindir(_,_,_,_,_)) :- !.
daddr(M) :- dalt(M).

%%
%% conaddr/1	-- checks to see that the argument has a control
%%		   addressing mode
conaddr(indir(_)) :- !.
conaddr(indir(_,_)) :- !.
conaddr(indir(_,_,_)) :- !.
conaddr(memindir(_,_,_,_,_)) :- !.
conaddr(pc_indir(_)) :- !.
conaddr(pc_indir(_,_)) :- !.
conaddr(pc_memindir(_,_,_,_,_)) :- !.
conaddr(abs_w(_)) :- !.
conaddr(llab(_)) :- !.
conaddr(abs(_)).

%%
%% any/1	-- checks to see if the argument has any normal addressing
%%		   mode.  This check will exclude the special bit field
%%		   format and any control registers

any(imm(_)) :- !.
any(areg(_)) :- !.
any(M) :- daddr(M).

%%
%% reg/1	-- checks to see if the argument is a register
%%

reg(dreg(_)).
reg(areg(_)).

%%
%% conalt/1	-- checks for a control-alterable addressing mode
%%

conalt(indir(_)) :- !.
conalt(indir(_,_)) :- !.
conalt(indir(_,_,_)) :- !.
conalt(memindir(_,_,_)) :- !.
conalt(abs_w(_)) :- !.
conalt(llab(_)) :- !.
conalt(abs(_)).

%%
%% mquick/1	-- checks to see if immediate argument is suitable for moveq
%%		   instruction

mquick(imm(I)) :- integer(I), -128 =< I, I =< 127.

endmod.		%% instructions
