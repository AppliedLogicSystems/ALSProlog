/*========================================================================*
 | atr_inst.pro		-- VAX instructions
 |	Copyright (c) 1990 Applied Logic Systems, Inc.
 |
 | Author: Kevin A. Buettner
 | Creation: 6/29/90
 | Revision History:
 |
 | Module Name:	instructions
 | Exported Procedures:
 |		instruction(Opcode,Args,OutOpcode,InTab,OutTab)
 |			-- succeeds if Opcode and Args are of appropriate type
 |			   giving the new OutOpCode.  InTab and OutTab are the
 |			   input and output database arguments
 |		opcode(Opcode,InList,OutList)
 |			-- Parses InList leaving OutList and giving Opcode as
 |			   the return value.  This is a dcg rule.
 |
 *========================================================================*/

module instructions.

use labels.

export instruction/5.
export opcode/3.

/*
 * An opcode is considered to be an identifier.
 */

opcode(Opcode) --> [ident(Opcode)].

/*
 * An instruction is valid if it can be found in the instr database below
 * with the appropriate arguments.
 */

instruction(InOp,Args,OutOp,InTab,OutTab) :-
    instr(InOp,Args,OutOp),
    db_mod(InOp,Args,InTab,OutTab),
    !.

db_mod(global,[pcdisp(Label,_)],InTab,OutTab) :-
    !,
    global_label(Label,InTab,OutTab).
db_mod(_,_,T,T).


%% Pseudo-ops

instr(text,[],text).
instr(data,[],data).
instr(global,[pcdisp(_,_)],global).
instr(long,_,long).
instr(entry,_,entry).		%% VMS .entry directive
instr(extern,[pcdisp(_,_)],extern).

instr(halt, [], halt).
instr(nop,  [], nop).
instr(rei, [], rei).
instr(bpt, [], bpt).
instr(ret, [], ret).
instr(rsb, [], rsb).
instr(ldpctx, UNKNOWN ).
instr(svpctx, UNKNOWN ).
instr(cvtps, [RW1,AB2,RW3,AB4], cvtps).
instr(cvtsp, [RW1,AB2,RW3,AB4], cvtsp).
instr(index, [RL1,RL2,RL3,RL4,RL5,WL6], index).
instr(crc, [AB1,RL2,RW3,AB4], crc).
instr(prober, [], prober ).
instr(probew, [], probew ).
instr(insque, [AB1,AB2], insque ).
instr(remque, [AB1,WL2], remque ).
instr(bsbb, [BB1], bsbb).
instr(brb, [BB1], brb).
instr(bneq, [BB1], bneq).
instr(beql, [BB1], beql).
instr(bgtr, [BB1], bgtr).
instr(bleq, [BB1], bleq).
instr(jsb, [AB1], jsb).
instr(jmp, [AB1], jmp).
instr(bgeq, [BB1], bgeq).
instr(blss, [BB1], blss).
instr(bgtru, [BB1], bgtru).
instr(blequ, [BB1], blequ).
instr(bvc, [BB1], bvc).
instr(bvs, [BB1], bvs).
instr(bgequ, [BB1], bgequ).
instr(bcc, [BB1], bcc).
instr(blssu, [BB1], blssu).
instr(bcs,[BB1],bcs).
instr(addp4, [RW1,AB2,RW3,AB4], addp4).
instr(addp6, [RW1,AB2,RW3,AB4,RW5,AB6], addp6).
instr(subp4, [RW1,AB2,RW3,AB4], subp4).
instr(subp6, [RW1,AB2,RW3,AB4,RW5,AB6], subp6).
instr(cvtpt, [RW1,AB2,AB3,RW4,AB5], cvtpt).
instr(mulp, [RW1,AB2,RW3,AB4,RW5,AB6], mulp).
instr(cvttp, [RW1,AB2,AB3,RW4,AB5], cvttp).
instr(divp, [RW1,AB2,RW3,AB4,RW5,AB6], divp).
instr(movc3, [RW1,AB2,AB3], movc3).
instr(cmpc3, [RW1,AB2,AB3], cmpc3).
instr(scanc, [RW1,AB2,AB3,RB4], scanc).
instr(spanc, [RW1,AB2,AB3,RB4], spanc).
instr(movc5, [RW1,AB2,RB3,RW4,AB5], movc5).
instr(cmpc5, [RW1,AB2,RB3,RW4,AB5], cmpc5).
instr(movtc, [RW1,AB2,RB3,AB4,RW5,AB6], movtc).
instr(movtuc, [RW1,AB2,RB3,AB4,RW5,AB6], movtuc).
instr(bsbw, [BW1], bsbw).
instr(brw, [BW1], brw).
instr(cvtwl, [RW1,WL2], cvtwl).
instr(cvtwb, [RW1,WB2], cvtwb).
instr(movp, [RW1,AB2,AB3], movp).
instr(cmpp3, [RW1,AB2,AB3], cmpp3).
instr(cvtpl, [RW1,AB2,WL3], cvtpl).
instr(cmpp4, [RW1,AB2,RW3,AB4], cmpp4).
instr(editpc, [RW1,AB2,AB3,AB4], editpc).
instr(matchc, [RW1,AB2,RW3,AB4], matchc).
instr(locc, [RB1,RW2,AB3], locc).
instr(skpc, [RB1,RW2,AB3], skpc).
instr(movzwl, [RW1,WL2], movzwl).
instr(acbw, [RW1,RW2,MW3,BW4], acbw).
instr(movaw, [AW1,WL2], movaw).
instr(pushaw, [AW1], pushaw).
instr(addf2, [RF1,MF2], addf2).
instr(addf3, [RF1,RF2,WF3], addf3).
instr(subf2, [RF1,MF2], subf2).
instr(subf3, [RF1,RF2,WF3], subf3).
instr(mullf2, [RF1,MF2], mullf2).
instr(mullf3, [RF1,RF2,WF3], mullf3).
instr(divf2, [RF1,MF2], divf2).
instr(divf3, [RF1,RF2,WF3], divf3).
instr(cvtfb, [RF1,WB2], cvtfb).
instr(cvtfw, [RF1,WW2], cvtfw).
instr(cvtfl, [RF1,WL2], cvtfl).
instr(cvtrfl,  [RF1,WL2], cvtrfl).
instr(cvtbf, [RB1,WF2], cvtbf).
instr(cvtwf, [RW1,WF2], cvtwf).
instr(cvtlf, [RL1,WF2], cvtlf).
instr(acbf, [RF1,RF2,MF3,BW4], acbf).
instr(movf, [RF1,WF2], movf).
instr(cmpf, [RF1,RF2], cmpf).
instr(mnegf, [RF1,WF2], mnegf).
instr(tstf, [RF1], tstf).
instr(emodf, [RF1,RB2,RF3,WL4,WF5], emodf).
instr(polyf, [RF1,RW2,AB3], polyf).
instr(cvtfd, [RF1,WD2], cvtfd).
instr(adawi, [RW1,MW2], adawi).
instr(insqhi, [AB1,AQ2], insqhi).
instr(insqti, [AB1,AQ2], insqti).
instr(remqhi, [AQ1,WL2], remqhi).
instr(remqti, [AQ1,WL2], remqti).
instr(addd2, [RD1,MD2], addd2).
instr(addd3, [RD1,RD2,WD3], addd3).
instr(subd2, [RD1,MD2], subd2).
instr(subd3, [RD1,RD2,WD3], subd3).
instr(muld2, [RD1,MD2], muld2).
instr(muld3, [RD1,RD2,WD3], muld3).
instr(divd2, [RD1,MD2], divd2).
instr(divd3, [RD1,RD2,WD3], divd3).
instr(cvtdb, [RD1,WB2], cvtdb).
instr(cvtdw, [RD1,WW2], cvtdw).
instr(cvtdl, [RD1,WL2], cvtdl).
instr(cvtrdl, [RD1,WL2], cvtrdl).
instr(cvtbd, [RB1,WD2], cvtbd).
instr(cvtwd, [RW1,WD2], cvtwd).
instr(cvtld, [RL1,WD2], cvtld).
instr(acbd, [RD1,RD2,MD3,BW4], acbd).
instr(movd, [RD1,WD2], movd).
instr(cmpd, [RD1,RD2], cmpd).
instr(mnegd, [RD1,WD2], mnegd).
instr(tstd, [RD1], tstd).
instr(emodd, [RD1,RB2,RD3,WL4,WD5], emodd).
instr(polyd, [RD1,RW2,AB3], polyd).
instr(cvtdf, [RD1,WF2], cvtdf).
instr(ashl, [RB1,RL2,WL3], ashl).
instr(ashq, [RB1,RQ2,WQ3], ashq).
instr(emul, [RL1,RL2,RL3,WQ4], emul).
instr(ediv, [RL1,RQ2,WL3,WL4], ediv).
instr(clrq, [WQ1], clrq).
instr(movq, [RQ1,WQ2], movq).
instr(movaq, [AQ1,WL2], movaq).
instr(pushaq, [AQ1], pushaq).
instr(addb2, [RB1,MB2], addb2).
instr(addb, [RB1,MB2], addb2).
instr(addb3, [RB1,RB2,WB3], addb3).
instr(addb, [RB1,RB2,WB3], addb3).
instr(subb2, [RB1,MB2], subb2).
instr(subb, [RB1,MB2], subb2).
instr(subb3, [RB1,RB2,WB3], subb3).
instr(subb, [RB1,RB2,WB3], subb3).
instr(mulb2, [RB1,MB2], mulb2).
instr(mulb, [RB1,MB2], mulb2).
instr(mulb3, [RB1,RB2,WB3], mulb3).
instr(mulb, [RB1,RB2,WB3], mulb3).
instr(divb2, [RB1,MB2], divb2).
instr(divb, [RB1,MB2], divb2).
instr(divb3, [RB1,RB2,WB3], divb3).
instr(divb, [RB1,RB2,WB3], divb3).
instr(bisb2, [RB1,MB2], bisb2).
instr(bisb, [RB1,MB2], bisb2).
instr(bisb3, [RB1,RB2,WB3], bisb3).
instr(bisb, [RB1,RB2,WB3], bisb3).
instr(bicb2, [RB1,MB2], bicb2).
instr(bicb, [RB1,MB2], bicb2).
instr(bicb3, [RB1,RB2,WB3], bicb3).
instr(bicb, [RB1,RB2,WB3], bicb3).
instr(xorb2, [RB1,MB2], xorb2).
instr(xorb, [RB1,MB2], xorb2).
instr(xorb3, [RB1,RB2,WB3], xorb3).
instr(xorb, [RB1,RB2,WB3], xorb3).
instr(mnegb, [RB1,WB2], mnegb).
instr(caseb, [RB1,RB2,RB3|_], caseb).
instr(movb, [RB1,WB2], movb).
instr(cmpb, [RB1,RB2], cmpb).
instr(mcomb, [RB1,WB2], mcomb).
instr(bitb, [RB1,RB2], bitb).
instr(clrb, [WB1], clrb).
instr(tstb, [RB1], tstb).
instr(incb, [MB1], incb).
instr(decb, [MB1], decb).
instr(cvtbl, [RB1,WL2], cvtbl).
instr(cvtbw, [RB1,WW2], cvtbw).
instr(movzbl, [RB1,WL2], movzbl).
instr(movzbw, [RB1,WW2], movzbw).
instr(rotl, [RB1,RL2,WL3], rotl).
instr(acbb, [RB1,RB2,MB3,BW4], acbb).
instr(movab, [AB1,WL2], movab).
instr(pushab, [AB1], pushab).
instr(addw2, [RW1,MW2], addw2).
instr(addw, [RW1,MW2], addw2).
instr(addw3, [RW1,RW2,WW3], addw3).
instr(addw, [RW1,RW2,WW3], addw3).
instr(subw2, [RW1,MW2], subw2).
instr(subw, [RW1,MW2], subw2).
instr(subw3, [RW1,RW2,WW3], subw3).
instr(subw, [RW1,RW2,WW3], subw3).
instr(mulw2, [RW1,MW2], mulw2).
instr(mulw, [RW1,MW2], mulw2).
instr(mulw3, [RW1,RW2,WW3], mulw3).
instr(mulw, [RW1,RW2,WW3], mulw3).
instr(divw2, [RW1,MW2], divw2).
instr(divw, [RW1,MW2], divw2).
instr(divw3, [RW1,RW2,WW3], divw3).
instr(divw, [RW1,RW2,WW3], divw3).
instr(bisw2, [RW1,MW2], bisw2).
instr(bisw, [RW1,MW2], bisw2).
instr(bisw3, [RW1,RW2,WW3], bisw3).
instr(bisw, [RW1,RW2,WW3], bisw3).
instr(bicw2, [RW1,MW2], bicw2).
instr(bicw, [RW1,MW2], bicw2).
instr(bicw3, [RW1,MW2], bicw3).
instr(bicw, [RW1,MW2], bicw3).
instr(xorw2, [RW1,MW2], xorw2).
instr(xorw, [RW1,MW2], xorw2).
instr(xorw3, [RW1,RW2,WW3], xorw3).
instr(xorw, [RW1,RW2,WW3], xorw3).
instr(mnegw, [RW1,WW2], mnegw).
instr(casew, [RW1,RW2,RW3|_], casew).
instr(movw, [RW1,WW2], movw).
instr(cmpw, [RW1,RW2], cmpw).
instr(mcomw, [RW1,WW2], mcomw).
instr(bitw, [RW1,RW2], bitw).
instr(clrw, [WW1], clrw).
instr(tstw, [RW1], tstw).
instr(incw, [MW1], incw).
instr(decw, [MW1], decw).
instr(bispsw, [RW1], bispsw).
instr(bicpsw, [RW1], bicpsw).
instr(popr, [RW1], popr).
instr(pushr, [RW1], pushr).
instr(chmk, UNKNOWN, chmk).
instr(chme, UNKNOWN, chme).
instr(chms, UNKNOWN, chms).
instr(chmu, UNKNOWN, chmu).
instr(addl2, [RL1,ML2], addl2).
instr(addl, [RL1,ML2], addl2).
instr(addl3, [RL1,RL2,WL3], addl3).
instr(addl, [RL1,RL2,WL3], addl3).
instr(subl2, [RL1,ML2], subl2).
instr(subl, [RL1,ML2], subl2).
instr(subl3, [RL1,RL2,WL3], subl3).
instr(subl, [RL1,RL2,WL3], subl3).
instr(mull2, [RL1,ML2], mull2).
instr(mull, [RL1,ML2], mull2).
instr(mull3, [RL1,RL2,WL3], mull3).
instr(mull, [RL1,RL2,WL3], mull3).
instr(divl2, [RL1,ML2], divl2).
instr(divl, [RL1,ML2], divl2).
instr(divl3, [RL1,RL2,WL3], divl3).
instr(divl, [RL1,RL2,WL3], divl3).
instr(bisl2, [RL1,ML2], bisl2).
instr(bisl, [RL1,ML2], bisl2).
instr(bisl3, [RL1,RL2,WL3], bisl3).
instr(bisl, [RL1,RL2,WL3], bisl3).
instr(bicl2, [RL1,ML2], bicl2).
instr(bicl, [RL1,ML2], bicl2).
instr(bicl3, [RL1,RL2,WL3], bicl3).
instr(bicl, [RL1,RL2,WL3], bicl3).
instr(xorl2, [RL1,ML2], xorl2).
instr(xorl, [RL1,ML2], xorl2).
instr(xorl3, [RL1,RL2,WL3], xorl3).
instr(xorl, [RL1,RL2,WL3], xorl3).
instr(mnegl, [RL1,WL2], mnegl).
instr(casel, [RL1,RL2,RL3|_], casel).
instr(movl, [RL1,WL2], movl).
instr(cmpl, [RL1,RL2], cmpl).
instr(mcoml, [RL1,WL2], mcoml).
instr(bitl, [RL1,RL2], bitl).
instr(clrl, [WL1], clrl).
instr(tstl, [RL1], tstl).
instr(incl, [ML1], incl).
instr(decl, [ML1], decl).
instr(adwc, [RL1,ML2], adwc).
instr(sbwc, [RL1,ML2], sbwc).
instr(mtpr, UNKNOWN, mtpr).
instr(mfpr, UNKNOWN, mfpr ).
instr(movpsl, [WL1], movpsl).
instr(pushl, [RL1], pushl).
instr(moval, [AL1,WL2], moval).
instr(pushal, [AL1], pushal).
instr(bbs, [RL1,AB2,BB3], bbs).
instr(bbc, [RL1,AB2,BB3], bbc).
instr(bbss, [RL1,AB2,BB3], bbss).
instr(bbcs, [RL1,AB2,BB3], bbcs).
instr(bbsc, [RL1,AB2,BB3], bbsc).
instr(bbcc, [RL1,AB2,BB3], bbcc).
instr(bbssi, [RL1,AB2,BB3], bbssi).
instr(bbcci, [RL1,AB2,BB3], bbcci).
instr(blbs, [RL1,BB2], blbs).
instr(blbc, [RL1,BB2], blbc).
instr(ffs, [RL1,RB2,VB3,WL4], ffs).
instr(ffc, [RL1,RB2,VB3,WL4], ffc).
instr(cmpv, [RL1,RB2,VB3,RL4], cmpv).
instr(cmpzv, [RL1,RB2,VB3,RL4], cmpzv).
instr(extv, [RL1,RB2,VB3,WL4], extv).
instr(extzv, [RL1,RB2,VB3,WL4], extzv).
instr(insv, [RL1,RL2,RB3,VB4], insv).
instr(acbl, [RL1,RL2,ML3,BW4], acbl).
instr(aoblss, [RL1,ML2,BB3], aoblss).
instr(aobleq, [RL1,ML2,BB3], aobleq).
instr(sobgeq, [ML1,BB2], sobgeq).
instr(sobgtr, [ML1,BB2], sobgtr).
instr(cvtlb, [RL1,WB2], cvtlb).
instr(cvtlw, [RL1,WW2], cvtlw).
instr(ashp, [RB1,RW2,AB3,RB4,RW5,AB6], ashp).
instr(cvtlp, [RL1,RW2,AB3], cvtlp).
instr(callg, [AB1,AB2], callg).
instr(calls, [RL1,AB2], calls).
instr(xfc, UNKNOWN, xfc).
instr(escd_to_digital, [], escd_to_digital).
instr(esce_to_digital, [], esce_to_digital).
instr(escf_to_digital, [], escf_to_digital).

endmod.
