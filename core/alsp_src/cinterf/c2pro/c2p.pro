/*-------------------------------------------------------------*
                    c2p.pro
               defStruct Type definitions generated from file:
                    c2p.typ
               by ALS defStruct Type Generator
               Macros written to file: c2p.mac
 *-------------------------------------------------------------*/

module utilities.
use c2pro.
endmod.

module c2pro.
use utilities.


%--- c2p defStruct ---

export accessC2P/3.
export setC2P/3.
accessC2P(inFile,_A,_B) :- arg(1,_A,_B).
setC2P(inFile,_A,_B) :- mangle(1,_A,_B).

accessC2P(inStream,_A,_B) :- arg(2,_A,_B).
setC2P(inStream,_A,_B) :- mangle(2,_A,_B).

accessC2P(curLine,_A,_B) :- arg(3,_A,_B).
setC2P(curLine,_A,_B) :- mangle(3,_A,_B).

accessC2P(curLineNum,_A,_B) :- arg(4,_A,_B).
setC2P(curLineNum,_A,_B) :- mangle(4,_A,_B).

accessC2P(ifdef_stack,_A,_B) :- arg(5,_A,_B).
setC2P(ifdef_stack,_A,_B) :- mangle(5,_A,_B).

accessC2P(fd_stack,_A,_B) :- arg(6,_A,_B).
setC2P(fd_stack,_A,_B) :- mangle(6,_A,_B).

accessC2P(info_table,_A,_B) :- arg(7,_A,_B).
setC2P(info_table,_A,_B) :- mangle(7,_A,_B).

accessC2P(fcn_filter,_A,_B) :- arg(8,_A,_B).
setC2P(fcn_filter,_A,_B) :- mangle(8,_A,_B).

accessC2P(outFile,_A,_B) :- arg(9,_A,_B).
setC2P(outFile,_A,_B) :- mangle(9,_A,_B).

accessC2P(outStream,_A,_B) :- arg(10,_A,_B).
setC2P(outStream,_A,_B) :- mangle(10,_A,_B).

export makeC2P/1.
makeC2P(_A) :- _A=..[c2p,nil,nil,nil,0,[[]],[],[],all,nil,nil].

export makeC2P/2.
makeC2P(_A,_B) :-
        struct_lookup_subst(
            [inFile,inStream,curLine,curLineNum,ifdef_stack,fd_stack,
                info_table,fcn_filter,outFile,outStream],
            [nil,nil,nil,0,[[]],[],[],all,nil,nil],_B,_C),
        _A=..[c2p|_C].

export xmakeC2P/2.
xmakeC2P(c2p(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J),[_A,_B,_C,_D,_E,_F,_G,_H,_I,_J]).

endmod.

module utilities.
typeProperties(c2p,
    [inFile,nil,inStream,nil,curLine,nil,curLineNum,0,ifdef_stack,[],
        fd_stack,[],info_table,[],fcn_filter,all,outFile,nil,outStream,nil]).
noteOptionValue(c2p,_A,_B,_C) :- setC2P(_A,_C,_B).
endmod.
