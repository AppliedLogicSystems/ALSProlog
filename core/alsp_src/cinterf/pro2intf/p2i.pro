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

accessP2I(fPrefix,_A,_B) :- arg(4,_A,_B).
setP2I(fPrefix,_A,_B) :- mangle(4,_A,_B).

accessP2I(dPred,_A,_B) :- arg(5,_A,_B).
setP2I(dPred,_A,_B) :- mangle(5,_A,_B).

accessP2I(tPred,_A,_B) :- arg(6,_A,_B).
setP2I(tPred,_A,_B) :- mangle(6,_A,_B).

accessP2I(curONum,_A,_B) :- arg(7,_A,_B).
setP2I(curONum,_A,_B) :- mangle(7,_A,_B).

accessP2I(curDispNum,_A,_B) :- arg(8,_A,_B).
setP2I(curDispNum,_A,_B) :- mangle(8,_A,_B).

accessP2I(initFcns,_A,_B) :- arg(9,_A,_B).
setP2I(initFcns,_A,_B) :- mangle(9,_A,_B).

accessP2I(outFile,_A,_B) :- arg(10,_A,_B).
setP2I(outFile,_A,_B) :- mangle(10,_A,_B).

accessP2I(outStream,_A,_B) :- arg(11,_A,_B).
setP2I(outStream,_A,_B) :- mangle(11,_A,_B).

accessP2I(cFileList,_A,_B) :- arg(12,_A,_B).
setP2I(cFileList,_A,_B) :- mangle(12,_A,_B).

accessP2I(pFileList,_A,_B) :- arg(13,_A,_B).
setP2I(pFileList,_A,_B) :- mangle(13,_A,_B).

accessP2I(pfStreams,_A,_B) :- arg(14,_A,_B).
setP2I(pfStreams,_A,_B) :- mangle(14,_A,_B).

accessP2I(module,_A,_B) :- arg(15,_A,_B).
setP2I(module,_A,_B) :- mangle(15,_A,_B).

accessP2I(fmax,_A,_B) :- arg(16,_A,_B).
setP2I(fmax,_A,_B) :- mangle(16,_A,_B).

accessP2I(smax,_A,_B) :- arg(17,_A,_B).
setP2I(smax,_A,_B) :- mangle(17,_A,_B).

accessP2I(cmax,_A,_B) :- arg(18,_A,_B).
setP2I(cmax,_A,_B) :- mangle(18,_A,_B).

export makeP2I/1.
makeP2I(_A) :-
        _A=..
            [p2i,nil,nil,nil,nil,nil,nil,0,0,[],nil,nil,[],[],[],builtins,
                100000,100000,100000].

export makeP2I/2.
makeP2I(_A,_B) :-
        struct_lookup_subst(
            [inFile,inStream,baseName,fPrefix,dPred,tPred,curONum,curDispNum,
                initFcns,outFile,outStream,cFileList,pFileList,pfStreams,
                module,fmax,smax,cmax],
            [nil,nil,nil,nil,nil,nil,0,0,[],nil,nil,[],[],[],builtins,100000,
                100000,100000],
            _B,_C),
        _A=..[p2i|_C].

export xmakeP2I/2.
xmakeP2I(p2i(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R),
    [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R]).

endmod.

module utilities.
typeProperties(p2i,
    [inFile,nil,inStream,nil,baseName,nil,fPrefix,nil,dPred,nil,tPred,nil,
        curONum,0,curDispNum,0,initFcns,[],outFile,nil,outStream,nil,
        cFileList,[],pFileList,[],pfStreams,[],module,builtins,fmax,100000,
        smax,100000,cmax,100000]).
noteOptionValue(p2i,_A,_B,_C) :- setP2I(_A,_C,_B).
endmod.
