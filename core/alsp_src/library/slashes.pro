/*========================================================================*
 |			slashes.pro
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Converting from slash terms to lists
 |
 |	Author:	Ken Bowen
 |	Date:		1994
 *========================================================================*/

module builtins.

export slash2list/2.

slash2list(DestDirDesc, DestDirList)
	:-
	slash2list(DestDirDesc, [], DestDirList).

slash2list((A/ (B/C) ), Accum, DestDirList)
	:-!,
	slash2list(( (A/B) / C ), Accum, DestDirList).

slash2list(( A/B ), Accum, DestDirList)
	:-!,
	slash2list( A, [B | Accum], DestDirList).

slash2list( A, Accum, NewAccum)
	:-
	subPath(APath,A),
	fix_front(APath, FixedAPath),
	append(FixedAPath,Accum,NewAccum).

fix_front(['' | Rest], ['/' | Rest])
	:-!.
fix_front(APath, APath).

endmod.
