// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
// --------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------
// LFSSpecArrayComp - This is an array comparator for an LArray of FSSpec records.
//		It compares on the basis of volume, directory and name, so you can add
//		multiple files with identical names in different directories
// --------------------------------------------------------------------------------------------

#include "LFSSpecArrayComp.h"
#include <string.h>
#include <LString.h>

Int32
LFSSpecArrayComp::Compare(
	const void*		inItemOne,
	const void*		inItemTwo,
	Uint32			/* inSizeOne */,
	Uint32			/* inSizeTwo */) const
{
	FSSpec fs1;
	FSSpec fs2;
	
	fs1 = *((FSSpec *) inItemOne);
	fs2 = *((FSSpec *) inItemTwo);

	short nameCompare = StringOrder(fs1.name, fs2.name, smSystemScript, smSystemScript,
							systemCurLang, systemCurLang);	// we use the system's alphabetizing
															// algorithm so the added list will
															// have the same order as the SF list
	if (nameCompare == 0) {
		if (fs1.parID == fs2.parID)	{
			if (fs1.vRefNum != fs2.vRefNum) {
				return 0;
			} else {
				return (fs1.vRefNum - fs2.vRefNum);
			}
		} else {
			return (fs1.parID - fs2.parID);
		}
	} else {
		return nameCompare;
	}
	return 0;	// we'll never get here; just so compiler won't complain

/*	if (fs1.vRefNum != fs2.vRefNum) {
		return (fs1.vRefNum - fs2.vRefNum);
	} else {			// volumes are the same
		if (fs1.parID != fs2.parID)	{
			return (fs1.parID - fs2.parID);
		} else {		// directories are the same
			if (fs1.name != fs2.name) {
				return ::StringOrder(fs1.name, fs2.name, smSystemScript, smSystemScript,
									systemCurLang, systemCurLang);	// we use the system's alphabetizing
																	// algorithm so the added list will
																	// have the same order as the SF list
			} else {	// everything's the same
				return 0;
			}
		}
	}
	return 0;	// we'll never get here; just so compiler won't complain
*/
}

Boolean
LFSSpecArrayComp::IsEqualTo(
	const void*		inItemOne,
	const void*		inItemTwo,
	Uint32			/* inSizeOne */,
	Uint32			/* inSizeTwo */) const
{
	FSSpec fs1;
	FSSpec fs2;
	fs1 = *((FSSpec *) inItemOne);
	fs2 = *((FSSpec *) inItemTwo);
	LStr255 name1(fs1.name);
	LStr255 name2(fs2.name);
	if ((fs1.vRefNum == fs2.vRefNum) && (fs1.parID == fs2.parID) && (name1.CompareTo(name2) == 0))
		return true;
	return false;
}
