// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
// --------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------
// LFSSpecArrayComp - This is an array comparator for an LArray of FSSpec records.
//		It compares on the basis of volume, directory and name, so you can add
//		multiple files with identical names in different directories
// --------------------------------------------------------------------------------------------

#pragma once
#include <LComparator.h>

class	LFSSpecArrayComp : public LComparator {
public:
						LFSSpecArrayComp() { }
	virtual				~LFSSpecArrayComp() { }
			
	virtual Int32		Compare(
								const void*			inItemOne,
								const void* 		inItemTwo,
								Uint32				inSizeOne,
								Uint32				inSizeTwo) const;
								
	virtual Boolean		IsEqualTo(
								const void*			inItemOne,
								const void* 		inItemTwo,
								Uint32				inSizeOne,
								Uint32				inSizeTwo) const;
};
