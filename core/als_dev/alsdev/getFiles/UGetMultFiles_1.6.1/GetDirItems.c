// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
// --------------------------------------------------------------------------------------------

// GetDirItems.c - taken from the following source, with changes.

	/*
	**	Apple Macintosh Developer Technical Support
	**
	**	A collection of useful high-level File Manager routines.
	**
	**	by Jim Luther, Apple Developer Technical Support Emeritus
	**
	**	File:		MoreFilesExtras.c
	**
	**	Copyright © 1992-1996 Apple Computer, Inc.
	**	All rights reserved.
	**
	**	You may incorporate this sample code into your applications without
	**	restriction, though the sample code has been provided "AS IS" and the
	**	responsibility for its operation is 100% yours.  However, what you are
	**	not permitted to do is to redistribute the source as "DSC Sample Code"
	**	after having made changes. If you're going to re-distribute the source,
	**	we require that you make it clear in the source that the code was
	**	descended from Apple Sample Code, but that you've made changes.
	*/


#include "GetDirItems.h"

/*****************************************************************************/

pascal	OSErr	MyGetDirItems(short vRefNum,
							long dirID,
							StringPtr,
							Boolean getFiles,
							Boolean getDirectories,
							FSSpecPtr items,
							short reqItemCount,
							short *actItemCount,
							short *itemIndex) /* start with 1, then use what's returned */
{
	CInfoPBRec pb;
	OSErr error = noErr;
	long theDirID;
	FSSpec *endItemsArray = items + reqItemCount;
	
	if ( *itemIndex <= 0 )
		return ( paramErr );
	
	pb.hFileInfo.ioVRefNum = 	vRefNum;
	theDirID = dirID;

	*actItemCount = 0;
	for ( ; (items < endItemsArray) && (error == noErr); )
	{
		pb.hFileInfo.ioNamePtr = (StringPtr) &items->name;
		pb.hFileInfo.ioDirID = theDirID;
		pb.hFileInfo.ioFDirIndex = *itemIndex;
		error = PBGetCatInfoSync(&pb);
		if ( error == noErr )
		{
			items->parID = pb.hFileInfo.ioFlParID;	/* return item's parID */
			items->vRefNum = pb.hFileInfo.ioVRefNum;	/* return item's vRefNum */
			++*itemIndex;	/* prepare to get next item in directory */
			
			if ( (pb.hFileInfo.ioFlAttrib & ioDirMask) != 0 )
			{
				if ( getDirectories )
				{
					++*actItemCount; /* keep this item */
					++items; /* point to next item */
				}
			}
			else
			{
				if ( getFiles )
				{
					++*actItemCount; /* keep this item */
					++items; /* point to next item */
				}
			}
		}
	}
	return ( error );
}


