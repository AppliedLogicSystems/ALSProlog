// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
// --------------------------------------------------------------------------------------------

// GetDirItems.h - taken from the following source, with changes.

	/*
	**	Apple Macintosh Developer Technical Support
	**
	**	A collection of useful high-level File Manager routines.
	**
	**	by Jim Luther, Apple Developer Technical Support Emeritus
	**
	**	File:		MoreFilesExtras.h
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


#ifdef __cplusplus
extern "C" {
#endif


#if PRAGMA_ALIGN_SUPPORTED
#pragma options align=reset
#endif

/*****************************************************************************/

pascal	OSErr	MyGetDirItems(short vRefNum,
							long dirID,
							StringPtr name,
							Boolean getFiles,
							Boolean getDirectories,
							FSSpecPtr items,
							short reqItemCount,
							short *actItemCount,
							short *itemIndex);
/*	¦ Return a list of items in a directory.
	The GetDirItems function returns a list of items in the specified
	directory in an array of FSSpec records. File, subdirectories, or
	both can be returned in the list.
	
	A noErr result indicates that the items array was filled
	(actItemCount == reqItemCount) and there may be additional items
	left in the directory. A fnfErr result indicates that the end of
	the directory list was found and actItemCount items were actually
	found this time.

	vRefNum			input:	Volume specification.
	dirID			input:	Directory ID.
	name			input:	Pointer to object name, or nil when dirID
							specifies a directory that's the object.
	getFiles		input:	Pass true to have files added to the items list.
	getDirectories	input:	Pass true to have directories added to the
							items list.
	items			input:	Pointer to array of FSSpec where the item list
							is returned.
	reqItemCount	input:	Maximum number of items to return (the number
							of elements in the items array).
	actItemCount	output: The number of items actually returned.
	itemIndex		input:	The current item index position. Set to 1 to
							start with the first item in the directory.
					output:	The item index position to get the next item.
							Pass this value the next time you call
							GetDirItems to start where you left off.
	
	Result Codes
		noErr				0		No error, but there are more items
									to list
		nsvErr				-35		No such volume
		ioErr				-36		I/O error
		bdNamErr			-37		Bad filename
		fnfErr				-43		File not found, there are no more items
									to be listed.
		paramErr			-50		No default volume or itemIndex was <= 0
		dirNFErr			-120	Directory not found or incomplete pathname
		afpAccessDenied		-5000	User does not have the correct access
		afpObjectTypeErr	-5025	Directory not found or incomplete pathname
*/



#ifdef __cplusplus
}
#endif

#ifndef __COMPILINGMOREFILES
#undef pascal
#endif
