// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
// --------------------------------------------------------------------------------------------

#pragma once
#include "LFSSpecArrayComp.h"
#include <LArray.h>
#include <Navigation.h>

// --------------------------------------------------------------------------------------------
//	Items in the custom dialog box.
// --------------------------------------------------------------------------------------------
const ResIDT	kDLOG_OpenMultiple		= 900;
const ResIDT	kStdButton_Add			= 1;
const ResIDT	kDesktopButton			= 6;
const ResIDT	kUserItem_FileList		= 7;
const ResIDT	kStaticText_Prompt		= 10;
const ResIDT	kUserItem_AddedList		= 11;
const ResIDT	kStdButton_Done			= 12;
const ResIDT	kStdButton_Remove		= 13;
const ResIDT	kStdButton_AddAll		= 15;
const ResIDT	kPict_DividerLine		= 14;
const ResIDT	kStdButton_RemoveAll	= 16;
const ResIDT	kStdButton_Select		= 17;
const short		kHome					= 1;
const short		kEnd					= 4;
const short		kUpArrow				= 30;
const short		kDownArrow				= 31;
const short		kPageUp					= 11;
const short		kPageDown				= 12;
const short		kUpArrowPlusCO			= kUpArrow + sfHookCharOffset;
const short		kDownArrowPlusCO		= kDownArrow + sfHookCharOffset;

const ResIDT	kSTR_List				= 900;
const ResIDT	kSTR_SelectItem			= 1;
const Str255	kFolderButtonRoot		= "\pAdd \"";

const ResIDT	kIconLDEF				= 128;
const short		kBWIconSize				= 64;
const short		k4bitIconSize			= 128;
const short		k8bitIconSize			= 256;

const	Point	kDialogLocation = {-1,-1};		// default location

const	short	kMaxNumFilesToFind = 20;		// number of files to examine at a time
const	short	kNumAddedCellsVisible = 5;		// number of cells that are visible in the added
												// file list at once

enum UGMF_Options {
	kDontShowFolders = 0x1,			// do we want to suppress display of folders?  (This inhibits navigation severely)
	kAddFolders = 0x2,				// are we adding folders & volumes also?
	kAllowConversion = 0x4,			// should we allow automatic translation by MacEasyOpen?  Turning this off may (I'm not 100% sure) suppress locked file alerts, also
	kShowInvisible = 0x8,			// should we show invisible files in the lists?
	kResolveAliases = 0x10,			// should we add aliases as the alias files or as the files/folders they point to?
	kAddUnresolvedAliases = 0x20,	// if an alias is unresolved, should we add it as an alias to the list?
	kForceStandardFile = 0x40,		// never use NavServices
	kSuppressTypePopup = 0x80,		// don't show type popup (see Read Me file for notes on using the popup)
	kDefaultOptions = kAllowConversion | kResolveAliases | kSuppressTypePopup
};

//  ICON PLOTTING
struct dataHolderType {
	short	whichFocused;
};
					
typedef struct ListItem {
	FSSpec			theSpec;
	Str255			name;
} ListItem, *ListItemPtr, **ListItemHdl;

typedef struct ToolListItem {
	OSType			type;
	OSType			creator;
	short			driveInfo;
	short			vRefNum;

	long			dirID;
	short			dontknow2;

	short			fndrFlags;
	short			dontknow3;
	short			dontKnow4;
	long			dontknow5;
	short			doyoucare;
	short			Idontcare;
	short			notabit;
	Str63			name;
} ToolListItem, *ToolListItemPtr, **ToolListItemHdl;
typedef struct{
#if powerc
   RoutineDescriptor desc;
#else
   Int16 op;
#endif
   ListDefProcPtr addr;
}JmpBuf, *JmpBufPtr, **JmpBufHandle;
#define JMPINSTRUCT 0x4EF9

/* constants for spacing */

#define kLeftOffset	2
#define kTopOffset	1
#define kIconSpace	2



class UGetMultipleFiles {
	static class IconErr{};

	public:
		enum { err_otherGetDirErr =	'oGDE' };
									UGetMultipleFiles();
									UGetMultipleFiles(Str255 prompt, short numTypes = 0, SFTypeList typeList = nil,
													LArray *inFileList = nil, FSSpecPtr inStartLoc = nil,
													unsigned long flags = kDefaultOptions);
									~UGetMultipleFiles();
		static LArray				*GetFSSpecs();
		static StandardFileReply	GetSFReply();
		static Boolean 				IsFile(FSSpec &inFile);
		static Boolean 				IsVisible(FSSpec &inFile);
		static Boolean 				IsAlias(FSSpec &inFile);

	private:	// member functions
		static void					SFPGetFiles(Str255 prompt, FSSpecPtr inStartLoc);
		static void					NSGetFiles(Str255 prompt, FSSpecPtr inStartLoc);

		static pascal Boolean		myModalFilter (DialogPtr theDialogP, EventRecord *event,
												 short *itemHit, void *myUnused);
		static pascal short			myDialogHook (short item, DialogPtr theDialogP, void *myUnused);
		static pascal Boolean		myFileFilter (CInfoPBPtr pb, void *ioParam);
		static pascal void			myActivateProc (DialogPtr theDialogP, short itemNum,
												 Boolean activating, void *ioParam);
		static pascal void			MyDrawListItem (WindowPtr theWindow, short item);
		static void					AddOneToList(Str255 , long , short);
		static void					AddAllToList(Boolean atDesktop);
		static void					RemoveAllFromList();
		static void					RemoveFromList();
		static long					GetSFCurDir(void);
		static short				GetSFVRefNum(void);
		static ListHandle			MyCreateTextListInDialog (DialogPtr , short );
		static ListHandle			MyCreateVerticallyScrollingList (WindowPtr , Rect , short , short );
		static void					SetEnable(ResIDT theItem, Boolean enabled);
		static Boolean				GetEnabled(ResIDT theItem);
		static void					ReplaceString ( Str255 destStr, Str255 whatStr, Str255 byStr ); 
		static Boolean 				TypeInList( OSType inType );
		static void					SetCurrentVolume(short vRefNum);
		static void					SetCurrentDirectory(long dirID);
		static void					FixButtons (DialogPtr theDialogP, void *ioParam);
		static void					SetButtonTitle (Handle ButtonHdl, Str255 name, Rect ButtonRect);
		static void					DoListKey(short theKey, ListHandle theListH);
		static OSErr				GetCatInfoNoName(short vRefNum, long dirID, ConstStr255Param name, CInfoPBPtr pb);
		static OSErr				GetCatInfoNoName(FSSpec &inSpec, CInfoPBPtr pb) {return GetCatInfoNoName(inSpec.vRefNum, inSpec.parID, inSpec.name, pb);};
		static void 				EnsureSelection(short inActiveList);
		static inline Boolean		IsFolder(CInfoPBPtr pb) {return (pb->hFileInfo.ioFlAttrib & 0x10);};
		static OSType				GetType(FSSpec &inFile);
		static void					InitScreenDepth(DialogPtr inDPtr);

		static void					ListElementProc(Rect *cellRect, Cell lCell, ListHandle theList, ListItemHdl lih);
		static OSErr				DrawFileIcon(FSSpec *spec, Rect *drawRect);
		static void					InstallMyLDEF(ListHandle list, ListDefProcPtr proc);
		static pascal void			MyLDEF(short lMessage, Boolean lSelect,	Rect *lRect, Cell lCell,
											short lDataOffset, short lDataLen, ListHandle lHandle);
		static void					DrawMsg(Boolean fSelect, Rect *r, Cell cell, ListHandle lh);
		static void					HiliteMsg(Boolean fSelect, Rect *r);

		static pascal void			NSEventFilterForPP(NavEventCallbackMessage callBackSelector,
											NavCBRecPtr callBackParms, NavCallBackUserData callBackUD);
		static pascal Boolean		NSFileFilter(AEDesc *theItem, void *theInfo, NavCallBackUserData callBackUD,
											NavFilterModes filterMode);

	private:	// data members
		static ListHandle			sAddedListH;
		static DialogPtr			sDialogP;
		static StandardFileReply	sReply;
		static LArray				*sTheFSpecs;
		static LArray				*sNixFiles;
		static short				sNumTypes;
		static OSType				*sTypeList;
		static Boolean				sNeedToRebuild;
		static Boolean				sShowFolders;
		static Boolean				sAddFolders;
		static Boolean				sShowInvisible;
		static Boolean				sAddUnresolvedAliases;
		static Boolean				sSuppressTypePopup;
		static Boolean 				sAllowConversion;		// Allow MEO to convert the File,
														// allow checking for locked files, etc. 
		static Boolean				sResolveAliases;
		static short				sKeyPressed;
		static short				sIconSize;
		static short				sIconType;
		static UserItemUPP			sTheDLUserItemUPP;	// NWF
};




