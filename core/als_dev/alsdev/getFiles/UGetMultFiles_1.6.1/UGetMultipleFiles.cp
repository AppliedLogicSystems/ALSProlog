// --------------------------------------------------------------------------------------------
//	UGetMulltipleFiles - An Add Files utility class
//		By David Hirsch
//			please read the "Read Me" file.
//
//	Notes:
//		1)	The List Manager's list that we display in the dialog box starts with zero for the
//			first item, while the LArray that holds the FSSpec records starts with 1.
//
// --------------------------------------------------------------------------------------------

#include "UGetMultipleFiles.h"
#include <UDrawingState.h>
#include <string.h>
#include <LString.h>
#include <UMemoryMgr.h>
#include "GetDirItems.h"
#include <UDrawingState.h>
#include <UDrawingUtils.h>
#include <UDesktop.h>
#include "patch.h"
#include <Icons.h>
#include <Sound.h>

// --------------------------------------------------------------------------------------------
//	 Global variables
// --------------------------------------------------------------------------------------------
ListHandle 				gToolboxList = nil;
UniversalProcPtr		patchCode = nil;
Boolean 				needToFixButtons;	// this is so we can delay one "cycle"
Boolean					tabKludge = false;

// --------------------------------------------------------------------------------------------
//	 Static class variables
// --------------------------------------------------------------------------------------------

ListHandle			UGetMultipleFiles::sAddedListH = nil;
DialogPtr			UGetMultipleFiles::sDialogP = nil;
StandardFileReply	UGetMultipleFiles::sReply;
LArray				*UGetMultipleFiles::sTheFSpecs = nil;
LArray				*UGetMultipleFiles::sNixFiles = nil;
short				UGetMultipleFiles::sNumTypes = -1;
OSType 				*UGetMultipleFiles::sTypeList = nil;
Boolean				UGetMultipleFiles::sNeedToRebuild = false;
Boolean				UGetMultipleFiles::sAllowConversion = true;
Boolean				UGetMultipleFiles::sShowFolders = true;
Boolean				UGetMultipleFiles::sAddFolders = false;
Boolean				UGetMultipleFiles::sResolveAliases = true;
Boolean				UGetMultipleFiles::sAddUnresolvedAliases = false;
Boolean				UGetMultipleFiles::sSuppressTypePopup = false;
short				UGetMultipleFiles::sKeyPressed = 0;
short				UGetMultipleFiles::sIconSize = 0;
short				UGetMultipleFiles::sIconType = 0;
Boolean				UGetMultipleFiles::sShowInvisible = false;
UserItemUPP			UGetMultipleFiles::sTheDLUserItemUPP = nil; // NWF

// --------------------------------------------------------------------------------------------
//	UGetMultipleFiles
// 		Default constructor; calls detailed constructor, but without a prompt or typelist.
// --------------------------------------------------------------------------------------------
UGetMultipleFiles::UGetMultipleFiles()
{
	UGetMultipleFiles((unsigned char *)"");
}

// --------------------------------------------------------------------------------------------
//	UGetMultipleFiles - main constructor.
// --------------------------------------------------------------------------------------------
UGetMultipleFiles::UGetMultipleFiles(Str255 prompt, short numTypes,
									OSType *typeList, LArray *inFileList,
									FSSpecPtr inStartLoc, unsigned long flags)
{
	LFSSpecArrayComp *comparatorP = nil;
	if (sTheFSpecs == nil) {
		comparatorP = new LFSSpecArrayComp();	// create comparator for the FSSpec array
		sTheFSpecs = new LArray(sizeof(FSSpec), comparatorP);	// create the FSSpec array
	} else {
		sTheFSpecs->RemoveItemsAt(sTheFSpecs->GetCount(), 1);
	}
	sTheFSpecs->SetKeepSorted(true);			// sort the array

	sNixFiles = inFileList;
	if ((sNixFiles != nil)) {
		comparatorP = new LFSSpecArrayComp();	// create comparator for the FSSpec array
		sNixFiles->SetComparator(comparatorP);
	}

	sNumTypes = numTypes;						// store the numTypes parameter
	if (sNumTypes > 0) {
		if (sTypeList != nil)
			delete[] sTypeList;
		sTypeList = new OSType[sNumTypes];
	}

	for (short j=0; j <= numTypes - 1; j++)			// store each type
		sTypeList[j] = typeList[j];

	sAllowConversion = (flags & kAllowConversion) > 0;
	sShowFolders = (flags & kDontShowFolders) == 0;
	sAddFolders = (flags & kAddFolders) > 0;
	sShowInvisible = (flags & kShowInvisible) > 0;
	sResolveAliases = (flags & kResolveAliases) > 0;
	sAddUnresolvedAliases = (flags & kAddUnresolvedAliases) > 0;
	sSuppressTypePopup = (flags & kSuppressTypePopup) > 0;
	
	UDesktop::Deactivate();
	::InitCursor();

	try {
		// Dispatch Call
		if (!(flags & kForceStandardFile) && NavServicesAvailable() && !(sAddFolders)) {
			NSGetFiles(prompt, inStartLoc);
		} else {
			SFPGetFiles(prompt, inStartLoc);
		}
	} catch (...) {
		::SysBeep(9);
	}
	
	UDesktop::Activate();
}

void
UGetMultipleFiles::SFPGetFiles(Str255 prompt, FSSpecPtr inStartLoc)
{
	FileFilterYDUPP		theFileFilterYDUPP;
	DlgHookYDUPP		theDlgHookYDUPP;
	ModalFilterYDUPP	theModalFilterYDUPP;
	ActivateYDUPP		theActivateYDUPP;
	PatchRouterUPP		thePatchRouterUPP;
	FSSpec				oneFSSpec;
	Int16				activateListArray[3];
	Handle				patchResource;
	dataHolderType		myData;
	

	myData.whichFocused = kUserItem_FileList;		// for focus routines; begins focused on Toolbox List

	prompt[63] = '\0';	// truncate the prompt so it will fit in the name field of an FSSpec - I should have originally made it a Str63, but didn't.
						//  Doing it this way for back-compatibility
	LString::CopyPStr((unsigned char *)prompt, (unsigned char *)oneFSSpec.name);		// send in the prompt disguised as the first FSSpec
	sTheFSpecs->InsertItemsAt(1, (sTheFSpecs->GetCount()) + 1, &oneFSSpec);
	
	// allocate global UPP for our draw proc
//	gListElementProcUPP = NewRefConLDEFProc(ListElementProc);
	theDlgHookYDUPP = NewDlgHookYDProc(myDialogHook);
	theModalFilterYDUPP = NewModalFilterYDProc(myModalFilter);
	theFileFilterYDUPP = NewFileFilterYDProc(myFileFilter);
	theActivateYDUPP = NewActivateYDProc(myActivateProc);
	activateListArray[0] = 2;
	activateListArray[1] = kUserItem_FileList;
	activateListArray[2] = kUserItem_AddedList;
	if (inStartLoc != nil) {
		SetCurrentVolume(inStartLoc->vRefNum);
		if (IsFile(*inStartLoc)) {
			SetCurrentDirectory(inStartLoc->parID);
		} else {
			CInfoPBRec pbr;
			GetCatInfoNoName(inStartLoc->vRefNum, inStartLoc->parID, inStartLoc->name, &pbr);
			SetCurrentDirectory(pbr.dirInfo.ioDrDirID);
		}
	}

	patchResource = GetResource('patc', 0);
	if (patchResource) {
		HLock(patchResource);
		HNoPurge(patchResource);
		
		thePatchRouterUPP = NewPatchRouterProc((ProcPtr)*patchResource);
		CallPatchRouterProc(thePatchRouterUPP, kInstall, &gToolboxList);

		gToolboxList = nil;
		::CustomGetFile(theFileFilterYDUPP, -1, NULL, &sReply, kDLOG_OpenMultiple, kDialogLocation,
			theDlgHookYDUPP, theModalFilterYDUPP, activateListArray, theActivateYDUPP, (void *) &myData);
		gToolboxList = nil;

		CallPatchRouterProc(thePatchRouterUPP, kRemove, &gToolboxList);
		DisposeRoutineDescriptor(thePatchRouterUPP);
	}
	else {
		SysBeep(1);
	}
	DisposeRoutineDescriptor(theDlgHookYDUPP);
	DisposeRoutineDescriptor(theModalFilterYDUPP);
	DisposeRoutineDescriptor(theActivateYDUPP);
//	DisposeRoutineDescriptor(gListElementProcUPP);
	DisposeRoutineDescriptor(theFileFilterYDUPP);
	DisposeRoutineDescriptor(sTheDLUserItemUPP); // NWF

		// If we weren't allowing conversion/checking, and we got a cancel message, but
		// we never deleted the files, then what really happened was that the user
		// in fact clicked the Done button, and the Dialog Hook sent the cancel message
		// to get around the conversion/checking stuff.  Reset the reply field.
	if ((!sAllowConversion) && !(sReply.sfGood) && (sTheFSpecs->GetCount() > 0)) {
		sReply.sfGood = true;
	}

//	Kludge for NOW Direct Open feature of SuperBoomerang - Direct Open does not 
//	fill in parID and vRefNum in the dialog hook which UGetMultipleFiles relies
//	on to work its magic.  Fortunately, Direct Open does fill in all FSSpec
//	fields in the reply record.  if there is only one FSSpec in the mTheFSpecs array
//	and its parID and vRefNum are both 0, replace it with the FSSpec in the reply record.
	if (sTheFSpecs->GetCount() == 1)  {
		sTheFSpecs->FetchItemAt(1, &oneFSSpec);
		if ( (0 == oneFSSpec.parID) && (0 == oneFSSpec.vRefNum) )  {
			sTheFSpecs->RemoveItemsAt(1, 1); // remove the incomplete FSSpec
			sTheFSpecs->InsertItemsAt(1, 1, &sReply.sfFile);
		}
	}
}

// --------------------------------------------------------------------------------------------
//	UGetMultipleFiles - main destructor.
// --------------------------------------------------------------------------------------------
UGetMultipleFiles::~UGetMultipleFiles()
{
	if (sTheFSpecs != nil) {
		delete sTheFSpecs;
		sTheFSpecs = nil;
	}
	if (sTypeList != nil) {
		delete[] sTypeList;
		sTypeList = nil;
	}
}

// --------------------------------------------------------------------------------------------
//	GetFSSpecs - accessor for the FSSpec LArray.
// --------------------------------------------------------------------------------------------
LArray* UGetMultipleFiles::GetFSSpecs()
{
	return sTheFSpecs;
}

// --------------------------------------------------------------------------------------------
//	GetSFReply - accessor for the reply record.
// --------------------------------------------------------------------------------------------
StandardFileReply UGetMultipleFiles::GetSFReply()
{
	return sReply;
}


// --------------------------------------------------------------------------------------------
//	myModalFilter - Modal Dialog calls this just after the Standard File Package's filter
// 		This routine was taken from the Apple Macintosh Developer Technical Support
//			Standard File Application, which I downloaded from Apple's web site, and modified.
// --------------------------------------------------------------------------------------------
pascal Boolean UGetMultipleFiles::myModalFilter (DialogPtr theDialogP, EventRecord *event,
												 short *itemHit, void *ioParam)
{

	Rect			itemRect;
	Point			clickPos;
	Handle			itemHandle;
	Boolean			firstTimeThrough = true;
	short			itemKind; // NWF

    if (GetWRefCon(theDialogP) != sfMainDialogRefCon)
            return false;
	if ((*event).what == mouseDown) {
		clickPos = (*event).where;
		GlobalToLocal(&clickPos);
		GetDialogItem(theDialogP, kUserItem_AddedList, &itemKind, &itemHandle, &itemRect); // NWF
		if (PtInRect(clickPos, &itemRect)) {
			if (::LClick(clickPos, (*event).modifiers, sAddedListH)) {
				RemoveFromList();
				sNeedToRebuild = true;
			}
			*itemHit = kUserItem_AddedList;
			return true;
		}
	}
	if ((*event).what == keyDown) {
		Int16	theKey = (*event).message & charCodeMask;
		if (theKey == '\t') {
			if ((((dataHolderType *)ioParam)->whichFocused) == kUserItem_AddedList)
				((dataHolderType *)ioParam)->whichFocused = kUserItem_FileList;
			else
				((dataHolderType *)ioParam)->whichFocused = kUserItem_AddedList;
		}
		if (theKey == kHome ||
			theKey == kEnd ||
			theKey == kUpArrow ||
			theKey == kDownArrow ||
			theKey == kPageUp ||
			theKey == kPageDown) {
			sKeyPressed = theKey;
		}
	}
	return false;
}

// --------------------------------------------------------------------------------------------
//	myFileFilter - This dictates which files to show in the list
// --------------------------------------------------------------------------------------------
pascal Boolean UGetMultipleFiles::myFileFilter (CInfoPBPtr pb, void *ioParam)
{
#pragma unused(ioParam)

	// extract the FSSpec from the CInfoPBPtr
	FSSpec tempSpec;
	LStr255 nameStr(pb->hFileInfo.ioNamePtr);
	UInt8 lastNamePos = nameStr.ReverseFind(':');
	nameStr.Remove(1, lastNamePos);
	OSErr theErr = ::FSMakeFSSpec(pb->hFileInfo.ioVRefNum, pb->hFileInfo.ioFlParID, nameStr, &tempSpec);
	
	if (sResolveAliases && IsAlias(tempSpec)) {
		Boolean targetIsFolder,	dummy;
		OSErr err;
		err = ::ResolveAliasFile(&tempSpec, true, &targetIsFolder, &dummy);
		if (err != noErr) {
			::FSMakeFSSpec(pb->hFileInfo.ioVRefNum,
					pb->hFileInfo.ioFlParID,
					nameStr, &tempSpec);
		}
	}
	
	if (theErr != noErr)
		return false;

	if (!sAddFolders) {	//	Don't need this if adding folders, since we're treating them like files!
		if (sShowFolders && IsFolder(pb)) {	// if we're showing folders and this is a folder
			return false;	// then show it
		}
	}

	if (!IsFolder(pb) && !TypeInList( pb->hFileInfo.ioFlFndrInfo.fdType )) {	// if it's not a folder and the type isn't in the list
		return true;	// then don't show it
	}	// Note: we need this check for when we're adding folders and showing them
	
	if (!IsVisible(tempSpec) && !sShowInvisible)
		return true;	// don't show invisibles
	
	// see if it's in the list of added files
	if (sTheFSpecs->FetchIndexOf(&tempSpec) != sTheFSpecs->index_Bad)	// if it is in the added list
		return true;	// then don't show it
	else {
		if (sNixFiles == nil)	// don't suppress any
			return false;
		// see if it's in the list of excluded files
		else if (sNixFiles->FetchIndexOf(&tempSpec) != sNixFiles->index_Bad)	// if it is in the exclude list
			return true;			// then don't show it
		else
			return false;
	}
}

// --------------------------------------------------------------------------------------------
//	myActivateProc - Modal Dialog calls this when activating or deactivating a list box
// --------------------------------------------------------------------------------------------
pascal void UGetMultipleFiles::myActivateProc (DialogPtr theDialogP, short itemNum,
												 Boolean activating, void *ioParam)
{
	dataHolderType	*myData = (dataHolderType *) ioParam;
	StColorPortState theState(theDialogP);
	RGBColor	theBackColor;
	Rect		itemRect;
	short		itemKind;
	Handle		itemHandle; // NWF

	if (GetWRefCon(theDialogP) != sfMainDialogRefCon)
    	return;
	::PenNormal();
	::PenSize(2, 2);
	if (itemNum == kUserItem_AddedList) {
		GetDialogItem(theDialogP, itemNum, &itemKind, &itemHandle, &itemRect); // NWF
		::InsetRect(&itemRect, -4, -4);
		if (activating) {
			::ForeColor(blackColor);
			::FrameRect(&itemRect);
			::LActivate(true, sAddedListH);
			EnsureSelection(kUserItem_AddedList);
			myData->whichFocused = kUserItem_AddedList;
		} else {
			::LActivate(false, sAddedListH);
			::PenNormal();
			::PenSize(2, 2);
			::GetBackColor(&theBackColor);
			::RGBForeColor(&theBackColor);
			::FrameRect(&itemRect);
			myData->whichFocused = kUserItem_FileList;
		}
	} else {
		if (activating) {
			EnsureSelection(kUserItem_FileList);
			tabKludge = true;
		}
	}
	needToFixButtons = true;
}

#include <stdlib.h>
// --------------------------------------------------------------------------------------------
//	myDialogHook - the Standard File Package calls this routine just after myModalFilter.
// --------------------------------------------------------------------------------------------
pascal short UGetMultipleFiles::myDialogHook (short item, DialogPtr theDialogP, void *ioParam)
{
	Cell			curSelCell = {0,0};
	Cell			nextCell;
	Boolean			result;
	dataHolderType	*myData = (dataHolderType *) ioParam;
	static Boolean 	onceThrough;
	static Boolean	haveRebuilt = false;
	Rect			itemRect;
	Handle			itemHandle;
	short			itemKind;
	FSSpec			oneFSSpec;
	
	if (GetWRefCon(theDialogP) != sfMainDialogRefCon)
	   	return item;

	if (item >= sfHookCharOffset) {	// this makes a char keypress (but not [Enter], for example) induce a button fix
		needToFixButtons = true;
	}
	
	switch (item) {
		case sfHookFirstCall:	// gets called just before the dialog is made
			onceThrough = false;
			needToFixButtons = true;
			::LSetDrawingMode(true, gToolboxList); // NWF
			ListHandle temp = gToolboxList;	// save the Toolbox ListHandle
			sAddedListH = MyCreateTextListInDialog(theDialogP, kUserItem_AddedList); // make the added list
			::LSetDrawingMode(false, sAddedListH);
			InstallMyLDEF(sAddedListH, MyLDEF);
			gToolboxList = temp;	// replace the Toolbox LH that was overwritten by our call of LNew
			GetDialogItem(theDialogP, kUserItem_AddedList, &itemKind, &itemHandle, &itemRect); // NWF
			sTheDLUserItemUPP = NewUserItemProc(MyDrawListItem);		// install the draw routine for
																		//	the list of added files
			SetDialogItem(theDialogP, kUserItem_AddedList, userItem, (Handle)sTheDLUserItemUPP, &itemRect); // NWF

			sDialogP = theDialogP;							// store the dialog pointer
			if (!sAddFolders) {
				HideDialogItem(sDialogP, kStdButton_Select); 
				GetDialogItem(sDialogP, kStdButton_Select, &itemKind, &itemHandle, &itemRect); // NWF
				::SizeWindow(sDialogP, sDialogP->portRect.right - sDialogP->portRect.left,
					sDialogP->portRect.bottom - sDialogP->portRect.top - (itemRect.bottom - itemRect.top), true);
			}
			SetEnable(sfItemOpenButton, true);	// Add button is active, no highlighting
			SetEnable(kStdButton_Done, true);	// Done button is active, no highlighting
			SetEnable(kStdButton_Remove, false);	// Remove button is inactive
			SetEnable(kStdButton_RemoveAll, false);	// Remove all button is inactive

			SetEnable(kStdButton_AddAll, true);	// Add all button is active, no highlighting
			sTheFSpecs->FetchItemAt(1, &oneFSSpec);			// Extract the prompt from the first FSSpec
			sTheFSpecs->RemoveItemsAt(1, 1);
			GetDialogItem(theDialogP, kStaticText_Prompt, &itemKind, &itemHandle, &itemRect); // NWF
			SetDialogItemText(itemHandle, oneFSSpec.name);
			
			onceThrough = true;

			return sfHookNullEvent;
		break;
		case sfHookLastCall:	// called just after we're done with the dialog
			if (sAddedListH) {
				LDispose(sAddedListH);
				sAddedListH = nil;
			}
			if ((sTheFSpecs->GetCount() > 0) && (!sAllowConversion)) {
					// since we're supressing checking, we won't be sending an sfItemOpenButton
					// "message", that would cause the Standard File Package to reset the current
					// volume and directory to that contained in the sReply.sfFile field.
					// We'll therefore set it by hand.
				FSSpec *oneSpec = (FSSpec *) sTheFSpecs->GetItemPtr(1);
				SetCurrentVolume(oneSpec->vRefNum);
				SetCurrentDirectory(oneSpec->parID);
			}
//			sTheDLUserItemUPP = nil;  // indicates disposal of the UPP, not functionally necessary
			return sfHookNullEvent;
		break;
		case sfItemCancelButton:
			sTheFSpecs->RemoveItemsAt(sTheFSpecs->GetCount(), 1);	// remove all files from array
			FixButtons(theDialogP, ioParam);
			sReply.sfGood = false;
			return sfItemCancelButton;
		break;
		
		case kUpArrowPlusCO:
		case kDownArrowPlusCO:
			needToFixButtons = true;
		break;
		
		case sfHookGoToDesktop:
		case sfHookFolderPopUp:
		case sfHookOpenFolder:
			// NWF
			needToFixButtons = true;
			if (sAddFolders)
				sNeedToRebuild = true;
			return item;
		case sfItemVolumeUser:
			sNeedToRebuild = true;
			needToFixButtons = true;
		break;

		case kUserItem_FileList:
			FixButtons(theDialogP, ioParam);
			return sfHookSetActiveOffset + kUserItem_FileList;
		break;
		
		case sfHookOpenAlias:
			Boolean targetIsFolder,	dummy;
			FSSpec oldSpec;
			::FSMakeFSSpec(	sReply.sfFile.vRefNum,
							sReply.sfFile.parID,
							sReply.sfFile.name,
							&oldSpec);
			OSErr err;
			if (sResolveAliases) {
				err = ::ResolveAliasFile(&(sReply.sfFile), true, &targetIsFolder, &dummy);
				if (err != noErr) {
					if (sAddUnresolvedAliases) {
						::FSMakeFSSpec(	oldSpec.vRefNum,
								oldSpec.parID,
								oldSpec.name,
								&(sReply.sfFile));
					} else {
						return sfHookNullEvent;
					}
				} else {	// alias resolved successfully
					if (targetIsFolder && !sAddFolders) {	// if it's a folder, then don't add it - just open it
						needToFixButtons = true;
						sNeedToRebuild = true;
						return item;
					}
				}
			}
		case sfItemOpenButton:
		case kStdButton_Select:
			::LSetDrawingMode(false, sAddedListH);
			
			AddOneToList(sReply.sfFile.name, sReply.sfFile.parID, sReply.sfFile.vRefNum);
			
			::LSetDrawingMode(true, sAddedListH);
			InvalRect(&((**sAddedListH).rView));
			InvalRect(&((**((**sAddedListH).vScroll)).contrlRect));
			LUpdate((*sDialogP).visRgn, sAddedListH);
			if (GetEnabled(kDesktopButton)) {
				result = LGetSelect(true, &curSelCell, gToolboxList);
				if (result) {	// if there was something selected
					Boolean isLast = (curSelCell.v == (**gToolboxList).dataBounds.bottom - 1);
					if (sReply.sfFile.parID != fsRtParID) { // and it wasn't a volume
						LDelRow(1, curSelCell.v, gToolboxList);	// delete that cell
					} else {	// it was a volume, so we didn't delete it - increment the selection instead
						LSetSelect(false, curSelCell, gToolboxList);
						curSelCell.v++;
					}
					if (isLast)	// if the cell was the last one
						nextCell.v = (**gToolboxList).dataBounds.bottom - 1;
					else		// else it was not the last cell
						nextCell.v = curSelCell.v;

					nextCell.h = 0;
					LSetSelect(true, nextCell, gToolboxList);
					LAutoScroll(gToolboxList);
				}			
				needToFixButtons = true;	// if we FixButtons now, the Reply field will not have been changed yet.
											// this will delay things by one cycle.
				return sfHookNullEvent;
			} else return sfHookRebuildList;
		break;

		
		case kStdButton_Done:
			if (sAllowConversion)
				return sfItemOpenButton;	// nothing to do here; everything is in the FSSpec array
			else
				return sfItemCancelButton;	// since we're supressing checking, we fake a cancel messge
													// we'll know in the calling function that it was fake
													// because there will still be files in the array.
		break;
		case kStdButton_Remove:
			RemoveFromList(); 
			needToFixButtons = true;
			haveRebuilt = true;
			return sfHookRebuildList;
		break;
		case kStdButton_AddAll:
			HLock((Handle)sAddedListH);
			AddAllToList(!GetEnabled(kDesktopButton));	// if Desktop btn is enabled, we're not at the Desktop level
			HUnlock((Handle)sAddedListH);
			needToFixButtons = true;
			haveRebuilt = true;
			return sfHookRebuildList;
		break;
		case kStdButton_RemoveAll:
			RemoveAllFromList();
			needToFixButtons = true;
			haveRebuilt = true;
			return sfHookRebuildList;
		break;
		case kUserItem_AddedList:
			Point	theCell;
			theCell.v = theCell.h = 0;
			if ((sTheFSpecs->GetCount() != 0) && (LGetSelect(true, &theCell, sAddedListH))) {	
					// list is not empty & something is selected, so enable Remove & Remove All
				SetEnable(kStdButton_Remove, true);		// enable remove button
				SetEnable(kStdButton_RemoveAll, true);	// enable remove all button
			}
			FixButtons(theDialogP, ioParam);
			return sfHookSetActiveOffset + kUserItem_AddedList;
		break;
		case sfHookNullEvent:
				// Key actions have to be done first, then rebuilding the list if necessary,
				// and lastly, fixing button states, because that depends on the outcome of
				// the first operations.
			if (sKeyPressed > 0) {
				if (myData->whichFocused == kUserItem_AddedList) {
					DoListKey(sKeyPressed, sAddedListH);
				} else
					DoListKey(sKeyPressed, gToolboxList);
				sKeyPressed = 0;
			} else if (sNeedToRebuild) {
				sNeedToRebuild = false;
				haveRebuilt = true;
				return sfHookRebuildList;
			} else if (haveRebuilt) {
				haveRebuilt = false;
				EnsureSelection(myData->whichFocused);
			} else if (onceThrough && needToFixButtons) {
				if (!tabKludge) {
					FixButtons(theDialogP, ioParam);
					needToFixButtons = false;
				} else if (myData->whichFocused == kUserItem_FileList)
					tabKludge = false;	// for some reason, it seems to take an extra cycle
										// to update the Reply field after tabbing to the Toolbox List
			}
		break;
		default:
		break;
	}
	return item;
}
// --------------------------------------------------------------------------------------------
//	EnsureSelection
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::EnsureSelection(short inActiveList)
{
	ListHandle activeListH = (inActiveList == kUserItem_FileList) ? gToolboxList : sAddedListH;
	Cell theCell = {0, 0};
	if (!LGetSelect(true, &theCell, activeListH))
		LSetSelect(true, theCell, activeListH);
}

// --------------------------------------------------------------------------------------------
//	MyDrawListItem
// 		This routine was taken from the Apple Macintosh Developer Technical Support
//			Standard File Application, which I downloaded from Apple's web site, and modified.
// --------------------------------------------------------------------------------------------
pascal void UGetMultipleFiles::MyDrawListItem (WindowPtr theWindow, short item)
{
	Rect		itemRect;
	Handle		itemHandle;
	PenState	myPenState;			// current status of pen
	short		itemKind; // NWF

	if (item == kUserItem_AddedList) {
//		LUpdate((*theWindow).visRgn, sAddedListH);
		GetPenState(&myPenState);		// store pen state
		GetDialogItem((DialogPtr) theWindow, item, &itemKind, &itemHandle, &itemRect); // NWF
		PenSize(1, 1);					// set pen to 1 pixel
		InsetRect(&itemRect, -1, -1);	// adjust rectangle for framing
		FrameRect(&itemRect);			// draw border
		SetPenState(&myPenState);		// restore old pen state
	}
}

// --------------------------------------------------------------------------------------------
//	MyCreateTextListInDialog
// 		This routine was taken from Inside Macintosh:More Macintosh Toolbox,
//		List Manager chapter.
// --------------------------------------------------------------------------------------------
ListHandle UGetMultipleFiles::MyCreateTextListInDialog (DialogPtr myDialog, short myItemNumber)
{
	const short kTextLDEF = 0;	// resource ID of default LDEF

	Rect	myUserItemRect;	// enclosure of user item
	Handle	myUserItemHdl;	// for GetDialogItem
	short	itemKind; // NWF

	GetDialogItem(myDialog, myItemNumber, &itemKind,  // NWF
						&myUserItemHdl, &myUserItemRect);

	return MyCreateVerticallyScrollingList(myDialog, myUserItemRect, 1, kTextLDEF);	// TextLDEF will be replaced
}

// --------------------------------------------------------------------------------------------
//	MyCreateVerticallyScrollingList
// 		This routine was taken from Inside Macintosh:More Macintosh Toolbox,
//		List Manager chapter.
// --------------------------------------------------------------------------------------------
ListHandle UGetMultipleFiles::MyCreateVerticallyScrollingList (WindowPtr myWindow, Rect myRect,
											 short columnsInList, short myLDEF)
{
	const short kDoDraw = TRUE;			// always draw list after changes
	const short kNoGrow = FALSE;			// don't leave room for size box
	const short kIncludeScrollBar = TRUE;	// leave room for scroll bar
	const short kScrollBarWidth = 15;		// width of vertical scroll bar

	Rect	myDataBounds;			// initial dimensions of the list
	Point	myCellSize;				// size of each cell in list

	// specify dimensions of the list
	// start with a list that contains no rows
	SetRect(&myDataBounds, 0, 0, columnsInList, 0);


	// adjust the rectangle to leave room for the scroll bar
	myRect.right = myRect.right - kScrollBarWidth;

	// don't let the List Manager calculate the size of a cell
	SetPt(&myCellSize,  myRect.right - myRect.left, 18);

	// create the list
	return LNew(&myRect, &myDataBounds, myCellSize, myLDEF, myWindow,
			  kDoDraw, kNoGrow, !kIncludeScrollBar, kIncludeScrollBar);
}

// --------------------------------------------------------------------------------------------
//	InstallMyLDEF
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::InstallMyLDEF(ListHandle list, ListDefProcPtr proc)
{
	JmpBufHandle	ourLDEF;
#if powerc
	ListDefUPP	desc;
#endif
//	long curA5;

	if (nil == list){
		return;
	}

//	SysEnvirons(1, &gWorld);
//	curA5 = ::SetCurrentA5();
//		RememberA5();
	ourLDEF = (JmpBufHandle) NewHandleClear(sizeof(JmpBuf));

#if powerc
	desc = NewListDefProc(proc);
	BlockMove(desc, &(**ourLDEF).desc, sizeof(RoutineDescriptor));
	DisposeRoutineDescriptor(desc);
#else
	(**ourLDEF).op = JMPINSTRUCT;
#endif

	(**ourLDEF).addr = proc;
	(**list).listDefProc = (Handle) ourLDEF;
	::MoveHHi((Handle) ourLDEF);
	::HLock((Handle) ourLDEF);
//	FlushCache();
}

// --------------------------------------------------------------------------------------------
//	AddOneToList - Add the currently selected file to the list.  This can be done only because
//		the Standard File Package fills in the reply record before it calls the dialog hook
//		function (a point not well-documented in IM).  We made the "Open" button our "Add"
//		button so that this would occur for us.  (The dialog hook has extracted the fields
//		of the reply record for us already)
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::AddOneToList(Str255 name, long theDir, short theVRef)
{
	Point	theCell;
	FSSpec	theFSSpec;
	ArrayIndexT	foundAt;	// the index in the FSSpec array of this file, if present already
	ArrayIndexT	putAt;		// where in the FSSpec array to put this file
	char	theName[256];
	
	if (name[0] == 0)
		return;		// this means we got a null FSpec in the reply field.  Probably dbl-click
					// when adding the last item (button doesn't get disabled in time)
					
	
	::FSMakeFSSpec(theVRef, theDir, name, &theFSSpec);
	theCell.v = theCell.h = 0;
	
	// First, check to see if the file is already present in list	
	foundAt = sTheFSpecs->FetchIndexOf(&theFSSpec);
	if (foundAt == sTheFSpecs->index_Bad) {			// if it's not already present
		if (IsVisible(theFSSpec) || sShowInvisible) {
			putAt = sTheFSpecs->FetchInsertIndexOf(&theFSSpec);		// figure out where to put it, based on the FSSpec array
			sTheFSpecs->InsertItemsAt(1, putAt, &theFSSpec);		// insert it into the FSSpec array
																	// set the text of this new row to be the name of the file
			theCell.v = ::LAddRow(1, (short) putAt-1, sAddedListH);	// insert a blank row into the displayed list for this file
			LString::CopyPStr((unsigned char *)theFSSpec.name, (unsigned char *)theName);
			if (!IsFile(theFSSpec)) {	// it's a folder or a volume
				if (theFSSpec.parID != fsRtParID)	// if it's a folder (not a volume)
					LString::AppendPStr((unsigned char *)theName, "\p:", 256);
				else {
					char temp[256];
					LString::CopyPStr("\p[", (unsigned char *)temp, 256);
					LString::AppendPStr((unsigned char *)temp, (unsigned char *)theName, 256);
					LString::AppendPStr((unsigned char *)temp, "\p]", 256);
				 	LString::CopyPStr((unsigned char *)temp, (unsigned char *)theName, 256);
				}
			}

			ListItemHdl		lih = nil;
			long	cellData;

			// allocate a list element
			lih = (ListItemHdl)NewHandleClear(sizeof(ListItem));
			HLock((Handle)lih);
			
			// set up the cell data
			LString::CopyPStr((unsigned char *)theName, (unsigned char *)(*lih)->name, 256);
			LString::CopyPStr((unsigned char *)theFSSpec.name, (unsigned char *)((*lih)->theSpec.name), 256);
			(*lih)->theSpec.vRefNum = theFSSpec.vRefNum;
			(*lih)->theSpec.parID = theFSSpec.parID;
			
			cellData = (long)lih;
			
			// set the cell data
			LSetCell(&cellData, sizeof(long), theCell, sAddedListH);

			HUnlock((Handle)lih);
		}
	}
	// set the button states
	theCell.v = theCell.h = 0;
	if ((sTheFSpecs->GetCount() != 0) && (LGetSelect(true, &theCell, sAddedListH))) {
					// list is not empty & something is selected, so enable Remove
		SetEnable(kStdButton_Remove, true);		// enable remove button
	}
	if (sTheFSpecs->GetCount() != 0) {
					// list is not empty, so enable Remove All
		SetEnable(kStdButton_RemoveAll, true);	// enable remove all button
	}
}

// --------------------------------------------------------------------------------------------
//	RemoveFromList - Remove all files in the current selection from the list, leaving the
//		selection on the item just after the last one that was deleted, or at the end of the
//		remaining list if the bottom cell was selected and removed.
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::RemoveFromList()
{
	Point	theCell;
	short	lastCell;

	theCell.v = theCell.h = 0;
	while (LGetSelect(true, &theCell, sAddedListH)) {	// for each selected cell
		lastCell = theCell.v;
		(theCell.v)++;
	}						// lastCell is now the cell number of the last selected cell in the list
	theCell.v = theCell.h = 0;
	while (LGetSelect(true, &theCell, sAddedListH)) {	// for each selected cell
		sTheFSpecs->RemoveItemsAt(1, theCell.v + 1);	// remove this item from the FSSpec array
			// remove this item from the displayed list:
		// First get the item, so we can dispose the handle it points to:
		long	cellData;
		ListItemHdl		lih = nil;
		short dataLen = sizeof(long);
		::LGetCell((Ptr)(&cellData), &dataLen, theCell, sAddedListH);
		lih = (ListItemHdl)cellData;
		::DisposeHandle((Handle)lih);

		::LDelRow(1,  theCell.v, sAddedListH);			// remove this item from the displayed list
		lastCell--;
	}
	if (sTheFSpecs->GetCount() == 0) {	// list is empty, so disable Remove & Remove All
		SetEnable(kStdButton_Remove, false);		// disable remove button
		SetEnable(kStdButton_RemoveAll, false);		// disable remove all button
	} else {		// set selection at the cell that was below the last one deleted, or
					// on the last one remaining in the list
		theCell.v = lastCell + 1;
		LSetSelect(true, theCell, sAddedListH);
	}
}

// --------------------------------------------------------------------------------------------
//	AddAllToList - Add all items in the current directory to the list.
//		This has to be accomplished in a roundabout way, because we can't directly get a list
//		of the files showing in the SF list.  So instead, we look at each file in the current
//		directory, check it against out typelist, and add it if it has a good type.
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::AddAllToList(Boolean  atDesktop )
{
	Boolean	typeFound;
	Rect	theBounds;
	FSSpec		theFSpec;
	Boolean		IsAFile;
	Point	theCell;
	short	theOffset;
	short	theLength;
	theCell.v = theCell.h = 0;
	DataHandle	dh;
	DataPtr		dp;
	Int8		handleState;
	ToolListItemPtr theRealData;
	theBounds = (*gToolboxList)->dataBounds;
	if (atDesktop) theBounds.bottom--;  //eliminate the Trash
	dh = (*gToolboxList)->cells;
	handleState = ::HGetState((Handle)dh);
	::HLock((Handle)dh);
	::LSetDrawingMode(false, sAddedListH);
	for (theCell.v = 0; theCell.v < theBounds.bottom; theCell.v++){
	// using this way because using LGetCell got me some blanks
		::LGetCellDataLocation(&theOffset,&theLength,theCell,gToolboxList);
		if (theLength < 0) theLength = sizeof(ToolListItem); // sometimes it is negative ( a toolbox bug?)
		if (theLength > sizeof(ToolListItem))  theLength = sizeof(ToolListItem); // sometimes it is huge
		if (theOffset >= 0){
			dp = *dh;
			dp +=theOffset;
			theRealData = (ToolListItemPtr)dp;


			::FSMakeFSSpec(	theRealData->vRefNum,
							theRealData->dirID,
							theRealData->name,
							&theFSpec);
			if (sResolveAliases && ((theRealData->fndrFlags & kIsAlias) != 0)) {
				Boolean targetIsFolder,	dummy;
				OSErr err;
				err = ::ResolveAliasFile(&theFSpec, true, &targetIsFolder, &dummy);
				if (err != noErr) {
					if (sAddUnresolvedAliases) {
						::FSMakeFSSpec(	theRealData->vRefNum,
							theRealData->dirID,
							theRealData->name,
							&theFSpec);
					} else {
						continue;	// do next cell
					}
				}
				IsAFile = !targetIsFolder;
				typeFound = TypeInList(GetType(theFSpec));
			} else {
				IsAFile = (theRealData->creator > 0);
				typeFound = TypeInList(theRealData->type);
			}
								// if theFSSpec isn't in the exclusion list, or there is no list
			if (sNixFiles == nil || (sNixFiles->FetchIndexOf(&theFSpec) == sNixFiles->index_Bad)) {
				if (IsAFile || sAddFolders) {	// if it's a file, or we are adding folders
					if (!IsAFile || (typeFound) || (sNumTypes == -1)) {
					
						AddOneToList(theFSpec.name, theFSpec.parID, theFSpec.vRefNum);
					}
				}
			}
		}
	}
	::HSetState((Handle)dh,handleState);
	::LSetDrawingMode(true, sAddedListH);
	InvalRect(&((**sAddedListH).rView));
	InvalRect(&((**((**sAddedListH).vScroll)).contrlRect));
	LUpdate((*sDialogP).visRgn, sAddedListH);

}

// --------------------------------------------------------------------------------------------
//	RemoveAllFromList - Remove all files from our list.
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::RemoveAllFromList()
{
	Point	theCell;

	theCell.v = theCell.h = 0;
	
	while (sTheFSpecs->GetCount() != 0) {		// as long as there's a cell left
		theCell.v = sTheFSpecs->GetCount() - 1;			// pick the last cell
		sTheFSpecs->RemoveItemsAt(1, theCell.v + 1);	// remove this thing from the FSSpec array

			// remove this item from the displayed list:
		// First get the item, so we can dispose the handle it points to:
		long	cellData;
		ListItemHdl		lih = nil;
		short dataLen = sizeof(long);
		::LGetCell((Ptr)(&cellData), &dataLen, theCell, sAddedListH);
		// typecast our refCon
		lih = (ListItemHdl)cellData;
		::DisposeHandle((Handle)lih);

		::LDelRow(1,  theCell.v, sAddedListH);			// remove this item from the displayed list
	}
	if (sTheFSpecs->GetCount() == 0) {	// list is empty, so disable Remove & Remove All
		SetEnable(kStdButton_Remove, false);		// disable remove button
		SetEnable(kStdButton_RemoveAll, false);		// disable remove all button
	} else {		// set selection on the last one remaining in the list
		theCell.v = sTheFSpecs->GetCount() - 1;	//  (we should never be here)
		LSetSelect(true, theCell, sAddedListH);
	}
}

// --------------------------------------------------------------------------------------------
//	Utility functions
// --------------------------------------------------------------------------------------------
long UGetMultipleFiles::GetSFCurDir(void)
{
	return *((long *)0x398);
}

short UGetMultipleFiles::GetSFVRefNum(void)
{
	return -1 * (*((short *)0x214));
}

void UGetMultipleFiles::SetCurrentVolume(short vRefNum)
{
	(*((short *)0x214)) = -vRefNum;
}

void UGetMultipleFiles::SetCurrentDirectory(long dirID)
{
	*((long *)0x398) = dirID;
}

void UGetMultipleFiles::SetEnable(ResIDT theItem, Boolean enabled)
{
	Rect	itemRect;
	Handle	itemHandle;
	short	itemKind; // NWF

	GetDialogItem(sDialogP, theItem, &itemKind, &itemHandle, &itemRect); // NWF

	if (enabled)
		HiliteControl((ControlHandle)itemHandle, 0);
	else
		HiliteControl((ControlHandle)itemHandle, 255);
}

Boolean UGetMultipleFiles::GetEnabled(ResIDT theItem)
{
	Rect	itemRect;
	Handle	itemHandle;
	short	itemKind; // NWF

	GetDialogItem(sDialogP, theItem, &itemKind, &itemHandle, &itemRect); // NWF

	return ((**((ControlHandle)itemHandle)).contrlHilite < kControlDisabledPart);
}

// --------------------------------------------------------------------------------------------
//	fixButtons - Sets enabled/disabled states, and default, based on selections & lists
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::FixButtons (DialogPtr theDialogP, void *ioParam)
{
	DialogPeek theDP = (DialogPeek) theDialogP;
	Boolean			topEmpty = ((**gToolboxList).dataBounds.bottom == 0);
	Boolean			botEmpty = (sTheFSpecs->GetCount() == 0);
	Rect			itemRect;
	Handle			itemHandle;
	RGBColor		bgColor, fgSaveColor;
	PenState		curPen;
	short			dataSize = sizeof(Str63);
	Cell			curSelCell = {0,0};
	Boolean			toolboxActive = ((dataHolderType *)ioParam)->whichFocused == kUserItem_FileList;
	short			itemKind; // NWF
	
	SetEnable(kStdButton_RemoveAll, !botEmpty);	// enable remove all button if added list isn't empty
	SetEnable(kStdButton_Remove, !botEmpty && !toolboxActive);	// enable remove button if added list is active and non-empty
	SetEnable(kStdButton_AddAll, !topEmpty);	// enable add all if top list isn't empty
	if (sAddFolders)
		SetEnable(kStdButton_Select, (!topEmpty) && toolboxActive);	// enable add all if top list is active and non-empty
	SetEnable(kStdButton_Add, (!topEmpty) && toolboxActive);	// enable add all if top list is active and non-empty

	if ( sAddFolders && toolboxActive) {
		// set text for folder button:
		GetDialogItem(theDialogP, kStdButton_Select, &itemKind, &itemHandle, &itemRect); // NWF
		if (topEmpty) {	// if there are no entries in the top list
			::SetControlTitle((ControlHandle) itemHandle, "\pNothing to Add");
			::ValidRect(&itemRect);
		} else {
			SetButtonTitle(itemHandle, sReply.sfFile.name, itemRect);
		}
	}
	// if top list is empty, and bottom list isn't, then set default button to Done
	if (topEmpty && !botEmpty) {
		if (theDP->aDefItem != kStdButton_Done) {
			theDP->aDefItem = kStdButton_Done;
			GetDialogItem(theDialogP, kStdButton_Done, &itemKind, &itemHandle, &itemRect); // NWF
			InsetRect(&itemRect, -4, -4);
			InvalRect(&itemRect);
			GetDialogItem(theDialogP, sfItemOpenButton, &itemKind, &itemHandle, &itemRect); // NWF
			InsetRect(&itemRect, -4, -4);
			GetPenState(&curPen);
			PenSize(3, 3);
			GetBackColor(&bgColor);
			GetForeColor(&fgSaveColor);
			RGBForeColor(&bgColor);
			FrameRoundRect(&itemRect, 16, 16);
			RGBForeColor(&fgSaveColor);
			SetPenState(&curPen);
		}
	} else {
		if (theDP->aDefItem != sfItemOpenButton) {
			theDP->aDefItem = sfItemOpenButton;
			GetDialogItem(theDialogP, sfItemOpenButton, &itemKind, &itemHandle, &itemRect); // NWF
			InsetRect(&itemRect, -4, -4);
			InvalRect(&itemRect);
			GetDialogItem(theDialogP, kStdButton_Done, &itemKind, &itemHandle, &itemRect); // NWF
			InsetRect(&itemRect, -4, -4);
			GetPenState(&curPen);
			PenSize(3, 3);
			GetBackColor(&bgColor);
			GetForeColor(&fgSaveColor);
			RGBForeColor(&bgColor);
			FrameRoundRect(&itemRect, 16, 16);
			RGBForeColor(&fgSaveColor);
			SetPenState(&curPen);
		}
	}
	InvalRect(&((**sAddedListH).rView));
	InvalRect(&((**((**sAddedListH).vScroll)).contrlRect));
//	LUpdate((*sDialogP).visRgn, sAddedListH);
}

// Utility by TE

Boolean UGetMultipleFiles::TypeInList( OSType inType )
{
	if( sNumTypes == -1 ) 
		return true;
		
	for (short j=0; j <= sNumTypes-1; j++) 
	{	// for each type in the list:
		if (inType == sTypeList[j])	// is this file's type one in the list?
				return true;
	}
	return false;
}

// Taken from IM:Files pg 3-35
void UGetMultipleFiles::SetButtonTitle (Handle ButtonHdl, Str255 name, Rect ButtonRect)
{
	short	result, width;
	LStr255 finalStr;
	
	width = (ButtonRect.right - ButtonRect.left) - (StringWidth(kFolderButtonRoot) + CharWidth(' '));
	result = TruncString(width, name, smTruncMiddle);
	finalStr = kFolderButtonRoot;
	finalStr.Append(name);
	finalStr.Append('"');
	::SetControlTitle((ControlHandle) ButtonHdl, finalStr);
	::ValidRect(&ButtonRect);
}

void UGetMultipleFiles::ReplaceString ( Str255 destStr, Str255 whatStr, Str255 byStr ) 
{
	Handle	tH;
	long		newOffset;
	short		len;
	
	tH = NewHandle(512L);
	BlockMove((Ptr) destStr, *tH, destStr[0]+1);
	newOffset = Munger(tH, 0L, &whatStr[1], whatStr[0], &byStr[1], byStr[0]);
	if (newOffset > 0) {
		len = destStr[0];
		BlockMove(*tH, (Ptr) destStr, 255);
		len = len + byStr[0] - whatStr[0];
		if (len > 255)
			len = 255;
		destStr[0] = len;
	}
	DisposeHandle(tH);
}

void UGetMultipleFiles::DoListKey(short theKey, ListHandle theListH)
{
	Cell theCell = {0,0};
	short numVisible = (**theListH).visible.bottom - (**theListH).visible.top;
	
	switch (theKey) {
		case kHome:
			if (LGetSelect(true, &theCell, theListH)) {
				LSetSelect(false, theCell, theListH);
				theCell.v = 0;
				LSetSelect(true, theCell, theListH);
				LAutoScroll(theListH);
			}
		break;
		case kEnd:
			if (LGetSelect(true, &theCell, theListH)) {
				LSetSelect(false, theCell, theListH);
				theCell.v = (**theListH).dataBounds.bottom-1;
				LSetSelect(true, theCell, theListH);
				LAutoScroll(theListH);
			}
		break;
		case kUpArrow:
			if (theListH != gToolboxList) {
				if (LGetSelect(true, &theCell, theListH)) {
					LSetSelect(false, theCell, theListH);
					if (theCell.v > 0)
						(theCell.v)--;
					LSetSelect(true, theCell, theListH);
					LAutoScroll(theListH);
				}
			}
		break;
		case kDownArrow:
			if (theListH != gToolboxList) {
				if (LGetSelect(true, &theCell, theListH)) {
					LSetSelect(false, theCell, theListH);
					if (theCell.v < (**theListH).dataBounds.bottom-1)
						(theCell.v)++;
					LSetSelect(true, theCell, theListH);
					LAutoScroll(theListH);
				}
			}
		break;
		case kPageUp:
			if (LGetSelect(true, &theCell, theListH)) {
				LSetSelect(false, theCell, theListH);
				if (theCell.v > numVisible-1)
					theCell.v -= numVisible;
				else
					theCell.v = 0;
				LSetSelect(true, theCell, theListH);
				LAutoScroll(theListH);
			}
		break;
		case kPageDown:
			if (LGetSelect(true, &theCell, theListH)) {
				LSetSelect(false, theCell, theListH);
				if (theCell.v < (**theListH).dataBounds.bottom - numVisible)
					theCell.v += numVisible;
				else
					theCell.v = (**theListH).dataBounds.bottom-1;
				LSetSelect(true, theCell, theListH);
				LAutoScroll(theListH);
			}
		break;
	}
	if (theListH == gToolboxList)
		needToFixButtons = true;
}

Boolean UGetMultipleFiles::IsFile(FSSpec &inFile)
{
	CInfoPBRec pb;
	OSErr error;
	
	error = GetCatInfoNoName(inFile.vRefNum, inFile.parID, inFile.name, &pb);
	if ( error == noErr )
		if ( (pb.dirInfo.ioFlAttrib & ioDirMask) != 0 )
			return false;
	return true;
	//handle error in GetCatInfoNoName here FIX
}

Boolean UGetMultipleFiles::IsVisible(FSSpec &inFile)
{
	FInfo theInfo;
	OSErr error;
	
	error = FSpGetFInfo(&inFile, &theInfo);
	if ( error == noErr )
		if ( (theInfo.fdFlags & kIsInvisible) != 0 )
			return false;
	return true;
}

Boolean UGetMultipleFiles::IsAlias(FSSpec &inFile)
{
	FInfo theInfo;
	OSErr error;
	
	error = FSpGetFInfo(&inFile, &theInfo);
	if ( error == noErr )
		if ( (theInfo.fdFlags & kIsAlias) != 0 )
			return true;
	return false;
}

OSType UGetMultipleFiles::GetType(FSSpec &inFile)
{
	FInfo theInfo;
	OSErr error;
	
	error = FSpGetFInfo(&inFile, &theInfo);
	return theInfo.fdType;
}

// --------------------------------------------------------------------------------------------
//	GetCatInfoNoName - Taken from MoreFiles 1.4.5
// --------------------------------------------------------------------------------------------
OSErr UGetMultipleFiles::GetCatInfoNoName(short vRefNum,
							   long dirID,
							   ConstStr255Param name,
							   CInfoPBPtr pb)
{
	Str31 tempName;
	OSErr error;
	
	/* Protection against File Sharing problem */
	if ( (name == NULL) || (name[0] == 0) )
	{
		tempName[0] = 0;
		pb->dirInfo.ioNamePtr = tempName;
		pb->dirInfo.ioFDirIndex = -1;	/* use ioDirID */
	}
	else
	{
		pb->dirInfo.ioNamePtr = (StringPtr)name;
		pb->dirInfo.ioFDirIndex = 0;	/* use ioNamePtr and ioDirID */
	}
	pb->dirInfo.ioVRefNum = vRefNum;
	pb->dirInfo.ioDrDirID = dirID;
	error = PBGetCatInfoSync(pb);
	pb->dirInfo.ioNamePtr = NULL;
	return ( error );
}

// --------------------------------------------------------------------------------------------
//	MyLDEF
// --------------------------------------------------------------------------------------------
pascal void UGetMultipleFiles::MyLDEF(short lMessage, Boolean lSelect,	Rect *lRect, Cell lCell,
	short lDataOffset, short lDataLen, ListHandle lHandle)
{
#pragma unused(lDataOffset)
#pragma unused(lDataLen)
	try{
		switch(lMessage) {
			case lInitMsg:
				break;
			
			case lDrawMsg:
				DrawMsg(lSelect, lRect, lCell, lHandle);
				break;
			
			case lHiliteMsg:
				HiliteMsg(lSelect, lRect);
				break;
				
			case lCloseMsg:
				break;

			default:
				break;
		}
	} catch (...) {
		::SysBeep(9);	// shouldn't get here, I hope
	}
}

// --------------------------------------------------------------------------------------------
//	DrawMsg
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::DrawMsg(Boolean fSelect, Rect *r, Cell cell, ListHandle lh)
{
	long	cellData;
	short		dataLen;

	// get cell	
	dataLen = (short)sizeof(cellData);
	LGetCell((Ptr)(&cellData), &dataLen, cell, lh);
	
	ListItemHdl	lih = nil;
	lih = (ListItemHdl)cellData;

	ListElementProc(r, cell, lh, lih);
	
	// hilite if selected
	if (fSelect)
		HiliteMsg(fSelect, r);
}

// --------------------------------------------------------------------------------------------
//	HiliteMsg
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::HiliteMsg(Boolean fSelect, Rect *r)
{
#pragma unused(fSelect)
	unsigned char	hMode;

	hMode = LMGetHiliteMode();
	BitClr((Ptr)(&hMode),(long)pHiliteBit);
	LMSetHiliteMode(hMode);
	InvertRect(r);
}

// --------------------------------------------------------------------------------------------
//	ListElementProc
// --------------------------------------------------------------------------------------------
void UGetMultipleFiles::ListElementProc(Rect *cellRect, Cell lCell, ListHandle theList,
					ListItemHdl lih)
{
#pragma unused(lCell)
	try {
		GrafPtr			savePort;
		short leftDraw,topDraw;					/* left/top offsets from topleft of cell */
		FontInfo fontInfo;						/* font information (ascent/descent/etc) */
		
		// set up the port
		GetPort(&savePort);
		SetPort((**theList).port);
		PenNormal();
		
		EraseRect(cellRect);

		// determine starting point for drawing
		
		leftDraw =	cellRect->left  + kLeftOffset;
		topDraw =	cellRect->top + kTopOffset;
	  		
		Rect destRect;
		SetRect(&destRect,leftDraw,topDraw,leftDraw + 16,topDraw + 16);

		StHandleLocker theLocker((Handle)lih);
		DrawFileIcon(&((*lih)->theSpec), &destRect);
		leftDraw += 16+kIconSpace;
		
		StTextState theSaver;
		SetPort(savePort);
		Str255 theString;
		GetFontInfo(&fontInfo);
		MoveTo(leftDraw,topDraw+fontInfo.ascent);
		LString::CopyPStr((**lih).name,theString);
		// set condensed mode if necessary (if the text doesn't fit otherwise)
		TextFace(0);
		if (IsAlias((*lih)->theSpec))
			TextFace(italic);
		if (StringWidth(theString) > (cellRect->right - leftDraw)) {
			TextFace(condense);
					// TruncString will do nothing if theString is already short
					// enough so no need for an if statement
			TruncString((cellRect->right - leftDraw),theString,truncEnd);
		}
		ClipRect(cellRect);
		DrawString(theString);
	} catch (...) {
		::SysBeep(9);	// shouldn't get here, I hope
	}
}

/***************************************************************************

	DrawFileIcon
		
***************************************************************************/

OSErr UGetMultipleFiles::DrawFileIcon(FSSpec *spec, Rect *drawRect)
{
	OSErr 	err = noErr;
	Handle	theSuite = NULL;
	DTPBRec	pb;
	CInfoPBRec		Cpb;
	short	ioDTRefNum;
	FInfo	fndrInfo;
	Handle	theIconData = NULL;
	
	/* get desktop database reference number */
	pb.ioNamePtr = NULL;
	pb.ioVRefNum = -1; // spec->vRefNum;
	err = PBDTGetPath(&pb);
	try {
		ioDTRefNum = pb.ioDTRefNum;

		/* create an icon suite to store icons in */
		err = NewIconSuite(&theSuite);
		if (err != noErr) throw IconErr();
		
		/* get file type/creator */
		err = FSpGetFInfo(spec, &fndrInfo);
		
		/* get small bw icon */
		theIconData = NewHandle(kSmallIconSize);
		if (theIconData) {
			HLock(theIconData);
			pb.ioCompletion = NULL;
			pb.ioDTRefNum = ioDTRefNum;
			pb.ioDTBuffer = *theIconData;
			pb.ioDTReqCount = kSmallIconSize;
			pb.ioIconType = kSmallIcon;
			pb.ioFileCreator = fndrInfo.fdCreator;
			pb.ioFileType = fndrInfo.fdType;
			err = PBDTGetIconSync(&pb);
			if (err == noErr) {
				HUnlock(theIconData);
				err = AddIconToSuite(theIconData, theSuite, small1BitMask);
			} else {
				DisposeHandle(theIconData);
				theIconData = NULL;
			}
			if (err != noErr) throw IconErr();
		}

		/* get small 4 bit icon */
		theIconData = NewHandle(kSmall4BitIconSize);
		if (theIconData) {
			HLock(theIconData);
			pb.ioCompletion = NULL;
			pb.ioDTRefNum = ioDTRefNum;
			pb.ioDTBuffer = *theIconData;
			pb.ioDTReqCount = kSmall4BitIconSize;
			pb.ioIconType = kSmall4BitIcon;
			pb.ioFileCreator = fndrInfo.fdCreator;
			pb.ioFileType = fndrInfo.fdType;
			err = PBDTGetIconSync(&pb);
			if (err == noErr) {
				HUnlock(theIconData);
				err = AddIconToSuite(theIconData, theSuite, small4BitData);
			} else {
				DisposeHandle(theIconData);
				theIconData = NULL;
			}
			if (err != noErr) throw IconErr();
		}

		/* get small 8 bit icon */
		theIconData = NewHandle(kSmall8BitIconSize);
		if (theIconData) {
			HLock(theIconData);
			pb.ioCompletion = NULL;
			pb.ioDTRefNum = ioDTRefNum;
			pb.ioDTBuffer = *theIconData;
			pb.ioDTReqCount = kSmall8BitIconSize;
			pb.ioIconType = kSmall8BitIcon;
			pb.ioFileCreator = fndrInfo.fdCreator;
			pb.ioFileType = fndrInfo.fdType;
			err = PBDTGetIconSync(&pb);
			if (err == noErr) {
				HUnlock(theIconData);
				err = AddIconToSuite(theIconData, theSuite, small8BitData);
			} else {
				DisposeHandle(theIconData);
				theIconData = NULL;
			}
			if (err != noErr) throw IconErr();
		}

		/* plot the icons */
		ClipRect(drawRect);
		err = PlotIconSuite(drawRect, atNone, ttNone, theSuite);
		if (err != noErr) throw IconErr();
	} catch (UGetMultipleFiles::IconErr) {
		// there was some problem in getting the various icons or plotting them,
		//	 so we'll try using the system icon suite:

		// get the file information for the item
		Cpb.hFileInfo.ioNamePtr = spec->name;
		Cpb.hFileInfo.ioVRefNum = spec->vRefNum;
		Cpb.hFileInfo.ioDirID = spec->parID;
		Cpb.hFileInfo.ioFDirIndex = 0;
		ThrowIfOSErr_(::PBGetCatInfoSync(&Cpb));

		SInt16 iconID;		
		if (spec->parID == fsRtParID)	// it's a volume
			iconID = kGenericHardDiskIconResource;	// we should really distinguish between hard disks & floppies, but I don't feel like figuring it out right now.
		else if (Cpb.hFileInfo.ioFlAttrib & ioDirMask)	// we have a folder
			iconID = kGenericFolderIconResource;
		else if (fndrInfo.fdType == 'APPL')
			iconID = kGenericApplicationIconResource;
		else
			iconID = kGenericDocumentIconResource;

		// dispose the suite if need be first, because GetIconSuite allocates its own
		if (theSuite != nil) {
			DisposeIconSuite(theSuite, true);
			theSuite = nil;
		}
		ThrowIfOSErr_(::GetIconSuite(&theSuite, iconID, svAllSmallData));
		
		/* plot the icon */
		ClipRect(drawRect);
		ThrowIfOSErr_(::PlotIconSuite(drawRect, atNone, ttNone, theSuite));
	}
	// dispose the suite if need be
	if (theSuite != nil)
		DisposeIconSuite(theSuite, true);
	return err;
}

#pragma mark ========== Navigation Services Version ========

void
UGetMultipleFiles::NSGetFiles(Str255 prompt, FSSpecPtr inStartLoc)
{
	OSErr err = noErr;
	NavReplyRecord		theReply;
	NavEventUPP			navEventFilterUPP;
 	NavDialogOptions	dlgOptions;
	NavObjectFilterUPP	navFileFilterUPP;
	NavTypeListHandle	NSTypeListH = nil;
	AEKeyword			theAEKeyword;
	AEDesc				theAEDesc;
	Str255				myAppName;
    OSType				myAppSignature;
	ProcessInfoRec		theInfo;
	ProcessSerialNumber	thePSN;
	long				numChosen;
	
	// Retrieve the application name & sig
	thePSN.highLongOfPSN = kNoProcess;
	thePSN.lowLongOfPSN  = kNoProcess;
	::GetCurrentProcess(&thePSN);

	theInfo.processInfoLength = sizeof(ProcessInfoRec);
	theInfo.processName = (StringPtr) &myAppName;
	theInfo.processAppSpec = NULL;
	::GetProcessInformation(&thePSN, &theInfo);
	myAppSignature = theInfo.processSignature;

	// Set up options
	err = ::NavGetDefaultDialogOptions(&dlgOptions);
	dlgOptions.dialogOptionFlags |= kNavAllowMultipleFiles;
	dlgOptions.location = kDialogLocation;
	dlgOptions.dialogOptionFlags |= kNavDontAutoTranslate;
	dlgOptions.dialogOptionFlags |= kNavSelectAllReadableItem;
	if (sShowInvisible)
		dlgOptions.dialogOptionFlags |= kNavAllowInvisibleFiles;
	else
		dlgOptions.dialogOptionFlags &= ~kNavAllowInvisibleFiles;

	if (sAllowConversion) {
		dlgOptions.dialogOptionFlags &= ~kNavDontAddTranslateItems;
	} else {
		dlgOptions.dialogOptionFlags |= kNavDontAddTranslateItems;
	}
	if (sSuppressTypePopup) {
		dlgOptions.dialogOptionFlags |= (kNavNoTypePopup | kNavAllFilesInPopup | kNavSelectAllReadableItem);
	} else {
		dlgOptions.dialogOptionFlags &= ~kNavNoTypePopup;
	}
	
    LString::CopyPStr(prompt, dlgOptions.message);
    LString::CopyPStr(myAppName, dlgOptions.clientName);

	// Create the list of types we want to filter on
	if (sNumTypes > 0 && !sSuppressTypePopup) {
		NSTypeListH = (NavTypeListHandle) NewHandle(sizeof(NavTypeList) + sNumTypes * sizeof(OSType));
		if (NSTypeListH != NULL) {
			(*NSTypeListH)->componentSignature = myAppSignature;
			(*NSTypeListH)->osTypeCount = sNumTypes;
			BlockMoveData(sTypeList, (*NSTypeListH)->osType, sNumTypes * sizeof(OSType));
			HLock((Handle) NSTypeListH);
		}
	}

	// Set Up PP Event Handler
	navEventFilterUPP = NULL;
	navEventFilterUPP = NewNavEventProc(NSEventFilterForPP);

	// Set Up File Filter
	navFileFilterUPP = NULL;
	navFileFilterUPP = NewNavObjectFilterProc(NSFileFilter);

	err = ::NavGetFile(	nil,
					&theReply,
					&dlgOptions,
					navEventFilterUPP,
					nil,
					navFileFilterUPP,
					NSTypeListH,
					nil);


	// If they selected a file, open it
	if (err == noErr && theReply.validRecord) {
		sReply.sfGood         = true;
		sReply.sfReplacing    = false;
		sReply.sfScript       = smSystemScript;
		sReply.sfIsFolder     = false;
		sReply.sfIsVolume     = false;
		sReply.sfReserved1    = 0;
		sReply.sfReserved2    = 0;
		// Count the number of items selected
		ThrowIfOSErr_(err = AECountItems(&theReply.selection, &numChosen));
		for (short i = 1; i<= numChosen; i++) {
			FSSpec	TempFSSpec;
			ThrowIfOSErr_(err = AEGetNthDesc(&theReply.selection, i, typeFSS, &theAEKeyword, &theAEDesc));
			BlockMove(*(theAEDesc.dataHandle), &TempFSSpec, sizeof(FSSpec));
			sTheFSpecs->AddItem(&TempFSSpec);
		}
	}

	// Clean Up
	ThrowIfOSErr_(err = NavDisposeReply(&theReply));

	if (NSTypeListH != nil)
		DisposeHandle((Handle) NSTypeListH);

    if (navFileFilterUPP != nil)
        DisposeRoutineDescriptor(navFileFilterUPP);

    if (navEventFilterUPP != nil)
        DisposeRoutineDescriptor(navEventFilterUPP);
}

#include <LEventDispatcher.h>
#include <LView.h>
#include <LPeriodical.h>
//=============================================================================
//      CNavServices::NSEventFilterForPP : Nav Services->PP event filter.
//		Taken (and altered a bit?) from Dair Grant's CNavServices PP class
//-----------------------------------------------------------------------------
pascal void
UGetMultipleFiles::NSEventFilterForPP(NavEventCallbackMessage callBackSelector,
											NavCBRecPtr			callBackParms,
											NavCallBackUserData	callBackUD)
{
	LEventDispatcher	*theDispatcher;
	EventRecord			*theEvent;
	StGrafPortSaver		savePort;



	// Handle the event
	switch (callBackSelector) {
		// Process Mac events
		case kNavCBEvent:
		// Retrieve the Mac event and current dispatcher
		theEvent  = (EventRecord *) callBackParms->eventData.event;
		theDispatcher = LEventDispatcher::GetCurrentEventDispatcher();


if (theDispatcher) {
		// Dispatch the event (we only need to handle update and idle events)
		LView::OutOfFocus(nil);
		switch (callBackParms->eventData.event->what) {
			case updateEvt:
				theDispatcher->DispatchEvent(*theEvent);
			break;

			case nullEvent:
				theDispatcher->UseIdleTime(*theEvent);
			break;

			default:
			break;
		}
}


		// Repeaters get time after every event
		LPeriodical::DevoteTimeToRepeaters(*theEvent);
		break;


		default:
		break;
	}
}

pascal Boolean
UGetMultipleFiles::NSFileFilter(AEDesc *theItem,
								void *theInfo,
								NavCallBackUserData callBackUD,
								NavFilterModes filterMode)
{
	if (filterMode != kNavFilteringBrowserList) return true;

	Boolean				shouldFilter;
	CInfoPBRec			stdFileInfo;
	NavFileOrFolderInfo	*navFileInfo;
	FSSpec				theFSSpec;

	navFileInfo = (NavFileOrFolderInfo *) theInfo;
	if (navFileInfo->isFolder || theItem->descriptorType != typeFSS)	// if it's a folder or volume
		return sShowFolders;
	
	// Obtain the information we need for the Std File filter
	BlockMove(*(theItem->dataHandle), &theFSSpec, sizeof(FSSpec));

	if (sNixFiles == nil)	// don't suppress any
		return true;
	// see if it's in the list of excluded files
	else if (sNixFiles->FetchIndexOf(&theFSSpec) != sNixFiles->index_Bad)	// if it is in the exclude list
		return false;			// then don't show it
	else if (TypeInList(navFileInfo->fileAndFolder.fileInfo.finderInfo.fdType)) // otherwise, show it if it's the right type
		return true;
	else
		return false;
}
