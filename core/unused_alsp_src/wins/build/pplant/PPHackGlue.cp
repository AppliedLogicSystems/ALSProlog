#include <LWindow.h>
#include <string.h>
//#include <stdlib.h>


#include "CALSPrologApp.h"
#include "CPrologListener.h"

#include "alspi.h"
#include "cinterf.h"

/*******************************************************************

LCommander class interface.

********************************************************************/

CPPI_CLASSDEF(class_LCommander, "LCommander", NULL, NULL, NULL)


/*******************************************************************

LApplication class interface.

********************************************************************/

/* I don't define any base classes for LApplication, because I'm
   only using Run. */

extern  ClassDefinition class_LApplication;

static int p_new_LApplication(const void *, const ClassDefinition *)
{
	PWord arg2; int type2;
	void *app;
	PWord obj; int objtype;	

    PI_getan(&arg2, &type2, 2);

	app = (void *) new CALSPrologApp();

	CPPI_make_object(&obj, &objtype, app, &class_LApplication);	
	
	if (PI_unify(arg2,type2,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int pLApplication_Run(const void *object, const ClassDefinition *class_def)
{
    if (!CPPI_cast_object(object, class_def, &object, &class_LApplication)) PI_FAIL;

	((LApplication *)object)->Run();

	PI_SUCCEED;
}

static int LApplicationCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	
	if (to_class == &class_LCommander) r = (LCommander *)(LApplication *)object;
	
	if (r) { *cast_obj = r; return 1;}
	else return 0;
}

CPPI_BEGMFUNC(mfunc_LApplication)
	CPPI_MEMBERFUNC("LApplication", p_new_LApplication),
	CPPI_MEMBERFUNC("Run", pLApplication_Run),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LApplication, "LApplication", NULL, LApplicationCaster, mfunc_LApplication)


/*******************************************************************

LPane class interface.

********************************************************************/

extern ClassDefinition class_LPane;

/* I don't define a allocator, because I don't need to directly create
   panes. */

static int pLPane_Show(const void *object, const ClassDefinition *class_def)
{
    if (!CPPI_cast_object(object, class_def, &object, &class_LPane)) PI_FAIL;

	((LPane *)object)->Show();

	PI_SUCCEED;
}

static int pLPane_Enable(const void *object, const ClassDefinition *class_def)
{
    if (!CPPI_cast_object(object, class_def, &object, &class_LPane)) PI_FAIL;

	((LPane *)object)->Enable();

	PI_SUCCEED;
}

static int pLPane_Disable(const void *object, const ClassDefinition *class_def)
{
    if (!CPPI_cast_object(object, class_def, &object, &class_LPane)) PI_FAIL;

	((LPane *)object)->Disable();

	PI_SUCCEED;
}

static int pLPane_SetDescriptor(const void *object, const ClassDefinition *class_def)
{
	PWord a3; int t3;

    if (!CPPI_cast_object(object, class_def, &object, &class_LPane)) PI_FAIL;
    
    PI_getan(&a3, &t3, 3);
   	if (t3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&a3,t3))
			PI_FAIL;

	((LPane *)object)->SetDescriptor((unsigned char *)a3);

	PI_SUCCEED;
}


CPPI_BEGMFUNC(mfunc_LPane)
	CPPI_MEMBERFUNC("Show", pLPane_Show),
	CPPI_MEMBERFUNC("Enable", pLPane_Enable),
	CPPI_MEMBERFUNC("Disable", pLPane_Disable),
	CPPI_MEMBERFUNC("SetDescriptor", pLPane_SetDescriptor),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LPane, "LPane", NULL, NULL, mfunc_LPane)

/*******************************************************************

LListener class interface.

********************************************************************/

/* Just a class descriptor to provide a target for casting. */

CPPI_CLASSDEF(class_LListener, "LListener", NULL, NULL, NULL);

/*******************************************************************

LBroadcaster class interface.

********************************************************************/

extern ClassDefinition class_LBroadcaster;

static int pLBroadcaster_AddListener(const void *object, const ClassDefinition *class_def)
{
	PWord a3; int t3;
	void *listener;
	ClassDefinition *listener_class;

    if (!CPPI_cast_object(object, class_def, &object, &class_LBroadcaster)) PI_FAIL;
    
    PI_getan(&a3, &t3, 3);
	if (!CPPI_get_object(a3, t3, &listener, &listener_class)) PI_FAIL;
    if (!CPPI_cast_object(listener, listener_class, &listener, &class_LListener)) PI_FAIL;

	((LBroadcaster *)object)->AddListener((LListener *)listener);

	PI_SUCCEED;
}


CPPI_BEGMFUNC(mfunc_LBroadcaster)
	CPPI_MEMBERFUNC("AddListener", pLBroadcaster_AddListener),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LBroadcaster, "LBroadcaster", NULL, NULL, mfunc_LBroadcaster)

/*******************************************************************

LWindow class interface.

********************************************************************/

extern ClassDefinition class_LWindow;

static int p_new_LWindow(const void *object, const ClassDefinition *class_def)
{
	PWord a2; int t2;
	PWord a3; int t3;
	PWord a4; int t4;
	PWord a5; int t5;
	void *super; ClassDefinition *super_class;
	void *window;
	PWord obj;
	int objtype;
	
    PI_getan(&a2, &t2, 2);
    PI_getan(&a3, &t3, 3);
    PI_getan(&a4, &t4, 4);
    PI_getan(&a5, &t5, 5);
	
	if (t2 != PI_INT)
		if (!CI_get_integer((unsigned long *)&a2,t2))
			PI_FAIL;

	if (t3 != PI_INT)
		if (!CI_get_integer((unsigned long *)&a3,t3))
			PI_FAIL;
	
	if (!CPPI_get_object(a4, t4, &super, &super_class)) PI_FAIL;
    if (!CPPI_cast_object(super, super_class, &super, &class_LCommander)) PI_FAIL;
	

	window = (void *) new LWindow(a2, a3, (LCommander *)super);
	
	CPPI_make_object(&obj, &objtype, window, &class_LWindow);
	
	if (PI_unify(a5,t5,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}


CPPI_BEGBASE(base_LWindow)
	CPPI_BASECLASS(class_LPane),
	{0}
CPPI_ENDBASE

CPPI_BEGMFUNC(mfunc_LWindow)
	CPPI_MEMBERFUNC("LWindow", p_new_LWindow),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LView, "LView", NULL, NULL, NULL)

static int LWindowCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LPane) r = (LPane *)(LWindow *)object;
	else if (to_class == &class_LView) r = (LView *)(LWindow *)object;
	else if (to_class == &class_LCommander) r = (LCommander *)(LWindow *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}

CPPI_CLASSDEF(class_LWindow, "LWindow", base_LWindow, LWindowCaster, mfunc_LWindow)

/*******************************************************************

LStdButton class interface.

********************************************************************/

extern ClassDefinition class_LStdButton;

static int pLStdButton(const void *object, const ClassDefinition *class_def)
{
	PWord arg2, arg3, arg4, arg5, arg6;
	int type2, type3, type4, type5, type6;
	void *control;
	PWord obj;
	int objtype;
	PWord v; int t;
	
    PI_getan(&arg2, &type2, 2);
    PI_getan(&arg3, &type3, 3);
    PI_getan(&arg4, &type4, 4);
    PI_getan(&arg5, &type5, 5);
    PI_getan(&arg6, &type6, 6);
	
	if (type2 != PI_INT) if (!CI_get_integer((unsigned long *)&arg2,type2)) PI_FAIL;
	if (type3 != PI_INT) if (!CI_get_integer((unsigned long *)&arg3,type3)) PI_FAIL;
	if (type4 != PI_INT) if (!CI_get_integer((unsigned long *)&arg4,type4)) PI_FAIL;
	if (type5 != PI_INT) if (!CI_get_integer((unsigned long *)&arg5,type5)) PI_FAIL;
	
	SPaneInfo x = *((SPaneInfo *)arg2);
	control = (void *) new LStdButton(x, (long)arg3,
							(long)arg4, (unsigned char *)arg5);
	
	CPPI_make_object(&obj, &objtype, control, &class_LStdButton);
	
	if (PI_unify(arg6,type6,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}


CPPI_BEGBASE(base_LStdControl)
	CPPI_BASECLASS(class_LPane),
	CPPI_BASECLASS(class_LBroadcaster),
	{0}
CPPI_ENDBASE

CPPI_CLASSDEF(class_LStdControl, "LStdControl", base_LStdControl, NULL, NULL)

CPPI_BEGBASE(base_LStdButton)
	CPPI_BASECLASS(class_LStdControl),
	{0}
CPPI_ENDBASE

CPPI_BEGMFUNC(mfunc_LStdButton)
	CPPI_MEMBERFUNC("LStdButton", pLStdButton),
	{0}
CPPI_ENDMFUNC

static int LStdButtonCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LBroadcaster) r = (LBroadcaster *)(LStdButton *)object;
	else if (to_class == &class_LPane) r = (LPane *)(LStdButton *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}


CPPI_CLASSDEF(class_LStdButton, "LStdButton", base_LStdButton, LStdButtonCaster, mfunc_LStdButton)

/*******************************************************************

LStdRadioButton class interface.

********************************************************************/


extern ClassDefinition class_LStdRadioButton;

static int pLStdRadioButton(const void *object, const ClassDefinition *class_def)
{
	PWord arg2, arg3, arg4, arg5, arg6, arg7;
	int type2, type3, type4, type5, type6, type7;
	void *radio;
	PWord obj;
	int objtype;
	PWord v; int t;
	
    PI_getan(&arg2, &type2, 2);
    PI_getan(&arg3, &type3, 3);
    PI_getan(&arg4, &type4, 4);
    PI_getan(&arg5, &type5, 5);
    PI_getan(&arg6, &type6, 6);
    PI_getan(&arg7, &type7, 7);
	
	if (type2 != PI_INT) if (!CI_get_integer((unsigned long *)&arg2,type2)) PI_FAIL;
	if (type3 != PI_INT) if (!CI_get_integer((unsigned long *)&arg3,type3)) PI_FAIL;
	if (type4 != PI_INT) if (!CI_get_integer((unsigned long *)&arg4,type4)) PI_FAIL;
	if (type5 != PI_INT) if (!CI_get_integer((unsigned long *)&arg5,type5)) PI_FAIL;
	if (type6 != PI_INT) if (!CI_get_integer((unsigned long *)&arg6,type6)) PI_FAIL;
	
	SPaneInfo x = *((SPaneInfo *)arg2);
	radio = (void *) new LStdRadioButton(x, (long)arg3,
							(long)arg4, (short)arg5, (unsigned char *)arg6);
	
	CPPI_make_object(&obj, &objtype, radio, &class_LStdRadioButton);
	
	if (PI_unify(arg7,type7,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}


CPPI_BEGBASE(base_LStdRadioButton)
	CPPI_BASECLASS(class_LStdControl),
	{0}
CPPI_ENDBASE

CPPI_BEGMFUNC(mfunc_LStdRadioButton)
	CPPI_MEMBERFUNC("LStdRadioButton", pLStdRadioButton),
	{0}
CPPI_ENDMFUNC

static int LStdRadioButtonCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LBroadcaster) r = (LBroadcaster *)(LStdRadioButton *)object;
	else if (to_class == &class_LPane) r = (LPane *)(LStdRadioButton *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}


CPPI_CLASSDEF(class_LStdRadioButton, "LStdRadioButton", base_LStdRadioButton, LStdRadioButtonCaster, mfunc_LStdRadioButton)



/*******************************************************************

CPrologListener class interface.

********************************************************************/

extern ClassDefinition class_CPrologListener;

static int pCPrologListener(const void *object, const ClassDefinition *class_def)
{
	PWord v3; int t3;
	PWord pred; int predtype;
	PWord obj; int objtype;
	void * listener;
	
	/* Get the list of arguments. */
	PI_getan(&pred, &predtype, 2);
	PI_getan(&v3, &t3, 3);

	/* get the one argument - the predicate string. */
	if (predtype != PI_INT)
		if (!CI_get_integer((unsigned long *)&pred,predtype))
			PI_FAIL;

	/* create the listner. */
	listener = (void *) new CPrologListener((const char *)pred);

	/* Make an object tag. */
	CPPI_make_object(&obj, &objtype, listener, &class_CPrologListener);


	if (PI_unify(v3,t3,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int CPrologListenerCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LListener) r = (LListener *)(CPrologListener *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}



CPPI_BEGMFUNC(mfunc_CPrologListener)
	CPPI_MEMBERFUNC("CPrologListener", pCPrologListener),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_CPrologListener, "CPrologListener", NULL, CPrologListenerCaster, mfunc_CPrologListener)

/*******************************************************************

LCaption class interface.

********************************************************************/

extern ClassDefinition class_LCaption;

static int pLCaption(const void *object, const ClassDefinition *class_def)
{
	PWord arg2, arg3, arg4, arg5;
	int type2, type3, type4, type5;
	void *caption;
	PWord obj;
	int objtype;
	
    PI_getan(&arg2, &type2, 2);
    PI_getan(&arg3, &type3, 3);
    PI_getan(&arg4, &type4, 4);
    PI_getan(&arg5, &type5, 5);
	
	if (type2 != PI_INT) if (!CI_get_integer((unsigned long *)&arg2,type2)) PI_FAIL;
	if (type3 != PI_INT) if (!CI_get_integer((unsigned long *)&arg3,type3)) PI_FAIL;
	if (type4 != PI_INT) if (!CI_get_integer((unsigned long *)&arg4,type4)) PI_FAIL;
	
	SPaneInfo x = *((SPaneInfo *)arg2);
	caption = (void *) new LCaption(x,
							(unsigned char *)arg3, (long)arg4);
	
	CPPI_make_object(&obj, &objtype, caption, &class_LCaption);
	
	if (PI_unify(arg5,type5,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int LCaptionCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LPane) r = (LPane *)(LCaption *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}

CPPI_BEGBASE(base_LCaption)
	CPPI_BASECLASS(class_LPane),
	{0}
CPPI_ENDBASE

CPPI_BEGMFUNC(mfunc_LCaption)
	CPPI_MEMBERFUNC("LCaption", pLCaption),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LCaption, "LCaption", base_LCaption, LCaptionCaster, mfunc_LCaption)

/*******************************************************************

LRadioGroup class interface.

********************************************************************/

extern ClassDefinition class_LRadioGroup;

static int pLRadioGroup(const void *object, const ClassDefinition *class_def)
{
	PWord arg2; int type2;
	void *radio;
	PWord obj; int objtype;
	
    PI_getan(&arg2, &type2, 2);
	
	radio = (void *) new LRadioGroup();
	
	CPPI_make_object(&obj, &objtype, radio, &class_LRadioGroup);
	
	if (PI_unify(arg2,type2,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}

static int pLRadioGroup_AddRadio(const void *object, const ClassDefinition *class_def)
{
	PWord a3; int t3;
	void *radio;
	ClassDefinition *radio_class;

    if (!CPPI_cast_object(object, class_def, &object, &class_LRadioGroup)) PI_FAIL;
    
    PI_getan(&a3, &t3, 3);
	if (!CPPI_get_object(a3, t3, &radio, &radio_class)) PI_FAIL;
    if (!CPPI_cast_object(radio, radio_class, &radio, &class_LStdRadioButton)) PI_FAIL;

	((LRadioGroup *)object)->AddRadio((LStdRadioButton *)radio);

	PI_SUCCEED;
}


CPPI_BEGMFUNC(mfunc_LRadioGroup)
	CPPI_MEMBERFUNC("LRadioGroup", pLRadioGroup),
	CPPI_MEMBERFUNC("AddRadio", pLRadioGroup_AddRadio),
	{0}
CPPI_ENDMFUNC

CPPI_CLASSDEF(class_LRadioGroup, "LRadioGroup", NULL, NULL, mfunc_LRadioGroup)


/*******************************************************************

LBroadcastEditField class interface.

********************************************************************/
class CBroadcastEditField : public LEditField, public LBroadcaster {
public:
			CBroadcastEditField(
					const SPaneInfo		&inPaneInfo,
					Str255				inString,
					ResIDT				inTextTraitsID,
					Int16				inMaxChars,
					Uint8				inAttributes,
					KeyFilterFunc		inKeyFilter,
					LCommander			*inSuper);
					
	virtual	void		UserChangedText();
	
};

CBroadcastEditField::CBroadcastEditField(
								const SPaneInfo		&inPaneInfo,
								Str255				inString,
								ResIDT				inTextTraitsID,
								Int16				inMaxChars,
								Uint8				inAttributes,
								KeyFilterFunc		inKeyFilter,
								LCommander			*inSuper)
 								: LEditField(inPaneInfo,
									inString,
									inTextTraitsID,
									inMaxChars,
									inAttributes,
									inKeyFilter,
									inSuper)
{
}

void CBroadcastEditField::UserChangedText()
{	
	BroadcastMessage(1, ::TEGetText(GetMacTEH()));
}

extern ClassDefinition class_CBroadcastEditField;

static int pCBroadcastEditField(const void *object, const ClassDefinition *class_def)
{
	PWord arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9;
	int type2, type3, type4, type5, type6, type7, type8, type9;
	void *field;
	void *commander; ClassDefinition *commander_class;
	PWord obj;
	int objtype;
	PWord v; int t;
	
    PI_getan(&arg2, &type2, 2);
    PI_getan(&arg3, &type3, 3);
    PI_getan(&arg4, &type4, 4);
    PI_getan(&arg5, &type5, 5);
    PI_getan(&arg6, &type6, 6);
    PI_getan(&arg7, &type7, 7);
    PI_getan(&arg8, &type8, 8);
    PI_getan(&arg8, &type8, 8);
    PI_getan(&arg9, &type9, 9);
	
	if (type2 != PI_INT) if (!CI_get_integer((unsigned long *)&arg2,type2)) PI_FAIL;
	if (type3 != PI_INT) if (!CI_get_integer((unsigned long *)&arg3,type3)) PI_FAIL;
	if (type4 != PI_INT) if (!CI_get_integer((unsigned long *)&arg4,type4)) PI_FAIL;
	if (type5 != PI_INT) if (!CI_get_integer((unsigned long *)&arg5,type5)) PI_FAIL;
	if (type6 != PI_INT) if (!CI_get_integer((unsigned long *)&arg6,type6)) PI_FAIL;
	if (type7 != PI_INT) if (!CI_get_integer((unsigned long *)&arg7,type7)) PI_FAIL;
	if (!CPPI_get_object(arg8, type8, &commander, &commander_class)) PI_FAIL;
    if (!CPPI_cast_object(commander, commander_class, &commander, &class_LCommander)) PI_FAIL;
	
	SPaneInfo x = *((SPaneInfo *)arg2);
	field = (void *) new CBroadcastEditField(x, (unsigned char *)arg3,
							(ResIDT)arg4, (Int16)arg5, (Uint8)arg6, (KeyFilterFunc)arg7,
							(LCommander *)commander);
	
	CPPI_make_object(&obj, &objtype, field, &class_CBroadcastEditField);
	
	if (PI_unify(arg9,type9,obj,objtype))
		PI_SUCCEED;
	PI_FAIL;
}


CPPI_BEGBASE(base_CBroadcastEditField)
	CPPI_BASECLASS(class_LPane),
	CPPI_BASECLASS(class_LBroadcaster),
	{0}
CPPI_ENDBASE

CPPI_BEGMFUNC(mfunc_CBroadcastEditField)
	CPPI_MEMBERFUNC("CBroadcastEditField", pCBroadcastEditField),
	{0}
CPPI_ENDMFUNC

static int CBroadcastEditFieldCaster(const void *object, const struct CD *to_class, const void **cast_obj)
{
	void *r = NULL;
	if (to_class == &class_LPane) r = (LPane *)(CBroadcastEditField *)object;
	if (to_class == &class_LBroadcaster) r = (LBroadcaster *)(CBroadcastEditField *)object;
	
	if (r) {
		*cast_obj = r;
		return 1;
	}
	else return 0;
}


CPPI_CLASSDEF(class_CBroadcastEditField, "CBroadcastEditField", base_CBroadcastEditField,
				CBroadcastEditFieldCaster, mfunc_CBroadcastEditField)


/**********************************/

CI_BEGARRAY(struct_SBooleanRect)
	CI_FIELD("left", left, SBooleanRect, CI_CHARTYPE, "Boolean"),
	CI_FIELD("top", top, SBooleanRect, CI_CHARTYPE, "Boolean"),
	CI_FIELD("right", right, SBooleanRect, CI_CHARTYPE, "Boolean"),
	CI_FIELD("bottom", bottom, SBooleanRect, CI_CHARTYPE, "Boolean")
CI_ENDARRAY

CI_BEGARRAY(struct_SPaneInfo)
	CI_FIELD("paneID", paneID, SPaneInfo, 8, "PaneIDT"),
	CI_FIELD("width", width, SPaneInfo, 8, "Int16"),
	CI_FIELD("height", height, SPaneInfo, 8, "Int16"),
	CI_FIELD("visible", visible, SPaneInfo, 6, "Boolean"),
	CI_FIELD("enabled", enabled, SPaneInfo, 6, "Boolean"),
	CI_FIELD("left", left, SPaneInfo, 3, "Int32"),
	CI_FIELD("top", top, SPaneInfo, 3, "Int32"),
	CI_FIELD("bindings", bindings, SPaneInfo, 0, "SBooleanRect"),
	CI_FIELD("userCon", userCon, SPaneInfo, 3, "Int32"),
	CI_FIELD("superView", superView, SPaneInfo, 5, "LView*"),
	{0}
CI_ENDARRAY


#pragma cplusplus off

void PPHackGlue_init(void);
void PPHackGlue_init(void)
{
//	CI_STRUCT("SWindowInfo",SWindowInfo,fieldsSWindowInfo);
	
	
	CI_INTCONST("windAttr_Regular", windAttr_Regular);
	CI_INTCONST("windAttr_Enabled", windAttr_Enabled);
	CI_INTCONST("editAttr_Box", editAttr_Box);
	
	CI_STRUCT("SBooleanRect", SBooleanRect, struct_SBooleanRect);
	CI_STRUCT("SPaneInfo", SPaneInfo, struct_SPaneInfo);
	
	// This will insert into the runtime CPP class system and
	// register all the messages.
	CPPI_CLASS(class_LView);
	CPPI_CLASS(class_LWindow);
	CPPI_CLASS(class_LApplication);
	CPPI_CLASS(class_LStdButton);
	CPPI_CLASS(class_CPrologListener);
	CPPI_CLASS(class_LCaption);
	CPPI_CLASS(class_LStdRadioButton);
	CPPI_CLASS(class_LRadioGroup);
	CPPI_CLASS(class_CBroadcastEditField);

}

/*

cpp_new('LApplication', [], MyApp),
cpp_new('LWindow', [1, MyApp], MyWindow),
cpp_send(MyWindow, 'Show', [], X),
cpp_send(MyApp, 'Run', [], X).

cpp_new('LWindow', [1, object(0, 'LCommander')], W), cpp_send(W, 'Show', [], X).
CI_BEGARRAY(fieldsSWindowInfo)
	CI_FIELD("WINDid", WINDid, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("layer", layer, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("attributes", attributes, SWindowInfo, CI_LONGTYPE, ""), 
	CI_FIELD("minimumWidth", minimumWidth, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("minimumHeight", minimumHeight, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("maximumWidth", maximumWidth, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("maximumHeight", maximumHeight, SWindowInfo, CI_SHORTTYPE, ""),
	CI_FIELD("standardSize", standardSize, SWindowInfo, CI_STRUCTTYPE, ""),
	CI_FIELD("userCon", userCon, SWindowInfo, CI_LONGTYPE, ""),
  {0}
CI_ENDARRAY
*/


/*

The plan:

create cpp_intf.pro
and cpp_intf.c

Exported predicates:

cpp_new(+Class, +Args, -Object).
	call a classes new function

cpp_destroy(+Object).
	dispose of an object

cpp_send(+Object, +Message, +Args, -Result).
	send te

cpp_new(Class, Args, Object) :-
	$cpp_new(Class, Args, Object).

cpp_destroy(

example:

cpp_new(class_CALSPrologApp, [], MyApp),
cpp_send(class_LWindow, 'LWindow::CreateWindow', [1, MyApp], MyWindow),
// or cpp_new(class_LWindow, [1, Attr, MyApp], MyWindow),
cpp_send(MyWindow, 'Show', []),
cpp_send(MyApp, 'Run', [], Ignore),
cpp_destroy(MyWindow),
cpp_destroy(MyApp).

*/