/*---------------------------------------------------------*
 |			macos.h
 |		Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *---------------------------------------------------------*/

#define CGLUESUPPORTED	0
#define OLDROUTINENAMES	0
#define OLDROUTINELOCATIONS	0


	#include <AppleEvents.h>
	#include <Controls.h>
	#include <Dialogs.h>
	#include <Events.h>
	#include <Files.h>
	#include <Fonts.h>
	#include <Gestalt.h>
	#include <Lists.h>
	#include <Memory.h>
	#include <Menus.h>
	#include <MixedMode.h>
	#include <OSUtils.h>
	#include <Packages.h>
	#include <Printing.h>
	#include <Processes.h>
	#include <Quickdraw.h>
	#include <Resources.h>
	#include <Scrap.h>
	#include <StandardFile.h>
	#include <Strings.h>
	#include <TextEdit.h>
	#include <TextUtils.h>
	#include <ToolUtils.h>
	#include <Traps.h>
	#include <Windows.h>

void macos_compact_init(void);
void macos2_init(void);
void macos3_init(void);
void macos4_init(void);
void macos_init(void);
