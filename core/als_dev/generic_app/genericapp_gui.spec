/*====================================================================*
 |		genericapp_gui.spec
 |		Copyright (c) 2000 Applied Logic Systems, Inc.
 |
 |		Top-level specification for a Generic Application
 |		
 |	Author: Ken Bowen
 *====================================================================*/

global_gui_options([
	name = genericapp,
	doc_extension = lts
	]).

gui_spec([
	name = genericapp_main,
	type = console,
    title = 'Generic Application',
	menu = [
		file,
		edit,
		windows,
		help
			]
]).


