/*======================================================================*
 |	Win32 Service.pro
 |	Copyright (c) 1997 Applied Logic Systems, Inc.
 |
 |	-- Win32 Service Module.
 |
 *======================================================================*/

:- dynamic(is_win32_service/0).

module win32_service.

export is_win32_service/0.

export save_win32_service_image/1.
export save_win32_service_image/2.

save_win32_service_image(Name) :-
	save_win32_service_image(Name, []).

save_win32_service_image(Name, Options) :-
	builtins:fix_image_name(Name, FixedName),
	builtins:process_image_options(Options, FixedName, OptionedName),
	pbi_copy_file('Service Package Stub.exe', OptionedName),
	attach_image(OptionedName).
	
endmod.
