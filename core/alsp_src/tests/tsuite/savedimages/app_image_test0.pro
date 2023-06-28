
mk_app_image0 
	:- 
	save_image(app_image0, [start_goal( start_app_image0 ), select_lib([]), verbose(false)]),
	printf(user, 'app_image0_saved\n', []).

start_app_image0 :-
	printf(user, 'app_image0_running\n', []).
