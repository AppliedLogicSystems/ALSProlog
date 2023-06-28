
mk_app_image1 
	:- 
	save_image(app_image1, [start_goal( start_app_image1 ), select_lib([]), verbose(false)]),
	printf(user, 'app_image1_saved\n', []).


start_app_image1 :-
%	File = './app_image_test2.pro',
getenv('TESTDIR', TestDir),
atom_concat(TestDir, '/savedimages/app_image_test2.pro', File),
	xconsult(File, NErrs, FinalErrs),
	save_image(app_image2, [start_goal( start_app_image2 ), select_lib([]), verbose(false)]),
	printf(user, 'app_image1_making_app_image2\n', []).
