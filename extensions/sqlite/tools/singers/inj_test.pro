
:- ['track_code.pro', 'artist_code.pro', 'crooners.pro'].


test_insert_bobby_tables 
	:-
	catch( test_insert_bobby_tables0, error(Err,EArgs), do_bobby_err(Err,EArgs)).

test_insert_bobby_tables0
	:-
	(exists_file(singers_data) ->
        	remove_file(singers_data) ; true ),
	dbmod_track:abolish(is_handle_db_track/1),
	dbmod_artist:abolish(is_handle_db_artist/1),
	recreate_table_db_artist,
        recreate_table_db_track,

	Title = 'Robert\', 1); DROP TABLE track_table;--',
	printf('Try inserting "%t" \n', [Title]),
        (insert_db_track(track(11, Title, 1)) ->
		printf('Attempt to insert "%t" SUCCEEDED!!!!\n', [Title])
		;
		printf('Attempt to insert "%t" FAILED\n', [Title])
	).
do_bobby_err(Err,EArgs)
	:-
	write(do_bobby_err = bobby_err(Err,EArgs)),nl.


init_track
	:-
	(exists_file(singers_data) ->
        	remove_file(singers_data) ; true ),
	dbmod_track:abolish(is_handle_db_track/1),
	dbmod_artist:abolish(is_handle_db_artist/1),
	recreate_table_db_artist,
        recreate_table_db_track,
        make_artists.
	
test_insert_with_single_quote 
	:-
	init_track,
	AM = 'That''s Amore',
	printf('test_insert_AM = %t\n\n', [AM]),
        insert_db_track(track(11,AM,1)),
	show_tt.

show_tt :-
        printf('Retrieve:  track_table rows:\n\n', []),
        select_all_table('./singers_data', track_table, TrackRows),
        write_lines(TrackRows).

t0 :-  init_track,
	printf('\nInput (get_line(X)):  ',[]),
	get_line(AM),
	printf('test_insert_AM = %t\n\n', [AM]),
        printf('Execute:  insert_db_track(track(11,AM,1))\n\n', []),
        printf('Execute:  insert_db_track(track(11,%t,1))\n\n', [AM]),
        insert_db_track(track(11,AM,1)),
	show_tt.

