
:- ['track_code.pro', 'artist_code.pro', 'crooners.pro'].


test_insert_bobby_tables 
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

/*-------

(1) 1 call: user:insert_db_track(track(11,That''s Amore,1))?l

compose_row [false] That''s Amore

ior = INSERT INTO track_table VALUES (11, 'That''s Amore', 1 );

buf=./singers_data  sql=INSERT INTO track_table VALUES (11, 'That''s Amore', 1 );

-----*/
	
test_insert_with_single_quote 
	:-
	(exists_file(singers_data) ->
        	remove_file(singers_data) ; true ),
	dbmod_track:abolish(is_handle_db_track/1),
	dbmod_artist:abolish(is_handle_db_artist/1),
	recreate_table_db_artist,
        recreate_table_db_track,
        make_artists,

        insert_db_track(track(11,'That\'\'s Amore',1)),
        insert_db_track(track(12,'Christmas Blues',1)),
        insert_db_track(track(13,'My Way',2)),
        nl,printf('track_table rows:\n', []),
        select_all_table('./singers_data', track_table, TrackRows),
        write_lines(TrackRows).

