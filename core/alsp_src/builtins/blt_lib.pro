:-libactivate(builtins,['library',misc_io],
    [read_terms/1,read_terms/2, read_terms_pos/1,
	 read_terms_pos/2, read_terms_vn/2, read_as_list/3,
        colwrite/4, putc_n_of/3,
		gen_file_header/3,gen_file_header/4],[]).

:-libactivate(builtins,['library',listutl1],
    [append/2,list_diff/3,list_diffs/4,symmetric_diff/3,intersect/3,
        intersect/2, int_diff/4, union/3,sorted_merge/3,sorted_merge/2],[]).

:-libactivate(builtins,['library',listutl2],
    [deleteNth/3,change_nth/3,nth/3,nth_tail/4,at_most_n/3,position/3,
        position/4,get_list_tail/3,list_delete/3,sublist/4,
		subst_nth/4,last/2],[]).

:-libactivate(builtins,['library',listutl3],
    [nobind_member/2,output_prolog_list/1,output_prolog_list/4,
    	output_prolog_list/5,
	flatten/2,n_of/3,number_list/2, number_list/3,encode_list/3,
	struct_lookup_subst/4,check_default/4,check_default_del/5,
	remove_tagged/3, merge_plists/3, mangle_change_tagged/3,
	subst_tagged/4, merge_in_list/3 ],[]).

%:-libactivate(builtins,['library',lib_ctl],
%	[bagOf/3, bagOf/4,setOf/4, setOf/3, max/3, min/3]).
%:-module_closure(bagOf,3,bagOf), module_closure(setOf,3,setOf).

:-builtins:libactivate(builtins, ['library',lib_ctl],
	[bagOf/3, bagOf/4,setOf/4, setOf/3, max/3, min/3],
	[module_closure(bagOf,3,bagOf), 
	 module_closure(setOf,3,setOf)]).

:-libactivate(builtins,['library',strings],
    [asplit/4,head/4,alower/2,asub/4,asplit0/4,asplit00/4,head0/4,alower0/2,
        insert_spaces/2,catenate/2,catenate/3,string_to_uia/2,cnvrt_to_UIA/2,
        string_to_uia/3,sized_string_to_uia/3,copy_to_uia/3,atomic_to_uia/2,
        make_uc/2,make_lc/2,make_uc_sym/2,make_lc_sym/2,change_case_sym/2,
        convert_to_uc/2,same_uc/2,truncate/3,strip_white/2, strip_tail_white/2,
		read_to/5,read_to_blank/3
		],[]).

:-libactivate(avl,['library',avl],
    [avl_create/1,avl_inorder/2,avl_inorder_wdata/2,avl_insert/4,avl_search/3],[]).

:-libactivate(windows,['library',simplio],
    [simple_menu/3, fin_simple_menu_code0/5, query_user/3,query_user/5 ],[]).

:-libactivate(windows,['library',iolayer],[chooseFile/3,menu/3,menu/4],[]).

:-libactivate(builtins,['library',commal],
				[flatten_comma_list/2, build_comma_list/2],[]).

:-libactivate(builtins, ['library',misc_db],
	[assert_all/2,assert_all_refs/3,erase_all/1,abolish_list/1,
	 abolish_list/2, retract_all/1],
	[module_closure(assert_all,1),module_closure(assert_all_refs,2)]
	).

%:-module_closure(assert_all,1), module_closure(assert_all_refs,2).

:-op(600,xfy,':=').
:-libactivate(objects,['library',objs_run],
	[send/2, send/3, send_all/2, send_each/2, 
	 queue_oop_event/1, insert_oop_event_request/2,
	 inherit/3, send_self/2, init_nil/2, clone/2,
	 accessObjStruct/3, setObjStruct/3, ':='/2, set_slots0/4,
	 deref_val/3, standardObject/2, genericObjects/2 ],[]).


   %% for macros:
:- op(925,fx,``).
:- op(930,fx,`).
:- op(600,xfx,'=>').
:- op(1190,xfx,when).
:- op(1180,xfx,where).
:- op(1170,xfx,with).
:- op(1160,xfx,if).

:-libactivate(builtins,['library',typecomp],
			[comptype_cl/0, comptype/0, comptype/1],[]).

:-libactivate(objects,['library',objects],
	[defineObject/2, defineClass/2, set_object_messages/1,
		defineObject/3, defineClass/3,
		opf/0, objectProcessFile_cl/0, 
		opf/1, objectProcessFile/1,
		objectProcessFile/2, objectProcessFile/3,
		do_objectProcess/4, do_objectProcess/5,
		merge_defaults/3,
		mods_with_objs/1, mods_with_classes/1,
		objs_in_mod/2, classes_in_mod/2,
		dpos/1, don/1, sdos/1, sdon/1,
		set_obj_profile/2, obj_slots/2], [] ).

:-libactivate(macroxp,['library',macro],
	[define_macro/1, mx_cl/0,
		mx/0, mx/1, expand_macros/2,
		macro_expand/0, macro_expand_files/2,
		macro_expand_files/3, macro_expand/2], [] ).

:-libactivate(builtins,['library', cmdline],
	[get_cmdline_vals/1,cmdline_vals/2, eat_cmd_line/3,
		pull_out_nullswitches/3 ], [] ).

:-libactivate(builtins,['library',xlists],
	[xlist_init/1, xlist_make/3,
		 xlist_unit_c/2, xlist_unit_l/2,
		 xlist_head/2, xlist_tail/2,
		 xlist_append/3, xlist_append/2,
		 xlist_mem/2,xlist_dmem/2],[]).

:-libactivate(sio,['library',sockmisc],
	[bread/1,bread/2, bread/3, bread_term/3, bread_term/4], [] ).

:-libactivate(builtins,['library',strctutl],
	[locate_struct/3, delete_struct/3, delete_struct/4], [] ).

:-libactivate(builtins,['library',slashes],
	[slash2list/2], [] ).

:-libactivate(builtins,['library',filemisc],
	[copy_fileslist_nl/4, copy_file_nl/3, copy_dir_files_nl/3,
	 install_file_links/2, copy_stream_nl/3, 
	 write_lines/1, write_lines/2, write_lines_nl/3, write_lines_nl/4,
	 write_lines_opt/2,write_lines_opt/3,
	 write_clause/1, write_clause/2, write_clause/3,
	 write_clauses/1, write_clauses/2, write_clauses/3,
	 get_lines/2, grab_lines/2
	 ], [] ).

:-libactivate(builtins,['library',sconfig],
	[general_os/3,general_os/4,lib_extension/2,
	 winsystems_for/2, winsystems_for/3, ws_vars/3,ws_vars/5,
	 flatten_ws_lists/2,  flatten_to_atom/2, cat_together_seplines/2,
	 system_dir_root/2, cat_together_spaced/2, prefix_dir/3,
	 determine_default_ws/1, known_ws/1,
	 specif_winsystems_for/3 ], [] ).

:-libactivate(builtins,['library',cmpdirs], [cmpdirs/3], [] ).

:-libactivate(cref,['library',cref], 
			[cref/1,cref/2,cref_shell/0], [] ).

:-libactivate(cref,['library',crefxtra], 
		[union_files_calls/2,
		 prefix_subset/2,prefix_subset/3,
		 prefix_undefs/2,
		 strip_prefix/3,prefix_undefs_basis/2,
		 prefix_undefs_basis_filt/1], [] ).

:-libactivate(builtins,['library',miscatom], 
		[atm_tail/3, trim_atoms/3
		], [] ).

:-libactivate(builtins,['library',progdoc], 
		[prog_doc/3, fm_doc/1
		], [] ).

