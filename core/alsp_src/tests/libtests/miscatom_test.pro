test_miscatom_lib
        :-
        test_catenate3,
        test_catenate2,
        test_trim_atoms,
        true.

test_catenate3 :-
        catenate(abc, def, Atom3),
        Atom3 == abcdef.
test_catenate3 :-
        printf(user, 'catenate3 test failed\n', []).

test_catenate2 :-
        catenate([abc, def, ghty], Result),
        Result == abcdefghty.
test_catenate2 :-
        printf(user, 'catenate2 test failed\n', []).

test_trim_atoms :-
        InAtoms = ['Abcd', gh768, bkdjfng, fr4],
        Sizes = [2,3,4,5],
        trim_atoms(InAtoms, Sizes, Results),
                % the truncation of gh768 is an atom:
        Results == [cd,'68',fng,''].
test_trim_atoms :-
        printf(user, 'trim_atoms test failed\n', []).

test_cat_together_seplines :-
        List = [a,b,c,d],
        als_system(SystemList),
        dmember(os = OS, SystemList),
        dmember(os_variation = OSVar, SystemList),
        ((OS = mswin32 ; OSVar = cygwin32) ->
                TgtResult = 'a\r\nb\r\nc\r\nd\r'
                ;
                TgtResult = 'a\nb\nc\nd\n'
        ),
        cat_together_seplines(List, Result),
        Result == TgtResult.

test_cat_together_seplines :-
        printf(user, 'cat_together_seplines test failed\n', []).

test_cat_together_spaced :-
        List = [a,b,c,d],
        cat_together_spaced(List, Result),
        Result == 'a b c d '.
test_cat_together_spaced :-
        printf(user, 'cat_together_spaced test failed\n', []).

test_prefix_to :-
        List = [a1,b2,c3],
        Atom = 'Zip_',
        prefix_to(List, Atom, XList),
        XList == ['Zip_a1','Zip_b2','Zip_c3'].
test_prefix_to :-
        printf(user, 'prefix_to test failed\n', []).

test_prefix_dir :-
        List = [foo, file3, bar],
        Dir = zipper,
        prefix_dir(List, Dir, XList),
        XList == ['zipper/foo','zipper/file3','zipper/bar'].
test_prefix_dir :-
        printf(user, 'prefix_dir test failed\n', []).

test_strip_prefix :-
        List = [abcd, foobar, pop, f, zeroes],
        NN = 3,
        strip_prefix(List, NN, Result),
        Result == [d,bar,'','',oes].
test_strip_prefix :-
        printf(user, 'strip_prefix test failed\n', []).


