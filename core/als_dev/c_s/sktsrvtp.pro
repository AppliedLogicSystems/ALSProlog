/*-------------------------------------------------------------*
                    /netwk/apache/als_dev/c_s/sktsrvtp.pro
               defStruct Type definitions generated from file:
                    /netwk/apache/als_dev/c_s/sktsrvtp.typ
               by ALS defStruct Type Generator
               Macros written to file: /netwk/apache/als_dev/c_s/sktsrvtp.mac
 *-------------------------------------------------------------*/

module utilities.
use socket_comms.
endmod.

module socket_comms.
use utilities.


%--- server_info defStruct ---

export access_server_info/3.
export set_server_info/3.
access_server_info(max_num_wkrs,_A,_B) :- arg(1,_A,_B).
set_server_info(max_num_wkrs,_A,_B) :- mangle(1,_A,_B).

access_server_info(machine_tgts,_A,_B) :- arg(2,_A,_B).
set_server_info(machine_tgts,_A,_B) :- mangle(2,_A,_B).

access_server_info(working_machines,_A,_B) :- arg(3,_A,_B).
set_server_info(working_machines,_A,_B) :- mangle(3,_A,_B).

access_server_info(idle_machines,_A,_B) :- arg(4,_A,_B).
set_server_info(idle_machines,_A,_B) :- mangle(4,_A,_B).

access_server_info(trusted_administration,_A,_B) :- arg(5,_A,_B).
set_server_info(trusted_administration,_A,_B) :- mangle(5,_A,_B).

access_server_info(admin_pw,_A,_B) :- arg(6,_A,_B).
set_server_info(admin_pw,_A,_B) :- mangle(6,_A,_B).

access_server_info(cur_logged_in,_A,_B) :- arg(7,_A,_B).
set_server_info(cur_logged_in,_A,_B) :- mangle(7,_A,_B).

access_server_info(user_file,_A,_B) :- arg(8,_A,_B).
set_server_info(user_file,_A,_B) :- mangle(8,_A,_B).

access_server_info(users_area,_A,_B) :- arg(9,_A,_B).
set_server_info(users_area,_A,_B) :- mangle(9,_A,_B).

access_server_info(accounting_file,_A,_B) :- arg(10,_A,_B).
set_server_info(accounting_file,_A,_B) :- mangle(10,_A,_B).

access_server_info(banner,_A,_B) :- arg(11,_A,_B).
set_server_info(banner,_A,_B) :- mangle(11,_A,_B).

access_server_info(motd,_A,_B) :- arg(12,_A,_B).
set_server_info(motd,_A,_B) :- mangle(12,_A,_B).

access_server_info(local_read_stream,_A,_B) :- arg(13,_A,_B).
set_server_info(local_read_stream,_A,_B) :- mangle(13,_A,_B).

access_server_info(local_write_stream,_A,_B) :- arg(14,_A,_B).
set_server_info(local_write_stream,_A,_B) :- mangle(14,_A,_B).

access_server_info(log_warnings,_A,_B) :- arg(15,_A,_B).
set_server_info(log_warnings,_A,_B) :- mangle(15,_A,_B).

access_server_info(log_info,_A,_B) :- arg(16,_A,_B).
set_server_info(log_info,_A,_B) :- mangle(16,_A,_B).

access_server_info(log_file,_A,_B) :- arg(17,_A,_B).
set_server_info(log_file,_A,_B) :- mangle(17,_A,_B).

access_server_info(log_stream,_A,_B) :- arg(18,_A,_B).
set_server_info(log_stream,_A,_B) :- mangle(18,_A,_B).

access_server_info(server_warnings,_A,_B) :- arg(19,_A,_B).
set_server_info(server_warnings,_A,_B) :- mangle(19,_A,_B).

access_server_info(polling_timeout,_A,_B) :- arg(20,_A,_B).
set_server_info(polling_timeout,_A,_B) :- mangle(20,_A,_B).

access_server_info(ports_list,_A,_B) :- arg(21,_A,_B).
set_server_info(ports_list,_A,_B) :- mangle(21,_A,_B).

access_server_info(non_login_ports,_A,_B) :- arg(22,_A,_B).
set_server_info(non_login_ports,_A,_B) :- mangle(22,_A,_B).

access_server_info(extension_slot,_A,_B) :- arg(23,_A,_B).
set_server_info(extension_slot,_A,_B) :- mangle(23,_A,_B).

export make_server_info/1.
make_server_info(_A) :-
        _A'=..'
            [server_info,1,[],[],[],[],'zZLxE.Tg7nmo6',[],nil,nil,
                'accting.dfl',[],[],user_input,user_output,false,false,nil,
                nil,false,10000,[],[],[]].

export make_server_info/2.
make_server_info(_A,_B) :-
        struct_lookup_subst(
            [max_num_wkrs,machine_tgts,working_machines,idle_machines,
                trusted_administration,admin_pw,cur_logged_in,user_file,
                users_area,accounting_file,banner,motd,local_read_stream,
                local_write_stream,log_warnings,log_info,log_file,log_stream,
                server_warnings,polling_timeout,ports_list,non_login_ports,
                extension_slot],
            [1,[],[],[],[],'zZLxE.Tg7nmo6',[],nil,nil,'accting.dfl',[],[],
                user_input,user_output,false,false,nil,nil,false,10000,[],[],
                []],
            _B,_C),
        _A'=..'[server_info|_C].

export xmake_server_info/2.
xmake_server_info(
    server_info(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T,
        _U,_V,_W),
    [_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M,_N,_O,_P,_Q,_R,_S,_T,_U,_V,_W]).

endmod.

module utilities.
typeProperties(server_info,
    [max_num_wkrs,machine_tgts,working_machines,idle_machines,
        trusted_administration,admin_pw,cur_logged_in,user_file,users_area,
        accounting_file,banner,motd,local_read_stream,local_write_stream,
        log_warnings,log_info,log_file,log_stream,server_warnings,
        polling_timeout,ports_list,non_login_ports,extension_slot]).
noteOptionValue(server_info,_A,_B,_C) :- set_server_info(_A,_C,_B).
endmod.
