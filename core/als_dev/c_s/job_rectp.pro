/*-------------------------------------------------------------*
                    /netwk/apache/als_dev/c_s/job_rectp.pro
               defStruct Type definitions generated from file:
                    /netwk/apache/als_dev/c_s/job_rectp.typ
               by ALS defStruct Type Generator
               Macros written to file: /netwk/apache/als_dev/c_s/job_rectp.mac
 *-------------------------------------------------------------*/

module utilities.
use socket_comms.
endmod.

module socket_comms.
use utilities.


%--- job_rec defStruct ---

export access_job_rec/3.
export set_job_rec/3.
access_job_rec(task,_A,_B) :- arg(1,_A,_B).
set_job_rec(task,_A,_B) :- mangle(1,_A,_B).

access_job_rec(taskArgs,_A,_B) :- arg(2,_A,_B).
set_job_rec(taskArgs,_A,_B) :- mangle(2,_A,_B).

access_job_rec(mod,_A,_B) :- arg(3,_A,_B).
set_job_rec(mod,_A,_B) :- mangle(3,_A,_B).

access_job_rec(state,_A,_B) :- arg(4,_A,_B).
set_job_rec(state,_A,_B) :- mangle(4,_A,_B).

access_job_rec(jobID,_A,_B) :- arg(5,_A,_B).
set_job_rec(jobID,_A,_B) :- mangle(5,_A,_B).

access_job_rec(userID,_A,_B) :- arg(6,_A,_B).
set_job_rec(userID,_A,_B) :- mangle(6,_A,_B).

access_job_rec(userAreaPath,_A,_B) :- arg(7,_A,_B).
set_job_rec(userAreaPath,_A,_B) :- mangle(7,_A,_B).

export make_job_rec/1.
make_job_rec(_A) :- _A'=..'[job_rec,nil,[],user,nil,0,nil,nil].

export make_job_rec/2.
make_job_rec(_A,_B) :-
        struct_lookup_subst(
            [task,taskArgs,mod,state,jobID,userID,userAreaPath],
            [nil,[],user,nil,0,nil,nil],_B,_C),
        _A'=..'[job_rec|_C].

export xmake_job_rec/2.
xmake_job_rec(job_rec(_A,_B,_C,_D,_E,_F,_G),[_A,_B,_C,_D,_E,_F,_G]).


%--- wkr_rec defStruct ---

export access_wkr_rec/3.
export set_wkr_rec/3.
access_wkr_rec(read_s,_A,_B) :- arg(1,_A,_B).
set_wkr_rec(read_s,_A,_B) :- mangle(1,_A,_B).

access_wkr_rec(write_s,_A,_B) :- arg(2,_A,_B).
set_wkr_rec(write_s,_A,_B) :- mangle(2,_A,_B).

access_wkr_rec(state,_A,_B) :- arg(3,_A,_B).
set_wkr_rec(state,_A,_B) :- mangle(3,_A,_B).

access_wkr_rec(workerID,_A,_B) :- arg(4,_A,_B).
set_wkr_rec(workerID,_A,_B) :- mangle(4,_A,_B).

access_wkr_rec(worker_info,_A,_B) :- arg(5,_A,_B).
set_wkr_rec(worker_info,_A,_B) :- mangle(5,_A,_B).

export make_wkr_rec/1.
make_wkr_rec(_A) :- _A'=..'[wkr_rec,nil,nil,idle,0,nil].

export make_wkr_rec/2.
make_wkr_rec(_A,_B) :-
        struct_lookup_subst([read_s,write_s,state,workerID,worker_info],
            [nil,nil,idle,0,nil],_B,_C),
        _A'=..'[wkr_rec|_C].

export xmake_wkr_rec/2.
xmake_wkr_rec(wkr_rec(_A,_B,_C,_D,_E),[_A,_B,_C,_D,_E]).


%--- wkr_info defStruct ---

export access_wkr_info/3.
export set_wkr_info/3.
access_wkr_info(host_name,_A,_B) :- arg(1,_A,_B).
set_wkr_info(host_name,_A,_B) :- mangle(1,_A,_B).

access_wkr_info(host_ip,_A,_B) :- arg(2,_A,_B).
set_wkr_info(host_ip,_A,_B) :- mangle(2,_A,_B).

access_wkr_info(login_name,_A,_B) :- arg(3,_A,_B).
set_wkr_info(login_name,_A,_B) :- mangle(3,_A,_B).

access_wkr_info(login_pw,_A,_B) :- arg(4,_A,_B).
set_wkr_info(login_pw,_A,_B) :- mangle(4,_A,_B).

access_wkr_info(port,_A,_B) :- arg(5,_A,_B).
set_wkr_info(port,_A,_B) :- mangle(5,_A,_B).

access_wkr_info(std_in,_A,_B) :- arg(6,_A,_B).
set_wkr_info(std_in,_A,_B) :- mangle(6,_A,_B).

access_wkr_info(std_out,_A,_B) :- arg(7,_A,_B).
set_wkr_info(std_out,_A,_B) :- mangle(7,_A,_B).

access_wkr_info(pid,_A,_B) :- arg(8,_A,_B).
set_wkr_info(pid,_A,_B) :- mangle(8,_A,_B).

export make_wkr_info/1.
make_wkr_info(_A) :- _A'=..'[wkr_info,nil,nil,nil,nil,nil,nil,nil,nil].

export make_wkr_info/2.
make_wkr_info(_A,_B) :-
        struct_lookup_subst(
            [host_name,host_ip,login_name,login_pw,port,std_in,std_out,pid],
            [nil,nil,nil,nil,nil,nil,nil,nil],_B,_C),
        _A'=..'[wkr_info|_C].

export xmake_wkr_info/2.
xmake_wkr_info(wkr_info(_A,_B,_C,_D,_E,_F,_G,_H),[_A,_B,_C,_D,_E,_F,_G,_H]).


%--- tsk_env defStruct ---

export access_tsk_env/3.
export set_tsk_env/3.
access_tsk_env(wkr_use,_A,_B) :- arg(1,_A,_B).
set_tsk_env(wkr_use,_A,_B) :- mangle(1,_A,_B).

access_tsk_env(jobID,_A,_B) :- arg(2,_A,_B).
set_tsk_env(jobID,_A,_B) :- mangle(2,_A,_B).

access_tsk_env(userID,_A,_B) :- arg(3,_A,_B).
set_tsk_env(userID,_A,_B) :- mangle(3,_A,_B).

access_tsk_env(read_s,_A,_B) :- arg(4,_A,_B).
set_tsk_env(read_s,_A,_B) :- mangle(4,_A,_B).

access_tsk_env(write_s,_A,_B) :- arg(5,_A,_B).
set_tsk_env(write_s,_A,_B) :- mangle(5,_A,_B).

access_tsk_env(state,_A,_B) :- arg(6,_A,_B).
set_tsk_env(state,_A,_B) :- mangle(6,_A,_B).

export make_tsk_env/1.
make_tsk_env(_A) :- _A'=..'[tsk_env,no_worker,0,nil,nil,nil,nil].

export make_tsk_env/2.
make_tsk_env(_A,_B) :-
        struct_lookup_subst([wkr_use,jobID,userID,read_s,write_s,state],
            [no_worker,0,nil,nil,nil,nil],_B,_C),
        _A'=..'[tsk_env|_C].

export xmake_tsk_env/2.
xmake_tsk_env(tsk_env(_A,_B,_C,_D,_E,_F),[_A,_B,_C,_D,_E,_F]).

endmod.

module utilities.
typeProperties(job_rec,[task,taskArgs,mod,state,jobID,userID,userAreaPath]).
noteOptionValue(job_rec,_A,_B,_C) :- set_job_rec(_A,_C,_B).
typeProperties(wkr_rec,[read_s,write_s,state,workerID,worker_info]).
noteOptionValue(wkr_rec,_A,_B,_C) :- set_wkr_rec(_A,_C,_B).
typeProperties(wkr_info,
    [host_name,host_ip,login_name,login_pw,port,std_in,std_out,pid]).
noteOptionValue(wkr_info,_A,_B,_C) :- set_wkr_info(_A,_C,_B).
typeProperties(tsk_env,[wkr_use,jobID,userID,read_s,write_s,state]).
noteOptionValue(tsk_env,_A,_B,_C) :- set_tsk_env(_A,_C,_B).
endmod.
