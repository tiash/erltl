-record(config, { dir,
                  opts }).

-record(global_state, { working_dir }).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(RES(RR),(?RES(?MODULE,RR))).
-define(RES(MOD,RR),(begin ?DEBUG("enter... ~p~n",[MOD]), __Res__=(catch RR), ?DEBUG("~p.RES = ~p~n",[MOD,__Res__]),__Res__ end)).


