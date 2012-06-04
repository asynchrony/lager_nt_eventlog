-module(nt_eventlog).

-on_load(on_load/0).

-define(NT_EVENTLOG_NIF_VSN, 100).

-export([register_event_source/1, deregister_event_source/1, report_event/3]).

-type error_tuple() :: {error,
                        SourceFile::string(),
                        SourceLine::integer(),
                        ErrorCode::integer(),
                        ErrorMessage:: string()}.
-type handle() :: integer().

on_load() ->
    Lib = filename:join([code:priv_dir(lager_nt_eventlog), "lib", "nt_eventlog"]),

    Status = erlang:load_nif(Lib, ?NT_EVENTLOG_NIF_VSN),
    case Status of
        ok -> ok;
        {error, {E, Str}} ->
            error_logger:error_msg(
                "Unable to load library \"nt_eventlog.dll\"."
                " Failed with error:~n\"~p, ~s\"~n",
                [E,Str]),
            Status
    end.

-spec(register_event_source(Source::string()) -> {ok, handle()} | error_tuple()).
register_event_source(_Source) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

-spec(report_event(handle(), lager:log_level_number(), Message::string()) -> ok | error_tuple()).
report_event(_Handle, _Priority, _Message) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

-spec(deregister_event_source(integer()) -> ok | error_tuple()).
deregister_event_source(_Handle) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).
