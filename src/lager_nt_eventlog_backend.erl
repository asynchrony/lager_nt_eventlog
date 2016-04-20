%% @doc NT event log backend for lager.

-module(lager_nt_eventlog_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).
-export([config_to_id/1]).

-record(state, {id, handle, level, formatter, format_config}).

-define(DEFAULT_FORMAT, [{pid, ""}, {module, [{pid, ["@"], ""}, module, {function, [":", function], ""}, {line, [":",line], ""}], ""}, " ", message]).
-include_lib("lager/include/lager.hrl").

%% @private
init([Source, Level]) ->
    init([Source, Level, {lager_default_formatter, ?DEFAULT_FORMAT}]);
init([Source, Level, {Formatter, FormatterConfig}]) ->
    case parse_level(Level) of
        {ok, Lvl} ->
            case nt_eventlog:register_event_source(Source) of
                {ok, Handle} ->
                    {ok, #state{id = {?MODULE, Source},
                                level = Lvl,
                                handle = Handle,
                                formatter = Formatter,
                                format_config = FormatterConfig}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case parse_level(Level) of
        {ok, Lvl} ->
            {ok, ok, State#state{level = Lvl}};
        Error ->
            {ok, Error, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {_Date, _Time}, [_LevelStr, Location, Message]},
    #state{handle = Handle, level = LogLevel} = State) when Level =< LogLevel ->
    nt_eventlog:report_event(Handle, Level, lists:flatten([Location, Message])),
    {ok, State};
handle_event({log, Message}, #state{handle = Handle, level = Level, formatter = Formatter, format_config = FormatConfig} = State) ->
    case lager_util:is_loggable(Message, Level, State#state.id) of
        true ->
            nt_eventlog:report_event(Handle, lager_msg:severity_as_int(Message), Formatter:format(Message, FormatConfig)),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{handle = Handle}) ->
    nt_eventlog:deregister_event_source(Handle).

%% @private
code_change("0.9.0", {state, Handle, Level}, _Extra) ->
    {ok, #state{id = ?MODULE, handle = Handle, level = Level, formatter = lager_default_formatter, format_config = ?DEFAULT_FORMAT }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

config_to_id([Source | _]) ->
    {?MODULE, Source}.

%% @private
parse_level(Level) ->
    case parse_level(Level, 2) of
        {error, _} ->
            parse_level(Level, 1);
        Mask ->
            Mask
    end.

parse_level(Level, 2) ->
    try lager_util:config_to_mask(Level) of
        Res ->
            {ok, Res}
    catch
        _:_ ->
            {error, bad_log_level}
    end;
parse_level(Level, 1) ->
    try lager_util:level_to_num(Level) of
        Res ->
            {ok, Res}
    catch
        _:_ ->
            {error, bad_log_level}
    end.