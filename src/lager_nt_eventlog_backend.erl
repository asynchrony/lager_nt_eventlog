%% @doc NT event log backend for lager.

-module(lager_nt_eventlog_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {handle, level}).

-include_lib("lager/include/lager.hrl").

%% @private
init([Source, Level]) ->
    case nt_eventlog:register_event_source(Source) of
        Handle when is_integer(Handle) ->
            {ok, #state{level=lager_util:level_to_num(Level),
                        handle = Handle}};
        Error ->
            Error
    end.

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {_Date, _Time}, [_LevelStr, Location, Message]}, #state{handle = Handle} = State) ->
    nt_eventlog:report_event(Handle, Level, lists:flatten([Location, Message])),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{handle = Handle}) ->
    nt_eventlog:deregister_event_source(Handle).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

