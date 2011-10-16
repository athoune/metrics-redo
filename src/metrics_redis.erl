-module(metrics_redis).
-author('mathieu@garambrogne.net').

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2,
handle_info/2, terminate/2, code_change/3]).

-export([value/1, flush/0, ping/0]).

init([]) ->
    {ok, _} = redo:start_link(),
    {ok, []}.

handle_call(_Request, State) ->
    {ok, State}.

handle_event({incr_counter, Key, Incr}, State) ->
    [I] = io_lib:format("~B", [Incr]),
    <<"OK">> = redo:cmd(["INCRBY", atom_to_list(Key), I]),
    io:format("popo", []),
    {ok, State};

handle_event(Msg, State) ->
    error_logger:warning_msg("Cast missing pattern : ~p~n", [Msg]),
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

value(Key) ->
    V = redo:cmd(["GET", atom_to_list(Key)]),
    V.

flush() ->
    case redo:cmd(["FLUSHDB"]) of
        <<"OK">> -> ok;
        Msg      -> {error, Msg}
    end.

ping() ->
    case redo:cmd(["PING"]) of
        <<"PONG">> -> ok;
        Msg        -> {error, Msg}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

redis_test_() ->
        {setup,
            fun() ->
                application:load(sasl),
                application:set_env(sasl, sasl_error_logger, {file, "metrics.log"}),
                error_logger:tty(false),
                error_logger:logfile({open, "metrics.log"}),
                ok = application:start(metrics)
            end,
            fun(_) ->
                ok = application:stop(metrics)
            end,
            fun() ->
                ok = metrics:add_writer(metrics_redis),
                ok = metrics_redis:ping(),
                ok = metrics_redis:flush(),
                metrics_counter:incr(popo, 42),
                timer:sleep(10),
                ?assertEqual(<<"42">>, metrics_redis:value(popo))
            end
        }.

-endif.

