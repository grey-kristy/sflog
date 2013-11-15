-module(sflog).

-export([start/0]).
-export([err/1, err/2, err/3, info/1, info/2, info/3, debug/1, debug/2, debug/3]).


start() ->
    application:start(sflog).

%% Interface

debug(Msg) when is_list(Msg) ->
    log(default, debug, Msg, []).

debug(Msg, Args) when is_list(Msg) ->
    log(default, debug, Msg, Args).

debug(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, debug, Msg, Args).

info(Msg) when is_list(Msg) ->
    log(default, info, Msg, []).

info(Msg, Args) when is_list(Msg) ->
    log(default, info, Msg, Args).

info(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, info, Msg, Args).

err(Msg) when is_list(Msg) ->
    log(default, error, Msg, []).

err(Msg, Args) when is_list(Msg) ->
    log(default, error, Msg, Args).

err(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, error, Msg, Args).


%% Internal functions

log(none, _Level, _Msg, _Args) -> ok;
log(Channel, Level, Msg, Args) ->
    gen_server:call(sflog, {log, {Channel, Level, Msg, Args}}).


