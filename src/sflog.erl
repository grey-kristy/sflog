-module(sflog).

-export([err/1, err/2, info/1, info/2, debug/1, debug/2]).

%% Interface

debug(Msg) when is_list(Msg) ->
    log(debug, Msg, []).

debug(Msg, Args) when is_list(Msg) ->
    log(debug, Msg, Args).

info(Msg) when is_list(Msg) ->
    log(info, Msg, []).

info(Msg, Args) when is_list(Msg) ->
    log(info, Msg, Args).

err(Msg, Args) when is_list(Msg) ->
    log(error, Msg, Args).

err(Msg) when is_list(Msg) ->
    log(error, Msg, []).

log(Level, Msg, Args) ->
    gen_server:call(sflog, {log, {Level, Msg, Args}}).


