-module(sflog).

-export([start/0, status/0]).

%% API exports

-export([crit/1, crit/2, crit/3, err/1, err/2, err/3, warn/1, warn/2, warn/3]).
-export([info/1, info/2, info/3, debug/1, debug/2, debug/3]).
-export([set_log_level/1, set_log_level/2]).

start() ->
    application:start(sflog).

%% API

debug(Msg) when is_list(Msg) ->
    log(default, debug, Msg, []).

debug(Channel, Msg) when is_atom(Channel) ->
    log(Channel, debug, Msg, []);
debug(Msg, Args) when is_list(Msg) ->
    log(default, debug, Msg, Args).

debug(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, debug, Msg, Args).

info(Msg) when is_list(Msg) ->
    log(default, info, Msg, []).

info(Channel, Msg) when is_atom(Channel) ->
    log(Channel, info, Msg, []);
info(Msg, Args) when is_list(Msg) ->
    log(default, info, Msg, Args).

info(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, info, Msg, Args).

warn(Msg) when is_list(Msg) ->
    log(default, warn, Msg, []).

warn(Channel, Msg) when is_atom(Channel) ->
    log(Channel, warn, Msg, []);
warn(Msg, Args) when is_list(Msg) ->
    log(default, warn, Msg, Args).

warn(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, warn, Msg, Args).

err(Msg) when is_list(Msg) ->
    log(default, error, Msg, []).

err(Channel, Msg) when is_atom(Channel) ->
    log(Channel, error, Msg, []);
err(Msg, Args) when is_list(Msg) ->
    log(default, error, Msg, Args).

err(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, error, Msg, Args).

crit(Msg) when is_list(Msg) ->
    log(default, crit, Msg, []).

crit(Channel, Msg) when is_atom(Channel) ->
    log(Channel, crit, Msg, []);
crit(Msg, Args) when is_list(Msg) ->
    log(default, crit, Msg, Args).

crit(Channel, Msg, Args) when is_atom(Channel), is_list(Msg) ->
    log(Channel, crit, Msg, Args).

set_log_level(Level) when is_atom(Level) ->
    set_log_level(default, Level).
set_log_level(Channel, Level) when is_atom(Channel), is_atom(Level) ->
    gen_server:call(sflog, {set_log_level, {Channel, Level}}).

%% Internal functions

log(none, _Level, _Msg, _Args) -> ok;
log(Channel, Level, Msg, Args) ->
    gen_server:call(sflog, {log, {Channel, Level, Msg, Args}}).

status() ->
    gen_server:call(sflog, {status}).
