-module(sflog_test).

-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").

-define(LOGFILE, "/tmp/sflog.log").

chop(Msg) ->
    re:replace(Msg, "\n$", "", [{return, list}]).

log_test() ->
    Msg = "Test",
    ok = sflog:debug(Msg),
    {ok, Bin} = file:read_file(?LOGFILE),
    [Level, _Date, _Time | Text] = string:tokens(binary_to_list(Bin), " "),
    ?assertEqual(Level, "[debug]"),
    ?assertEqual(Msg, chop(Text)).

setup() ->
    application:start(sflog).

cleanup() -> 
    file:delete(?LOGFILE).

test_all() ->
    setup(),
    eunit:test(?MODULE, [verbose]),
    cleanup().