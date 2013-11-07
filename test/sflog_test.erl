-module(sflog_test).

-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").

-define(LOGFILE, "/tmp/sflog.log").
-define(CH_LOGFILE, "/tmp/details.log").

chop(Msg) ->
    re:replace(Msg, "\n$", "", [{return, list}]).

log_test() ->
    Msg = "Test",
    ok = sflog:debug(Msg),
    {ok, Bin} = file:read_file(?LOGFILE),
    [Level, _Date, _Time | Text] = string:tokens(binary_to_list(Bin), " "),
    ?assertEqual(Level, "[debug]"),
    ?assertEqual(Msg, chop(Text)).

channel_test() ->
    Msg = "Test",
    ok = sflog:debug(detail, Msg, []),
    {ok, Bin} = file:read_file(?CH_LOGFILE),
    [Level, _Date, _Time | Text] = string:tokens(binary_to_list(Bin), " "),
    ?assertEqual(Level, "[debug]"),
    ?assertEqual(Msg, chop(Text)).


setup() ->
    application:start(sflog).

cleanup() -> 
    file:delete(?LOGFILE),
    file:delete(?CH_LOGFILE).

test_all() ->
    setup(),
    eunit:test(?MODULE, [verbose]),
    cleanup().