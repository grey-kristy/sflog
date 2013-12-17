-module(sflog_test).

-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").

-define(LOGFILE, "/tmp/sflog.log").
-define(CH_LOGFILE, "/tmp/details.log").

chop(Msg) ->
    re:replace(Msg, "\n$", "", [{return, list}]).

get_last_line(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Strings = binary:split(Bin, <<"\n">>, [trim, global]),
%    ?debugFmt("file ~p ~p", [FileName, Strings]), 
    Str = lists:nth(length(Strings), Strings),
    binary_to_list(Str).

log_test() ->
    Test = 45,
    ok = sflog:debug("label ~p", [Test]),
    Str = get_last_line(?LOGFILE),
    [Level, _Date, _Tim, Label, Value |_] = string:tokens(Str, " "),
    ?assertEqual(Level, "[debug]"),
    ?assertEqual(Label, "label"),
    ?assertEqual(list_to_integer(Value), Test).

log_err_test() ->
    Msg = "Test",
    ok = sflog:err(Msg),
    Str = get_last_line(?LOGFILE),
    [Level, _Date, _Time | Text] = string:tokens(Str, " "),
    ?assertEqual(Level, "[error]"),
    ?assertEqual(Msg, chop(Text)).

log_info_test() ->
    Msg = "LogLevelTest",
    ok = sflog:info(Msg),
    Str = get_last_line(?LOGFILE),
    [Level, _, _Date, _Time | Text] = string:tokens(Str, " "),
    ?assertEqual(Level, "[info"),
    ?assertEqual(Msg, chop(Text)).

channel_test() ->
    Msg = "Test",
    ok = sflog:debug(detail, Msg, []),
    Str = get_last_line(?CH_LOGFILE),
    [Level, _Date, _Time | Text] = string:tokens(Str, " "),
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
