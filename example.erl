-module(example).

-export([test/0]).

test() ->
    application:start(sflog),
    sflog:debug("First log record"),
    sflog:info("First log record with ~p formated ~p", [2, argumets]),
    sflog:err("Default log file: ~ts", ["/tmp/sflog.log"]).
