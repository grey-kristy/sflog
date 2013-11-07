-module(sflog_server).
-behaviour(gen_server).

-export([start_link/0, set_path/2, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).


-define(FLUSH_INTERVAL, 1000).                  % 1 second
-define(ROTATE_COUNT, 600).                     % Once in 10 minutes
-define(DEFAULT_LOG_SIZE, 10 * 1024 * 1024).    % 10Mb
-define(DEFAULT_BACKUP_FILES, 5).    


start_link() ->
    LogPath = get_env(logdir, "/tmp"),
    LogFile = get_env(logfile),
    Channels = case get_env(channels) of
        undefined -> [{default, LogFile}];
        Chnls -> Chnls
    end,
    gen_server:start_link({local, sflog}, ?MODULE, [{LogPath, Channels}], []).

init([{LogPath, AllChannels}]) ->
    process_flag(trap_exit, true),
    InitLog = fun(Filename) ->
        LogFile = lists:flatten([LogPath, "/", Filename, ".log"]),
        S = check_log_file(LogFile),
        {LogFile, S}
    end,
    LogFiles = [{Channel, InitLog(FN)} || {Channel, FN} <- AllChannels],
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {ok, {LogFiles, 0}}.


%% Server

handle_call({log, {Channel, Level, Msg, Args}}, _From, State) -> 
    {AllChannels, _N} = State, 
    {LogFile, S} = case proplists:get_value(Channel, AllChannels) of
        undefined -> proplists:get_value(default, AllChannels);
        Ch -> Ch
    end,
    write_log(Level, Msg, Args, S, LogFile),
    Reply = ok,
    {reply, Reply, State};

handle_call({set_path, {LogFile, S}}, _From, _State) ->
    {reply, ok, {LogFile, S, 0}};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};

handle_call(_Request, _From, State) -> 
    io:format("handle_call: unknown message: ~p", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

format_date() ->
    {{_,M,D}, {H,Mi,S}} = erlang:localtime(),
    io_lib:format("~.2.0w/~.2.0w ~.2.0w:~.2.0w:~.2.0w", [M,D, H,Mi,S]).

write_log(Level, Msg, Args, S, _LogFile) ->
%   {ok, S} = file:open(_LogFile, [append]),
    Format = "[~.5s] ~s " ++ Msg ++ "~n",
    io:format(S, Format, [Level, format_date()] ++ Args),
%   file:close(S),
    ok.

%debug_out(Msg, Args) ->
%   {ok, S} = file:open("/tmp/my.out.log", [append]), io:format(S, Msg, Args), file:close(S).

handle_cast(_Msg, N)  -> {noreply, N}.

handle_info({flush}, {AllChannels, N})  -> 
    S1 = [flush_file(Channel, File, S, N) || {Channel, {File, S}} <- AllChannels],
    S2 = [{Channel, {File, S}} || {Channel, File, S, _N} <- S1],
    N2 = lists:min([CN || {_Channel, _File, _S, CN} <- S1]),
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {noreply, {S2, N2}};
handle_info(_Info, State)  -> {noreply, State}.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason, {AllChannels, _N}) -> 
    [file:close(S) || {_Channel, {_File, S}} <- AllChannels],
    ok.

%% Internal functions

die(Format, Args) ->
    io:format(Format, Args),
    erlang:error(Format, Args).

get_env(Key) -> get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(Key) of
        {ok, V} -> V;
        _ -> Default
    end.

set_path(LogDir, LogFile) ->
    LogPath = lists:flatten([LogDir, "/", LogFile, ".log"]),
    S = check_log_file(LogFile),
    gen_server:call(sflog, {set_path, {LogPath, S}}).

check_log_file(LogFile) ->
    case file:open(LogFile, [append]) of
        {error, Error} ->
            die("Can't open file ~s ~p~n", [LogFile, Error]);
        {ok, S} -> S
    end.

rotate_files(LogFile, Max) ->
    NFile = fun(N) ->
        case N of
            0 -> LogFile;
            _ -> LogFile ++ "." ++ integer_to_list(N)
        end
    end,
    Rotate = fun(N) ->
        case filelib:is_regular(NFile(N-1)) of
            true  -> file:rename(NFile(N-1), NFile(N));
            false -> none
        end
    end,
    lists:foreach(Rotate, lists:seq(Max,1,-1)).

do_rotate(LogFile) ->
    case filelib:file_size(LogFile) > get_env(max_log_size, ?DEFAULT_LOG_SIZE) of
        true -> rotate_files(LogFile, get_env(max_backup_files, ?DEFAULT_BACKUP_FILES));
        false -> none
    end.

flush_file(Channel, LogFile, S, N) ->
    file:close(S),
    N2 = case N > ?ROTATE_COUNT of
        true -> do_rotate(LogFile), 0;
        false -> N+1
    end,
    S2 = check_log_file(LogFile),    
    {Channel, LogFile, S2, N2}.
