-module(sflog_server).
-behaviour(gen_server).

-export([start_link/0, set_path/2, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([read_channels/1]).

-define(FLUSH_INTERVAL, 1000).                  % 1 second
-define(ROTATE_COUNT, 60).                      % Once in 1 minutes
-define(DEFAULT_LOG_SIZE, 10 * 1024 * 1024).    % 10Mb
-define(DEFAULT_BACKUP_FILES, 5).    


start_link() ->
    LogPath = get_env(logdir, "/tmp"),
    LogFile = get_env(logfile),
    Channels = case get_env(channels) of
        undefined -> [{default, LogFile}];
        Chnls -> read_channels(Chnls)
    end,
    gen_server:start_link({local, sflog}, ?MODULE, [{LogPath, Channels}], []).

init([{LogPath, AllChannels}]) ->
    process_flag(trap_exit, true),
    InitLog = fun(Filename, Opts, State) ->
        LogFile = lists:flatten([LogPath, "/", Filename, ".log"]),
        S = check_log_file(LogFile),
        {LogFile, S, Opts, State}
    end,
    Now = calendar:local_time(),
    LogFiles = [{Channel, InitLog(FN, Opts, {0, Now})} || {Channel, FN, Opts} <- AllChannels],
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {ok, {LogFiles}}.

read_channels(AllChannels) ->
    Cook = fun
        ({Channel, FN})         -> {Channel, FN, [{rotate, size}]};
        ({Channel, FN, Opts})   -> {Channel, FN, Opts}
    end,
    [Cook(Channel) || Channel <- AllChannels].

%% Server

handle_call({log, {Channel, Level, Msg, Args}}, _From, State) -> 
    {AllChannels} = State, 
    {LogFile, S, _Opts, _State} = case proplists:get_value(Channel, AllChannels) of
        undefined -> proplists:get_value(default, AllChannels);
        Ch -> Ch
    end,
    write_log(Level, Msg, Args, S, LogFile),
    Reply = ok,
    {reply, Reply, State};

handle_call({set_path, {LogFile, S, _Opts}}, _From, _State) ->
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

handle_info({flush}, {AllChannels})  -> 
    S2 = [flush_file(Channel) || Channel <- AllChannels],
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {noreply, {S2}};
handle_info(_Info, State)  -> {noreply, State}.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason, {AllChannels}) -> 
    [file:close(S) || {_Channel, {_File, S, _Opts, _State}} <- AllChannels],
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

do_n_rotate(LogFile, Max) ->
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

size_rotate(LogFile) ->
    case filelib:file_size(LogFile) > get_env(max_log_size, ?DEFAULT_LOG_SIZE) of
        true -> do_n_rotate(LogFile, get_env(max_backup_files, ?DEFAULT_BACKUP_FILES));
        false -> none
    end.

flush_file({Channel, {LogFile, S, Opts, {N, Date}}}) ->
    file:close(S),
    {N2, D2} = check_rotate(LogFile, Opts, N, Date),
    S2 = check_log_file(LogFile),    
    {Channel, {LogFile, S2, Opts, {N2, D2}}}.

check_rotate(LogFile, Opts, N, Date) ->
    case N > ?ROTATE_COUNT of
        true -> 
            manage_rotate(LogFile, Opts, Date), 
            {0, calendar:local_time()};
        false -> 
            {N+1, Date}
    end.

manage_rotate(LogFile, Opts, Date) ->
    case proplists:get_value(rotate, Opts, none) of
        size  -> size_rotate(LogFile);
        daily -> daily_rotate(LogFile, Date);
        none  -> ok
    end.

daily_rotate(LogFile, Date) ->
    NewDate = calendar:local_time(),
    {{_,_M,OldDay}, {_,_OldMi,_}} = Date,
    {{_,_M,NewDay}, {_,_NewMi,_}} = NewDate,
    case OldDay == NewDay of
        true -> none;
        false -> do_rotate_daily(LogFile)
    end.

do_rotate_daily(LogFile) ->
    case filelib:file_size(LogFile) > 0 of
        true -> 
            {{Y,M,D}, {_H,_Mi,_}} = calendar:local_time(),
            NewFile = io_lib:format("~s.~4..0B.~2..0B.~2..0B", [LogFile, Y,M,D]),
%            io:format("ROTATE: ~p => ~p~n", [LogFile, NewFile]),
            file:rename(LogFile, NewFile);
        false -> 
            none
    end.
    