-module(sflog_server).
-behaviour(gen_server).

-export([start_link/0, set_path/2, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(FLUSH_INTERVAL, 1000).                  % 1 second
-define(ROTATE_COUNT, 60).                      % Once in 1 minutes
-define(DEFAULT_LOG_SIZE, 10 * 1024 * 1024).    % 10Mb
-define(DEFAULT_BACKUP_FILES, 5).    

-define(LEVELS, [{debug, 1}, {info, 2}, {warn, 3}, {error, 4}, {crit, 5}, {none, 6}]).

-record(channel, {
    logfile,
    logname,
    logpath,
    fh,
    date,
    counter = 0,
    rotate = size,
    level = debug
}).


start_link() ->
    gen_server:start_link({local, sflog}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    LogPath = get_env(logdir, "/tmp"),
    ALLChannels = case get_env(channels) of
        undefined -> [{default, get_opts(LogPath, get_env(logfile), [])}];
        Chnls -> read_channels(LogPath, Chnls)
    end,    
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {ok, {ALLChannels}}.

read_channels(LogPath, AllChannels) ->
    Cook = fun
        ({Channel, FN})         -> {Channel, get_opts(LogPath, FN, [])};
        ({Channel, FN, Opts})   -> {Channel, get_opts(LogPath, FN, Opts)}
    end,
    [Cook(Channel) || Channel <- AllChannels].

get_level(Level) ->
    proplists:get_value(Level, ?LEVELS, 0).

get_opts(LogPath, Filename, Opts) ->
    LogFile = lists:flatten([LogPath, "/", Filename, ".log"]),
    #channel{
        logfile = LogFile,
        fh = check_log_file(LogFile),
        logpath = LogPath,
        logname = Filename,
        rotate = proplists:get_value(rotate, Opts, size),
        level = proplists:get_value(log_level, Opts, debug),
        date = calendar:local_time()
    }.

%% Server

handle_call({log, {Channel, Level, Msg, Args}}, _From, State) -> 
    {AllChannels} = State, 
    Ch = case proplists:get_value(Channel, AllChannels) of
        undefined -> proplists:get_value(default, AllChannels);
        Chln -> Chln
    end,
    case get_level(Level) >= get_level(Ch#channel.level) of
        true  -> write_log(Level, Msg, Args, Ch#channel.fh);
        false -> ok
    end,
    {reply, ok, State};

handle_call({set_path, {_LogFile, _S}}, _From, State) ->
%%  ToDo: set path for all channels
    {reply, ok, State};

handle_call({set_log_level, {Channel, Level}}, _From, {AllChannels}) ->
    State = case proplists:get_value(Channel, AllChannels) of
        undefined -> 
            AllChannels;
        Ch -> 
            lists:keyreplace(Channel, 1, AllChannels, {Channel, Ch#channel{level = Level}})
    end,
    {reply, ok, {State}};

handle_call({status}, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};

handle_call(Request, _From, State) -> 
    io:format("handle_call: unknown message: ~p", [Request]),
    {reply, error, State}.

format_date() ->
    {{_,M,D}, {H,Mi,S}} = erlang:localtime(),
    io_lib:format("~.2.0w/~.2.0w ~.2.0w:~.2.0w:~.2.0w", [M,D, H,Mi,S]).

write_log(Level, Msg, Args, S) ->
    Format = "[~.5s] ~s " ++ Msg ++ "~n",
    io:format(S, Format, [Level, format_date()] ++ Args),
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
    [file:close(Ch#channel.fh) || {_Channel, Ch} <- AllChannels],
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

flush_file({Channel, Ch}) ->
    file:close(Ch#channel.fh),
    {N2, D2} = check_rotate(Ch#channel.logfile, Ch#channel.rotate, Ch#channel.counter, Ch#channel.date),
    {Channel, Ch#channel{
        fh = check_log_file(Ch#channel.logfile),
        counter = N2,
        date = D2
    }}.

check_rotate(LogFile, Rotate, N, Date) ->
    case N > ?ROTATE_COUNT of
        true -> 
            manage_rotate(LogFile, Rotate, Date), 
            {0, calendar:local_time()};
        false -> 
            {N+1, Date}
    end.

manage_rotate(LogFile, Rotate, Date) ->
    case Rotate of
        size  -> size_rotate(LogFile);
        daily -> daily_rotate(LogFile, Date);
        none  -> ok;
        _ -> ok
    end.

daily_rotate(LogFile, Date) ->
    NewDate = calendar:local_time(),
    {{_,_M,OldDay}, {_,_OldMi,_}} = Date,
    {{_,_M,NewDay}, {_,_NewMi,_}} = NewDate,
    case OldDay == NewDay of
        true -> none;
        false -> do_rotate_daily(LogFile, Date)
    end.

do_rotate_daily(LogFile, Date) ->
    case filelib:file_size(LogFile) > 0 of
        true -> 
            {{Y,M,D}, {_H,_Mi,_}} = Date,
            NewFile = io_lib:format("~s.~4..0B.~2..0B.~2..0B", [LogFile, Y,M,D]),
%            io:format("ROTATE: ~p => ~p~n", [LogFile, NewFile]),
            file:rename(LogFile, NewFile);
        false -> 
            none
    end.
    