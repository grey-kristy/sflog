-module(sflog).
-behaviour(gen_server).

-export([start_link/0, set_path/2, init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([err/1, err/2, info/1, info/2, debug/1, debug/2, rotate_files/2]).

-define(FLUSH_INTERVAL, 1000).                  % 1 second
-define(ROTATE_COUNT, 600).                     % Once in 10 minutes
-define(DEFAULT_LOG_SIZE, 10 * 1024 * 1024).    % 10Mb
-define(DEFAULT_BACKUP_FILES, 5).    

start_link() ->
    {ok, LogPath} = application:get_env(logdir),
    {ok, LogFile} = application:get_env(logfile),
    gen_server:start_link({local, sflog}, ?MODULE, [{LogPath, LogFile}], []).

init([{LogPath, File}]) ->
    process_flag(trap_exit, true),
    LogFile = lists:flatten([LogPath, "/", File, ".log"]),
    S = check_log_file(LogFile),
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {ok, {LogFile, S, 0}}.

set_path(LogDir, LogFile) ->
    LogPath = lists:flatten([LogDir, "/", LogFile, ".log"]),
    S = check_log_file(LogFile),
    gen_server:call(sflog, {set_path, {LogPath, S}}).

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


%% Server

check_log_file(LogFile) ->
    case file:open(LogFile, [append]) of
        {error, Error} ->
            io:format(standard_error, "~n Can't open file ~s ~p ~n", [LogFile, Error]),
            init:stop();
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

get_env(Key, Default) ->
    case application:get_env(Key) of
        {ok, V} -> V;
        _ -> Default
    end.

handle_call({log, {Level, Msg, Args}}, _From, State) -> 
    {LogFile, S, _N} = State, 
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

handle_info({flush}, State)  -> 
    {LogFile, S, N} = State,
    file:close(S),
    N2 = case N > ?ROTATE_COUNT of
        true -> do_rotate(LogFile), 0;
        false -> N+1
    end,
    S2 = check_log_file(LogFile),
    erlang:send_after(?FLUSH_INTERVAL, self(), {flush}),
    {noreply, {LogFile, S2, N2}};

handle_info(_Info, State)  -> {noreply, State}.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason, {_LogFile, S, _N}) -> 
    file:close(S), ok.
