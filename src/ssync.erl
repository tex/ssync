%% Copyright (c) 2012 Milan Svoboda
%% Released under the MIT License.

-module(ssync).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, rebar/1, reload/1]).

-export([start/0]).

-export([do_compile/1, do_reload/1, do_watch_root/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start() ->
    application:start(ets_manager),
    application:start(erlinotify),
    application:start(ssync),
    rebar('get-deps'),
    rebar(compile).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

rebar(compile) ->
    gen_server:cast(?MODULE, {compile});

rebar('get-deps') ->
    gen_server:cast(?MODULE, {'get-deps'}).

reload(ModuleName) ->
    gen_server:cast(?MODULE, {reload, ModuleName}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init(_Args) ->
    ssync(),
    {ok, []}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({compile}, State) ->
    notify:notify("ssync: build started", []),
    cmd:cmd("rebar", ["compile"], fun parse_output/2),
    notify:notify("ssync: build finished", []),
    {noreply, State};
handle_cast({'get-deps'}, State) ->
    notify:notify("ssync: get-deps started", []),
    cmd:cmd("rebar", ["get-deps"], fun parse_output/2),
    notify:notify("ssync: get-deps finished", []),
    {noreply, State};
handle_cast({reload, ModuleName}, State) ->
    Ext = string:to_lower(filename:extension(ModuleName)),
    case Ext of
        ".beam" ->
            Module = list_to_atom(filename:rootname(ModuleName)),
            code:purge(Module),
            {module, Module} = code:load_file(Module);
        _ -> ok
    end,
    {noreply, State};
handle_cast(Msg, State) ->
  ?log({unknown_message, Msg}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
  ?log({unknown_message, Info}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    {close, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ssync() ->
    {ok, Dirs} = application:get_env(dirs),
    {ok, SubDirs} = application:get_env(sub_dirs),
    [watch(Dir, SubDir, Fun) || Dir <- Dirs, {SubDir, Fun} <- SubDirs],
    ok.

watch(".", SubDir, Fun) ->
    watch([SubDir], Fun);

watch(Dir, ".", Fun) ->
    watch([Dir], Fun);

watch(Dir, SubDir, Fun) ->
    watch(filelib:wildcard(filename:join([Dir, "*", SubDir])), Fun).

watch(Paths, Callback) ->
    lists:foreach(
        fun(Path) ->
                lists:foreach(
                    fun(X) ->
                            case filelib:is_dir(X) of
                                true ->
                                    erlinotify:watch(X, Callback);
                                _ ->
                                    ok
                            end
                    end, dirs_recursive(Path) )
        end, Paths ).

dirs_recursive(Path) ->
    lists:map(fun(TaggedDir) -> element(2, TaggedDir) end,
              tagged_dirs_recursive(Path) ).

tagged_dirs_recursive([]) ->
    [];

tagged_dirs_recursive(Path) ->
    lists:flatten([
            {directory, Path},
            lists:map(fun(X) -> tagged_dirs_recursive(X) end,
                      get_dirs(Path) ) ]).

get_dirs(Path) ->
    lists:filter(fun(X) -> filelib:is_dir(X) end,
                 filelib:wildcard(filename:join(Path, "*")) ).

print_project(_, []) ->
    ok;

print_project(Project, Msgs) ->
    notify:notify(io_lib:format("ssync: build (~s)", [Project]), Msgs).

parse_output(eof, {Project, Msgs} = _Acc) ->
    print_project(Project, Msgs);

parse_output(BinMsg, Acc) ->
    Msg = binary_to_list(BinMsg),
    case re:run(Msg, "==> \\S+") of
        {match, [{Start, Length}]} ->
            Project = string:substr(Msg, Start + 1 + 4, Length - 4),
            case Acc of
                {Project, _} ->
                    Acc;
                {PrevProject, PrevMsgs} ->
                    print_project(PrevProject, PrevMsgs),
                    {Project, []};
                _ ->
                    {Project, []}
            end;
        nomatch ->
            {Project, Msgs} = Acc,
            {Project, Msgs ++ [Msg]}
    end.

do_compile({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:watch(FN, fun do_compile/1);

do_compile({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:unwatch(FN);

do_compile({_File, file, close_write, _Cookie, Name} = _Info) ->
    Ext = string:to_lower(filename:extension(Name)),
    {ok, Exts} = application:get_env(ssync, extension),
    case lists:any(fun(X) -> X == Ext end, Exts) of
        true ->
            rebar(compile);
        false ->
            ok
    end;

do_compile({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

do_reload({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:watch(FN, fun do_reload/1);

do_reload({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:unwatch(FN);

do_reload({_File, file, move_to, _Cookie, Name} = _Info) ->
    reload(Name);

do_reload({_File, file, close_write, _Cookie, Name} = _Info) ->
    reload(Name);

do_reload({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

do_watch_root({_File, file, move_to, _Cookie, "rebar.config"} = _Info) ->
    rebar('get-deps');

do_watch_root({_File, file, close_write, _Cookie, "rebar.config"} = _Info) ->
    rebar('get-deps');

do_watch_root({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

