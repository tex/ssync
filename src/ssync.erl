%% ssync: Rebarized Erlang code always compiled
%%
%% Copyright (c) 2012 Milan Svoboda (milan.svoboda@centrum.cz)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

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
    gen_server:cast(?MODULE, {50, compile, []});

rebar(doc) ->
    gen_server:cast(?MODULE, {35, doc, []});

rebar('get-deps') ->
    gen_server:cast(?MODULE, {25, 'get-deps', []}).

rebar(compile, skip_deps) ->
    gen_server:cast(?MODULE, {50, compile, [skip_deps]});

rebar(doc, skip_deps) ->
    gen_server:cast(?MODULE, {35, doc, [skip_deps]}).

reload(ModuleName) ->
    gen_server:cast(?MODULE, {reload, ModuleName}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    watch(ssync_rebar_config:get_all_dirs(".")),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State, get_action_timeout()}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({reload, ModuleName}, State) ->
    Ext = string:to_lower(filename:extension(ModuleName)),
    case Ext of
        ".beam" ->
            Module = list_to_atom(filename:rootname(ModuleName)),
            code:purge(Module),
            {module, Module} = code:load_file(Module),
            Summary = io_lib:format("ssync: reloaded (~s)", [Module]),
            ssync_notify:notify(Summary, []);
        _ -> ok
    end,
    {noreply, State, get_action_timeout()};

handle_cast(Action = {_, _, _}, State) ->
    {noreply, [Action | State], get_action_timeout()};

handle_cast(Msg, State) ->
  ?log({unknown_message, Msg}),
  {noreply, State, get_action_timeout()}.

handle_info(timeout, ActionList) ->
    Actions = lists:usort(ActionList),
    lists:foreach(fun make_action/1, Actions),
    {noreply, []};

handle_info(Info, State) ->
  ?log({unknown_message, Info}),
  {noreply, State, get_action_timeout()}.

terminate(_Reason, State) ->
    {close, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

make_action({_, compile, []}) ->
    ssync_notify:notify("ssync: build started", []),
    ssync_cmd:cmd("rebar", ["compile"], fun parse_output/2),
    ssync_notify:notify("ssync: build finished", []);

make_action({_, compile, [skip_deps]}) ->
    ssync_notify:notify("ssync: build started (skip deps)", []),
    ssync_cmd:cmd("rebar", ["skip_deps=true", "compile"], fun parse_output/2),
    ssync_notify:notify("ssync: build finished (skip deps)", []);

make_action({_, doc, []}) ->
    ssync_notify:notify("ssync: doc build started", []),
    ssync_cmd:cmd("rebar", ["doc"], fun parse_output/2),
    ssync_notify:notify("ssync: doc build finished", []);

make_action({_, doc, [skip_deps]}) ->
    ssync_notify:notify("ssync: doc build started (skip deps)", []),
    ssync_cmd:cmd("rebar", ["skip_deps=true", "doc"], fun parse_output/2),
    ssync_notify:notify("ssync: doc build finished (skip deps)", []);

make_action({_, 'get-deps', []}) ->
    ssync_notify:notify("ssync: get-deps started", []),
    ssync_cmd:cmd("rebar", ["get-deps"], fun parse_output/2),
    ssync_notify:notify("ssync: get-deps finished", []),
    watch(ssync_rebar_config:get_all_dirs(".")).

watch({Path, reload = CallbackName}) ->
    code:add_path(Path),
    watch_recursive(Path, CallbackName);

watch({Path, watch_rebar_config = CallbackName}) ->
    erlinotify:watch(Path, get_callback(CallbackName));

watch({Path, CallbackName}) ->
    watch_recursive(Path, CallbackName);

watch([]) ->
    ok;

watch([F|R]) ->
    watch(F),
    watch(R).

watch_recursive(Path, CallbackName) ->
    Callback = get_callback(CallbackName),
    Dirs = subdirs(Path) ++ [{dir, Path}],
    % files that will created later are handled with `do_watch_new_dirs`
    [erlinotify:watch(X, Callback) ||
        {dir, X} <- lists:flatten(Dirs), filelib:is_file(X) == true ], ok.

get_callback(reload) ->
    fun do_reload/1;
get_callback(compile) ->
    fun do_compile/1;
get_callback(doc) ->
    fun do_doc/1;
get_callback(watch_rebar_config) ->
    fun do_watch_rebar_config/1;
get_callback(watch_new_dirs) ->
    fun do_watch_new_dirs/1.

subdirs(Path) ->
    [[{dir, Y} | subdirs(Y)] ||
            Y <- filelib:wildcard(filename:join([Path, "*"])),
            filelib:is_dir(Y) ].

print_project(_, []) ->
    ok;

print_project(Project, Msgs) ->
    ssync_notify:notify(io_lib:format("ssync: build (~s)", [Project]), Msgs).

parse_output(eof, {Project, Msgs} = _Acc) ->
    print_project(Project, lists:reverse(Msgs));

parse_output({eol, BinMsg}, Acc) ->
    case re:run(BinMsg, "==> (\\S+)", [{capture, [1], list}]) of
        {match, [Project]} ->
            case Acc of
                {Project, _} ->
                    Acc;
                {PrevProject, PrevMsgs} ->
                    print_project(PrevProject, lists:reverse(PrevMsgs)),
                    {Project, []};
                _ ->
                    {Project, []}
            end;
        nomatch ->
            {Project, Msgs} = Acc,
            {Project, [BinMsg | Msgs]}
    end;

parse_output({noeol, BinMsg}, {Project, Acc}) ->
    StartLine = hd(Acc),
    {Project, [<<StartLine/binary, BinMsg/binary>> | tl(Acc)]}.

do_compile({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:watch(FN, fun do_compile/1);

do_compile({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:unwatch(FN);

do_compile({File, file, close_write, _Cookie, Name} = _Info) ->
    Ext = string:to_lower(filename:extension(Name)),
    {ok, Exts} = application:get_env(ssync, extensions),
    case lists:any(fun(X) -> X == Ext end, Exts) of
        true ->
            DepsDir = filename:join([".", ssync_rebar_config:get_deps_dir(".")]),
            case lists:prefix(DepsDir, File) of
                true  -> rebar(compile);
                false -> rebar(compile, skip_deps)
            end;
        false ->
            ok
    end;

do_compile({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

do_doc({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:watch(FN, fun do_doc/1);

do_doc({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    erlinotify:unwatch(FN);

do_doc({File, file, close_write, _Cookie, Name} = _Info) ->
    Ext = string:to_lower(filename:extension(Name)),
    {ok, Exts} = application:get_env(ssync, extensions),
    case lists:any(fun(X) -> X == Ext end, Exts) of
        true ->
            DepsDir = filename:join([".", ssync_rebar_config:get_deps_dir(".")]),
            case lists:prefix(DepsDir, File) of
                true  -> rebar(doc);
                false -> rebar(doc, skip_deps)
            end;
        false ->
            ok
    end;

do_doc({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
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

do_watch_rebar_config({_File, file, move_to, _Cookie, "rebar.config"} = _Info) ->
    rebar('get-deps');

do_watch_rebar_config({_File, file, close_write, _Cookie, "rebar.config"} = _Info) ->
    rebar('get-deps');

do_watch_rebar_config({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

do_watch_new_dirs({File, dir, move_to, Cookie, Name} = _Info) ->
    do_watch_new_dirs({File, dir, create, Cookie, Name});

do_watch_new_dirs({File, dir, create, _Cookie, Name} = _Info) ->
    Dirs = lists:flatten(ssync_rebar_config:get_all_dirs(".")),
    FN = filename:join(File, Name),
    watch(proplists:lookup_all(FN, Dirs)),
    rebar(compile);

do_watch_new_dirs({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

get_action_timeout() ->
    get_action_timeout(application:get_env(timeout)).

get_action_timeout({ok, Timeout}) ->
    Timeout;

get_action_timeout(undefined) ->
    1000.
