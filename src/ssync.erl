%% Copyright (c) 2012 Milan Svoboda
%% Released under the MIT License.

-module(ssync).
-export([start/0]).
-export([watch_r/2, dirs_r/1, dirs/1]).

start() ->
    application:start(ets_manager),
    application:start(erlinotify),

    lists:foreach(
        fun({Path, Fun}) ->
                lists:foreach(
                    fun(ExpandedPath) ->
                            watch_r(ExpandedPath, Fun)
                    end,
                    filelib:wildcard(Path) )
        end,
        [{"src", fun do_compile/1},
         {"c_src", fun do_compile/1},
         {"include", fun do_compile/1},
         {"ebin", fun do_reload/1},
         {"deps/*/src", fun do_compile/1},
         {"deps/*/c_src", fun do_compile/1},
         {"deps/*/include", fun do_compile/1},
         {"deps/*/ebin", fun do_reload/1} ] ).

watch_r(Path, Callback) ->
    lists:foreach(fun(X) -> erlinotify:watch(X, Callback) end,
                  dirs_r(Path) ).

dirs_r(Path) ->
    lists:map(fun(X) -> element(2, X) end, dirs_rt(Path)).

dirs_rt([]) -> [];

dirs_rt(Path) ->
    lists:flatten([
            {directory, Path},
            lists:map(fun(X) -> dirs_rt(X) end,
                      dirs(Path) ) ]).

dirs(Path) ->
    lists:filter(fun(X) -> filelib:is_dir(X) end,
                 filelib:wildcard(filename:join(Path, "*")) ).

rebar_compile(_File, _Name) ->
    notify:notify("ssync: build started", ""),
    cmd:cmd("rebar", ["compile"], fun parse_output/2),
    notify:notify("ssync: build finished", "").

print_project(_, []) ->
    ok;

print_project(Project, Msgs) ->
    notify:notify(io_lib:format("ssync: build (~s)", [Project]), string:join(Msgs, "\n")).

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

do_compile({File, file, close_write, _Cookie, Name} = _Info) ->
    Ext = string:to_lower(filename:extension(Name)),
    case lists:any(fun(X) -> X == Ext end,
                   [".erl", ".hrl",
                    ".c", ".cpp", ".cc",
                    ".h", ".hpp", ".hh" ] ) of
        true ->
            rebar_compile(File, Name);
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

do_reload({_File, file, close_write, _Cookie, Name} = _Info) ->
    M = list_to_atom(filename:rootname(Name)),
    code:purge(M),
    {module, M} = code:load_file(M);

do_reload({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

