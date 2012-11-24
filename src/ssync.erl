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
         {"ebin", fun do_reload/1},
         {"deps/*/src", fun do_compile/1},
         {"deps/*/c_src", fun do_compile/1},
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

rebar_compile(_File, Name) ->
    Result = os:cmd("rebar compile"),
    notify:notify(io_lib:format("ssync compile ~s", [Name]), Result).

do_compile({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    notify:notify("ssync watch", FN),
    erlinotify:watch(FN, fun do_compile/1);

do_compile({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    notify:notify("ssync unwatch", FN),
    erlinotify:unwatch(FN);

do_compile({File, file, close_write, _Cookie, Name} = _Info) ->
    Ext = string:to_lower(filename:extension(Name)),
    case lists:any(fun(X) -> X == Ext end,
                   [".erl", ".c", ".cpp"] ) of
        true ->
            rebar_compile(File, Name);
        false ->
            ok
    end;

do_compile({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

do_reload({File, dir, create, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    notify:notify("ssync watch", FN),
    erlinotify:watch(FN, fun do_reload/1);

do_reload({File, dir, delete, _Cookie, Name} = _Info) ->
    FN = filename:join(File, Name),
    notify:notify("ssync unwatch", FN),
    erlinotify:unwatch(FN);

do_reload({_File, file, close_write, _Cookie, Name} = _Info) ->
    M = list_to_atom(filename:rootname(Name)),
    code:purge(M),
    {module, M} = code:load_file(M),
    notify:notify("ssync reload", io_lib:format("~p", [M]));

do_reload({_File, _Type, _Event, _Cookie, _Name} = _Info) ->
    ok.

