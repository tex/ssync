-module(ssync_rebar_config).

-export([get_all_dirs/1]).

get_all_dirs(Root) ->
    case file:consult(filename:join(Root, "rebar.config")) of
        {ok, Terms} ->
            lists:foldl(
                fun ({Path, CallbackName}, Acc) ->
                        Acc ++ [{Path, CallbackName}];
                    (Path, Acc) ->
                        Acc ++ get_all_dirs(filename:join([Root, Path]))
                end, [], get_deps(Root, Terms) ++ get_sub_dirs(Root, Terms) );
        {error, _} ->
            get_sub_dirs(Root, [])
    end.

get_deps(Root, Terms) ->
    DepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    {ok, Dirs} = application:get_env(ssync, dirs),
    [{filename:join([Dir, Path]), CallbackName} ||
        Dir <- filelib:wildcard(filename:join([Root, DepsDir, "*"])),
        {Path, CallbackName} <- Dirs ].

get_sub_dirs(Root, Terms) ->
    SubDirs = proplists:get_value(sub_dirs, Terms, [""]),
    {ok, Dirs} = application:get_env(ssync, dirs),
    [{filename:join([Root, Dir, Path]), CallbackName} ||
        Dir <- SubDirs,
        {Path, CallbackName} <- Dirs ].
