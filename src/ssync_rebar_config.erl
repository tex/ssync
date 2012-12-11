-module(ssync_rebar_config).

-export([get_all_dirs/1]).

get_all_dirs(Root) ->
    case file:consult(filename:join(Root, "rebar.config")) of
        {ok, Terms} ->
            {ok, Dirs} = application:get_env(ssync, dirs),
            [{filename:join([Root, Path]), CallbackName} ||
                {Path, CallbackName} <- Dirs ] ++
            [get_all_dirs(Dir) || Dir <- get_deps(Root, Terms)] ++
            [get_all_dirs(filename:join([Root, Dir])) || Dir <- get_sub_dirs(Root, Terms)];
        {error, _} ->
            {ok, Dirs} = application:get_env(ssync, dirs),
            [{filename:join([Root, Path]), CallbackName} ||
                {Path, CallbackName} <- Dirs ]
    end.

get_deps(Root, Terms) ->
    DepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    filelib:wildcard(filename:join([Root, DepsDir, "*"])).

get_sub_dirs(_Root, Terms) ->
    proplists:get_value(sub_dirs, Terms, []).
