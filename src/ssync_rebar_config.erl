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

-module(ssync_rebar_config).
-export([get_all_dirs/1]).

get_all_dirs(Root) ->
    case file:consult(filename:join(Root, "rebar.config")) of
        {ok, Terms} ->
            {ok, Dirs} = application:get_env(ssync, dirs),
            [{filename:join([Root, Path]), CallbackName} ||
                {Path, CallbackName} <- Dirs ] ++
            [get_all_dirs(Dir) || Dir <- get_deps(Root, Terms)] ++
            [get_all_dirs(Dir) || Dir <- get_sub_dirs(Root, Terms)];
        {error, _} ->
            {ok, Dirs} = application:get_env(ssync, dirs),
            [{filename:join([Root, Path]), CallbackName} ||
                {Path, CallbackName} <- Dirs ]
    end.

get_deps(Root, Terms) ->
    DepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    filelib:wildcard(filename:join([Root, DepsDir, "*"])).

get_sub_dirs(Root, Terms) ->
    RebarSubDir = proplists:get_value(sub_dirs, Terms, []),
    lists:flatmap(fun(Dir) ->
            filelib:wildcard(filename:join([Root, Dir]))
        end, RebarSubDir).
