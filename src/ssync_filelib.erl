%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

-module(ssync_filelib).

-export([safe_relative_path/1, partition/2]).

%% @spec safe_relative_path(string()) -> string() | undefined
%% @doc Return the reduced version of a relative path or undefined if it
%% is not safe. safe relative paths can be joined with an absolute path
%% and will result in a subdirectory of the absolute path.
safe_relative_path("/" ++ _) ->
    undefined;

safe_relative_path(P) ->
    safe_relative_path(P, []).

safe_relative_path("", Acc) ->
    case Acc of
        [] ->
            "";
        _ ->
            string:join(lists:reverse(Acc), "/")
    end;

safe_relative_path(P, Acc) ->
    case partition(P, "/") of
        {"", "/", _} ->
            %% /foo or foo//bar
            undefined;
        {"..", _, _} when Acc =:= [] ->
            undefined;
        {"..", _, Rest} ->
            safe_relative_path(Rest, tl(Acc));
        {Part, "/", ""} ->
            safe_relative_path("", ["", Part | Acc]);
        {Part, _, Rest} ->
            safe_relative_path(Rest, [Part | Acc])
    end.

%% @spec partition(String, Sep) -> {String, [], []} | {Prefix, Sep, Postfix}
%% @doc Inspired by Python 2.5's str.partition:
%% partition("foo/bar", "/") = {"foo", "/", "bar"},
%% partition("foo", "/") = {"foo", "", ""}.
partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;

partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;

partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);

partition2(_S, _Sep) ->
    undefined.
