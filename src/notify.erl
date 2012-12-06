%% Copyright (c) 2012 Milan Svoboda
%% Released under the MIT License.

-module(notify).
-export([notify/2]).

-define(TIME_PER_LINE, 3000).

notify(Title, Msgs) ->
    {ok, Outputs} = application:get_env(ssync, output),
    lists:foreach(
        fun(Output) ->
                notify(Output, Title, Msgs)
        end, Outputs ).

notify('notify-send', Title, Msgs) ->
    Msg1 = re:replace(Msgs, "'", "\\\\'", [global, {return, list}]),
    Msg2 = re:replace(Msg1, "\\n", "\\\\n", [global, {return, list}]),
    Timeout = case length(Msgs) of 0 -> 100; Lines -> ?TIME_PER_LINE * Lines end,
    os:cmd(io_lib:format("notify-send -t ~p \"~s\" \"~s\"", [Timeout, Title, Msg2]));

notify(console, Title, []) ->
    io:format("~s~n", [Title]);
notify(console, Title, Msgs) ->
    lists:foreach(fun(Msg) ->
            io:format("~s: ~s~n", [Title, Msg])
        end, Msgs).
