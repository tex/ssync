%% Copyright (c) 2012 Milan Svoboda
%% Released under the MIT License.

-module(notify).
-export([notify/2]).

-define(TIME_PER_LINE, 3000).

notify(Title, Msg) ->
    Msg1 = re:replace(Msg, "'", "\\\\'", [global, {return, list}]),
    Msg2 = re:replace(Msg1, "\\n", "\\\\n", [global, {return, list}]),
    Timeout = case length(Msg) of 0 -> 100; Lines -> ?TIME_PER_LINE * Lines end,
    os:cmd(io_lib:format("notify-send -t ~p \"~s\" \"~s\"", [Timeout, Title, Msg2])).
