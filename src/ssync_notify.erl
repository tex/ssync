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

-module(ssync_notify).
-export([notify/2]).

-define(TIME_PER_LINE, 3000).

notify(Title, Msgs) ->
    {ok, Outputs} = application:get_env(ssync, output),
    lists:foreach(
        fun(Output) ->
                notify(Output, Title, Msgs)
        end, Outputs ).

notify('notify-send', Title, Msgs) ->
    Msg1 = lists:foldl(
            fun(Msg, Acc) ->
                    Acc ++ io_lib:format("~s\\\\n", [Msg])
            end, [], Msgs ),
    Msg2 = re:replace(Msg1, "\"", "\\\\\"", [global, {return, list}]),
    Timeout = case length(Msgs) of 0 -> 100; Lines -> ?TIME_PER_LINE * Lines end,
    os:cmd(io_lib:format("notify-send -t ~p \"~s\" \"~s\"", [Timeout, Title, Msg2]));

notify(console, Title, []) ->
    io:format("~s~n", [Title]);

notify(console, Title, Msgs) ->
    lists:foreach(
        fun(Msg) ->
            io:format("~s: ~s~n", [Title, Msg])
        end, Msgs ).
