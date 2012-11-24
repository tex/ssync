%% Copyright (c) 2012 Milan Svoboda
%% Released under the MIT License.

-module(notify).
-export([notify/2]).

notify(Title, Msg) ->
    os:cmd(io_lib:format("notify-send '~s' '~s'", [Title, Msg])).

