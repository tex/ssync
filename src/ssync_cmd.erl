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

-module(ssync_cmd).
-export([cmd/3]).

cmd(Cmd, Args, Callback) ->
    Tag = make_ref(),
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
                    Rv = cmd_sync(Cmd, Args, Callback),
                    exit({Tag, Rv})
            end),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
        {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
    end.

cmd_sync(Cmd, Args, Callback) ->
    % Try to find the Cmd executable in the PATH
    E = case os:find_executable(Cmd) of
            false ->
                % Cmd not in the PATH, try current working directory
                {ok, CWD} = file:get_cwd(),
                os:find_executable(filename:join(CWD, Cmd));
            R -> R
        end,

    P = open_port({spawn_executable, E},
                          [binary, use_stdio, stream, {line, 1024}, eof, {args, Args}]),
    cmd_receive(P, Callback, <<>>).

cmd_receive(Port, Callback, Acc) ->
    receive
        {Port, {data, Data}} -> cmd_receive(Port, Callback, Callback(Data, Acc));
        {Port, eof}          -> Callback(eof, Acc), ok
    end.
