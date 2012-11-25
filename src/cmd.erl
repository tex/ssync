-module(cmd).

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
    P = open_port({spawn_executable, os:find_executable(Cmd)}, [
                binary, use_stdio, stream, eof, {args, Args}]),
    cmd_receive(P, Callback, []).

cmd_receive(Port, Callback, Acc) ->
    receive
        {Port, {data, Data}} -> cmd_receive(Port, Callback, Callback(Data, Acc));
        {Port, eof}          -> Callback(eof, Acc), ok
    end.
