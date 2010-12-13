-module(chandy).
-export([start/0,p1/1,p2/0]).

p1(P2_PID) ->
    io:format("P1 meldet sich zum Dienst: P1_PID=~w P2_PID=~w~n",[self(),P2_PID]),
    P2_PID ! {pid, self()}.

p2() ->
    io:format("P2 meldet sich zum Dienst: P2_PID=~w~n", [self()]),
    receive
        {pid, P1_PID} ->
            io:format("P2 empfängt: P1_PID=~w~n", [P1_PID])
    end.

start() ->
    P2_PID = spawn(chandy, p2, []),
    spawn(chandy, p1, [P2_PID]).
