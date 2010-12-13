-module(chandy).
-export([start/0,p1/2,p2/1]).

p1({Schrauben, Euro}, P2_PID) ->
    io:format("P1 meldet sich zum Dienst: P1_PID=~w P2_PID=~w~n",[self(),P2_PID]),
    io:format("P1 hat ~w Schrauben und ~w Euro~n", [Schrauben, Euro]),
    P2_PID ! {pid, self()}.

p2({Schrauben, Euro}) ->
    io:format("P2 meldet sich zum Dienst: P2_PID=~w~n", [self()]),
    receive
        {pid, P1_PID} ->
            io:format("P2 empfaengt: P1_PID=~w~n", [P1_PID])
    end.

start() ->
    P2_PID = spawn(chandy, p2, [{2000,50}]),
    spawn(chandy, p1, [{0,900},P2_PID]).
