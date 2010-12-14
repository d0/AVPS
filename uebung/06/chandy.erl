-module(chandy).
-export([start/0,p1/2,p2/1]).

p1({Schrauben, Euro}, P2_PID) ->
    io:format("P1 meldet sich zum Dienst: P1_PID=~w P2_PID=~w~n",[self(),P2_PID]),
    io:format("P1 hat ~w Schrauben und ~w Euro~n", [Schrauben, Euro]),
    %Send our PID to P2
    P2_PID ! {pid, self()},
    %Start Chandy-Lamport algorithm
    chandy_lamport({Schrauben, Euro}, [P2_PID], [P2_PID], []).

p2({Schrauben, Euro}) ->
    io:format("P2 meldet sich zum Dienst: P2_PID=~w~n", [self()]),
    receive
        {pid, P1_PID} ->
            io:format("P2 empfaengt: P1_PID=~w~n", [P1_PID]),
            p2({Schrauben, Euro});
        %Chandy-Lamport: First we dump our state
        chandy ->
            io:format("State of ~w: ~w Schrauben und ~w Euro ~n",[self(), Schrauben, Euro]),
            p2({Schrauben, Euro})
    end.


%Send markers over all outgoing channels
send_markers([first | rest]) ->
    send_markers(first),
    send_markers(rest);
send_markers(channel) ->
    channel ! {self(), chandy}. 


%Only one element in incoming list, messages have been recorded
record_traffic(incoming, [messages]) ->
    receive
        %Normal message: Record it and keep on listening
        {incoming, {Schrauben, Euro}} ->
            record_traffic(incoming, [messages | {Schrauben, Euro}]);
        %Marker received. Stop recording and return messages received
        {incoming, chandy} ->
            [messages]
    end.

%Record the traffic from all processes included in incoming
record_traffic([]) ->
    [];
%Several elements in incoming list
record_traffic([first | rest]) ->
    record_traffic(first),
    record_traffic(rest);
%Only one element in incoming list, no messages recorded
record_traffic(incoming) ->
    receive
        %Normal message: Record it and keep on listening
        {pid, {Schrauben, Euro}} ->
            record_traffic(incoming, [{Schrauben, Euro}]);
        %Marker received. Stop recording and return empty list (no messages received)
        {pid, chandy} ->
            io:format("Marker von Prozess ~w empfangen~n", [pid]),
            []
    end.

%Perform Chandy-Lampert algorith:
%Parameters: Own state ({Schrauben, Euro}), list of all outgoing channels,
%   list of all incoming channels, channel on which the first marker was
%   received
chandy_lamport({Schrauben, Euro}, incoming, outgoing, first_marker) ->
    %Record state
    State = {self(), {Schrauben, Euro}},
    %Send marker to all processes
    send_markers(outgoing),
    %Record messages on all inbound channels
    %TODO: make sure first_marker is not part of outgoing list
    Recorded_messages = record_traffic(incoming),
    [State | Recorded_messages].

start() ->
    P2_PID = spawn(chandy, p2, [{2000,50}]),
    spawn(chandy, p1, [{0,900},P2_PID]).
