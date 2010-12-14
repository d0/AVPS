-module(chandy).
-export([start/0,active_proc/2,passive_proc/1]).

print_partners([]) ->
    io:format("~n",[]);
print_partners([Head | Tail]) ->
    io:format("~w is connected with ~w~n", [self(), Head]),
    print_partners(Tail).

send_pid(Partners) ->
    lists:foreach(fun(P) -> P ! {pids, lists:delete(P, Partners)} end, Partners).

%The process that starts Chandy-Lamport
active_proc({Schrauben, Euro}, Partners) ->
    io:format("Active process started. PID: ~w~n", [self()]),
    print_partners(Partners),
    %Send the PIDs to all processes
    send_pid([self() | Partners]), 
    %Start Chandy-Lamport algorithm
    chandy_lamport({Schrauben, Euro}, Partners, Partners, []).

%A process that doesn't start Chandy-Lamport
passive_proc({Schrauben, Euro}) ->
    io:format("P2 meldet sich zum Dienst: PID:~w~n", [self()]),
    receive
        {pids, Partners} ->
            lists:delete(self(), Partners),
            io:format("~w: Liste der Partner empfangen", [self()]),
            print_partners(Partners),
            passive_proc({Schrauben, Euro}, Partners)
    end.

passive_proc({Schrauben, Euro}, Partners) ->
    receive
        %Chandy-Lamport: First we dump our state
        chandy ->
            io:format("State of ~w: ~w Schrauben und ~w Euro ~n",[self(), Schrauben, Euro]),
            chandy_lamport({Schrauben, Euro}, Partners, Partners, [])
    end.

%Send markers over all outgoing channels
send_markers([First | Rest]) ->
    if
        First == self() ->
            send_markers(Rest);
        true -> %else
            First ! {self(), chandy}, 
            send_markers(Rest)
    end;
send_markers([]) ->
    nothing.

record_traffic(Incoming) ->
    record_traffic(Incoming, []).
record_traffic([], Messages) ->
    Messages;
record_traffic(Incoming, Messages) ->
    receive
        %Normal message: Record it and keep on listening
        {Pid, {Schrauben, Euro}} ->
            record_traffic(incoming, lists:append(Messages, [{Schrauben, Euro}]));
        %Marker received. Stop recording and return empty list (no messages received)
        {Pid, chandy} ->
            io:format("Marker von Prozess ~w empfangen~n", [Pid]),
            record_traffic(lists:delete(Pid, Incoming), Messages)
    end.

%Perform Chandy-Lampert algorith:
%Parameters: Own state ({Schrauben, Euro}), list of all outgoing channels,
%   list of all incoming channels, channel on which the first marker was
%   received
chandy_lamport({Schrauben, Euro}, Incoming, Outgoing, First_marker) ->
    %Record state
    State = {self(), {Schrauben, Euro}},
    %Send marker to all processes
    send_markers(Outgoing),
    %Record messages on all inbound channels
    %TODO: make sure first_marker is not part of outgoing list
    Recorded_messages = record_traffic(Incoming),
    [State | Recorded_messages].

start() ->
    P2_PID = spawn(chandy, passive_proc, [{2000, 50}]),
    P3_PID = spawn(chandy, passive_proc, [{1337, 23}]),
    spawn(chandy, active_proc, [{0,900},[P2_PID, P3_PID]]).
